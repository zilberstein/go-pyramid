// Monte Carlo CFR (Discounted CFR, alternating updates) for Go Pyramid.
//
//   node train.mjs [seconds=600] [out=../ai-hard-policy.js] [seed=1]
//
// Abstraction (see engine.mjs): suits are ignored and J/Q/K are one worthless
// rank. Placing a placeable card is dominant, so each turn is two decisions:
//   swap-or-play  (infoset: swapKey)   and   which unplaceable cards to keep
//   (infoset: keepKey).
//
// Each traversal updates one seat; seats alternate. Chance (the deck order) is
// sampled once; the history is sampled with the current strategies (the
// updating seat mixes in epsilon-uniform exploration). At one decision of the
// updating seat every action is evaluated by playing the game out under the
// current strategies with common random numbers, and the sampled
// counterfactual regrets are accumulated.
//
// Discounting is standard DCFR (alpha = 1.5, beta = 0, gamma = 2), applied lazily
// per infoset, which is exact because an infoset's regrets only change when it
// is updated. One DCFR iteration = a batch of BATCH traversals for each seat.

import fs from 'node:fs';
import { exportPolicy, SWAP_DIV, KEEP_DIV } from './policy.mjs';
import {
  newGame, clone, apply, swapKey, keepKey, numKeepActions,
  mediumSwap, mediumKeep, mediumAction, easyAction, makeRng,
} from './engine.mjs';

const SECONDS = Number(process.argv[2] || 600);
const OUT = process.argv[3] || new URL('../ai-hard-policy.js', import.meta.url).pathname;
const SEED = Number(process.argv[4] || 1);

const ALPHA = 1.5, BETA = 0, GAMMA = 2;
const BATCH = 2000;          // traversals per seat per DCFR iteration
const EPS = 0.1;             // exploration for the updating seat's own history
const MAX_BRANCH_DEPTH = 20; // own decision points (about two per round)
const MIN_VISITS = Number(process.env.MIN_VISITS || 50); // trust an infoset only after this many updates
const EVAL_EVERY = Number(process.env.EVAL_EVERY || 60); // seconds

const rng = makeRng(SEED);
const SW = new Map();        // swap-or-play infosets: action 0 = swap, 1 = play
const KP = new Map();        // keep infosets: action k = keep-subset bitmask

// log of prod_{k=1}^{t-1} k^a/(k^a+1), for lazy positive-regret discounting
const logPos = [0, 0];
function LP(t) {
  while (logPos.length <= t) {
    const k = logPos.length - 1;
    const ka = Math.pow(k, ALPHA);
    logPos.push(logPos[k] + Math.log(ka / (ka + 1)));
  }
  return logPos[t];
}

function discount(e, t) {
  if (e.t >= t) return;
  const fp = Math.exp(LP(t) - LP(e.t));
  // beta = 0: negative regrets are halved every iteration
  const fn = Math.pow(Math.pow(e.t, BETA) / (Math.pow(e.t, BETA) + 1), t - e.t);
  const fs = Math.pow(e.t / t, GAMMA);
  for (let a = 0; a < e.R.length; a++) {
    e.R[a] *= e.R[a] > 0 ? fp : fn;
    e.S[a] *= fs;
  }
  e.t = t;
}

// Regret matching. With no positive regret: best-regret action (ties -> default).
function strategy(e, out) {
  let sum = 0;
  for (let a = 0; a < e.R.length; a++) sum += e.R[a] > 0 ? e.R[a] : 0;
  if (sum > 0) {
    for (let a = 0; a < e.R.length; a++) out[a] = e.R[a] > 0 ? e.R[a] / sum : 0;
  } else {
    let best = e.def;
    for (let a = 0; a < e.R.length; a++) if (e.R[a] > e.R[best]) best = a;
    out.fill(0, 0, e.R.length);
    out[best] = 1;
  }
}

function bestAverage(e) {
  let best = e.def;
  for (let a = 0; a < e.S.length; a++) if (e.S[a] > e.S[best]) best = a;
  return best;
}

const tmp = new Float64Array(64);
function sample(p, n, r) {
  let x = r();
  for (let a = 0; a < n - 1; a++) { x -= p[a]; if (x < 0) return a; }
  return n - 1;
}

// ---------------------------------------------------------------- policies
function curSwap(st) {
  const s = st.toMove;
  const e = SW.get(swapKey(s, st.p[s], st.p[1 - s]));
  if (!e) return mediumSwap(st);
  strategy(e, tmp);
  return sample(tmp, 2, st.rng) === 0;
}
function curKeep(st) {
  const s = st.toMove;
  if (numKeepActions(st.p[s]) === 1) return 0;
  const e = KP.get(keepKey(s, st.p[s], st.p[1 - s]));
  if (!e) return mediumKeep(st);
  strategy(e, tmp);
  return sample(tmp, e.R.length, st.rng);
}
// Current strategies (rollouts and opponent play during training).
function currentPolicy(st) {
  return curSwap(st) ? 0 : 1 + curKeep(st);
}

// Purified average strategy: what ships in the browser.
function averagePolicy(st) {
  const s = st.toMove, me = st.p[s], op = st.p[1 - s];
  const es = SW.get(swapKey(s, me, op));
  const swap = es && es.N >= MIN_VISITS ? bestAverage(es) === 0 : mediumSwap(st);
  if (swap) return 0;
  if (numKeepActions(me) === 1) return 1;
  const ek = KP.get(keepKey(s, me, op));
  return 1 + (ek && ek.N >= MIN_VISITS ? bestAverage(ek) : mediumKeep(st));
}

// ---------------------------------------------------------------- traversal
const vals = new Float64Array(64);
const sig = new Float64Array(64);

function rollout(st) {
  while (st.result === null) apply(st, currentPolicy(st));
  return st.result;
}

function getEntry(map, key, n, def, t) {
  let e = map.get(key);
  if (!e) {
    e = { R: new Float64Array(n), S: new Float64Array(n), t, def, N: 0 };
    map.set(key, e);
  }
  discount(e, t);
  return e;
}

function update(e, n) {
  strategy(e, sig);
  let v = 0;
  for (let a = 0; a < n; a++) v += sig[a] * vals[a];
  for (let a = 0; a < n; a++) {
    e.R[a] += vals[a] - v;
    e.S[a] += sig[a];
  }
  e.N++;
}

function branchSwap(st, seat, t) {
  const e = getEntry(SW, swapKey(seat, st.p[seat], st.p[1 - seat]), 2, mediumSwap(st) ? 0 : 1, t);
  const crn = (rng() * 4294967296) >>> 0;
  for (let a = 0; a < 2; a++) {
    const c = clone(st, makeRng(crn));
    apply(c, a === 0 ? 0 : 1 + curKeep(c));
    const res = rollout(c);
    vals[a] = seat === 0 ? res : -res;
  }
  update(e, 2);
}

function branchKeep(st, seat, t) {
  const n = numKeepActions(st.p[seat]);
  const e = getEntry(KP, keepKey(seat, st.p[seat], st.p[1 - seat]), n, mediumKeep(st), t);
  const crn = (rng() * 4294967296) >>> 0;
  for (let a = 0; a < n; a++) {
    const c = clone(st, makeRng(crn));
    apply(c, 1 + a);
    const res = rollout(c);
    vals[a] = seat === 0 ? res : -res;
  }
  update(e, n);
}

function traverse(seat, t) {
  const st = newGame(rng);
  const target = Math.floor(rng() * MAX_BRANCH_DEPTH);
  let own = 0;
  while (st.result === null) {
    if (st.toMove !== seat) { apply(st, currentPolicy(st)); continue; }
    // decision 1: swap or play
    if (own++ === target) { branchSwap(st, seat, t); return true; }
    const swap = rng() < EPS ? rng() < 0.5 : curSwap(st);
    if (swap) { apply(st, 0); continue; }
    // decision 2: what to keep
    const n = numKeepActions(st.p[seat]);
    if (n === 1) { apply(st, 1); continue; }
    if (own++ === target) { branchKeep(st, seat, t); return true; }
    apply(st, 1 + (rng() < EPS ? Math.floor(rng() * n) : curKeep(st)));
  }
  return false;
}

// ---------------------------------------------------------------- evaluation
function evaluate(pA, pB, games, seed) {
  const r = makeRng(seed);
  let score = 0, w = 0, l = 0;
  for (let g = 0; g < games; g++) {
    const flip = g % 2 === 1;
    const st = newGame(r);
    const pol = flip ? [pB, pA] : [pA, pB];
    while (st.result === null) apply(st, pol[st.toMove](st));
    const res = flip ? -st.result : st.result;
    score += res;
    if (res > 0) w++; else if (res < 0) l++;
  }
  return { win: +(w / games).toFixed(4), loss: +(l / games).toFixed(4), score: +(score / games).toFixed(4) };
}

// ---------------------------------------------------------------- main loop
const start = Date.now();
let t = 1, traversals = 0, lastReport = start;
while (Date.now() - start < SECONDS * 1000) {
  for (const seat of [0, 1]) {            // alternation
    for (let b = 0; b < BATCH; b++) if (traverse(seat, t)) traversals++;
  }
  t++;
  if (Date.now() - lastReport > EVAL_EVERY * 1000) {
    lastReport = Date.now();
    const ev = evaluate(averagePolicy, mediumAction, 20000, 777);
    console.log(`[${Math.round((Date.now() - start) / 1000)}s] iter ${t} traversals ${traversals} infosets swap ${SW.size} keep ${KP.size} vs medium`, ev);
  }
}

console.log('final hard vs medium', evaluate(averagePolicy, mediumAction, 200000, 999));
console.log('final hard vs easy  ', evaluate(averagePolicy, easyAction, 200000, 998));
console.log('medium vs easy      ', evaluate(mediumAction, easyAction, 200000, 998));

// Export the purified average strategy for seat 1 (the computer moves second in
// the browser). With DUMP=path, also write the raw seat-1 average-strategy
// tables so several independent runs can be merged (merge.mjs).
const seat1 = (map, div) => [...map].filter(([key]) => Math.floor(key / div) === 1)
  .map(([key, e]) => [key, e.N, e.def, Array.from(e.S, x => +x.toPrecision(6))]);
const dump = { seconds: SECONDS, iterations: t, traversals, sw: seat1(SW, SWAP_DIV), kp: seat1(KP, KEEP_DIV) };
if (process.env.DUMP) fs.writeFileSync(process.env.DUMP, JSON.stringify(dump));
const { text, deviations, trusted, total } = exportPolicy([dump], MIN_VISITS);
fs.writeFileSync(OUT, text);
console.log(`exported ${deviations} deviations from medium (${trusted} trusted of ${total} seat-1 infosets) to ${OUT}`);
