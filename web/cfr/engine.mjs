// Rank-only Go Pyramid engine used for CFR training and evaluation.
//
// Card isomorphism: suits never matter, so a card is just its rank 1..10,
// and J/Q/K are all the same worthless card (FACE = 11).
// The rules mirror web/index.html exactly: 5 face-down middle cards, 5-card
// hands, seat 0 moves then seat 1, both refill to 5, discard pile is reshuffled
// when the deck runs out, and the game ends after a round in which someone
// completed their pyramid (both = draw).

export const FACE = 11;
export const FULL = 1023;          // all 10 ranks placed
export const MAX_ROUNDS = 150;     // safety cap: counts as a draw

const SUP = [];
SUP[5] = [1, 2]; SUP[6] = [2, 3]; SUP[7] = [3, 4];
SUP[8] = [5, 6]; SUP[9] = [6, 7]; SUP[10] = [8, 9];

export const bit = r => 1 << (r - 1);

export function popcount(x) {
  let c = 0;
  while (x) { x &= x - 1; c++; }
  return c;
}

export function canPlace(r, m) {
  if (r > 10 || (m & bit(r))) return false;
  const s = SUP[r];
  return !s || ((m & bit(s[0])) !== 0 && (m & bit(s[1])) !== 0);
}

// Set of useful ranks in a hand (duplicates and face cards dropped).
export function rankSet(hand, m) {
  let s = 0;
  for (const r of hand) if (r <= 10) s |= bit(r);
  return s & ~m;
}

// Ranks from `set` that can be placed this turn (in ascending order, so chains
// like A, 2, then 5 work).
export function chain(set, m) {
  let placed = 0;
  for (let r = 1; r <= 10; r++) {
    if ((set & bit(r)) && canPlace(r, m | placed)) placed |= bit(r);
  }
  return placed;
}

// ------------------------------------------------------------------ abstraction
export function oppBucket(m) {
  const c = popcount(m);
  return c <= 5 ? 0 : c <= 8 ? 1 : 2;
}

// What this player knows about the middle:
//   0     = unknown, still the original five cards from the deal
//   7     = unknown, the hand the opponent just swapped away
//   1..6  = known (I left it there): 1 + how many of its cards I could place now
export function midFeature(me) {
  if (me.known) return 1 + popcount(chain(rankSet(me.known, me.mask), me.mask));
  return me.oppStash ? 7 : 0;
}

// Card counting. live[r] = copies of rank r this player has not seen: 4 minus
// copies in its hand, in either pyramid, in the discard pile (public), and in a
// middle it knows. Unseen copies are in the deck, the opponent's hand or an
// unknown middle. A reshuffle puts the discard pile back into the deck, so
// those copies become live again.
export function liveCounts(st, me, opp) {
  const live = new Int8Array(11).fill(4);
  for (const r of me.hand) if (r <= 10) live[r]--;
  for (let r = 1; r <= 10; r++) {
    if (me.mask & bit(r)) live[r]--;
    if (opp.mask & bit(r)) live[r]--;
  }
  for (const r of st.discard) if (r <= 10) live[r]--;
  if (me.known) for (const r of me.known) if (r <= 10) live[r]--;
  return live;
}

// Fewer than this many cards left in the deck = a reshuffle is near.
export const NEAR_SHUFFLE = 8;
const nearShuffle = st => (st.deck.length - st.dp <= NEAR_SHUFFLE ? 1 : 0);

// Each turn is two decisions, each with its own infoset.
//
// 1. Swap or play.  Key: seat, pyramid, which ranks I'd place now, how many other
//    useful cards I hold, middle knowledge, opponent progress, how many ranks I
//    still need that have no live copies (0/1/2+), and whether a reshuffle is near.
export const SWAP_DIV = 1024 * 1024 * 8 * 8 * 4 * 3 * 2;
export function swapKey(seat, me, opp, st) {
  const hs = rankSet(me.hand, me.mask);
  const placed = chain(hs, me.mask);
  const nRem = popcount(hs & ~placed);
  const live = liveCounts(st, me, opp);
  let dead = 0;
  for (let r = 1; r <= 10; r++) if (!(me.mask & bit(r)) && !(hs & bit(r)) && live[r] === 0) dead++;
  let k = me.mask;
  k = k * 1024 + placed;
  k = k * 8 + nRem;
  k = k * 8 + midFeature(me);
  k = k * 4 + oppBucket(opp.mask);
  k = k * 3 + Math.min(dead, 2);
  k = k * 2 + nearShuffle(st);
  return seat * SWAP_DIV + k;
}

// 2. Which cards that can't be placed yet to keep.  Key: seat, pyramid after
//    placing, the set of unplaceable useful ranks, opponent progress, which of the
//    ranks those cards are still waiting on have at most one live copy, and
//    whether a reshuffle is near.
//    (Many different hands lead to the same key, so samples pool together.)
export const KEEP_DIV = 1024 * 1024 * 4 * 1024 * 2;
export function keepKey(seat, me, opp, st) {
  const hs = rankSet(me.hand, me.mask);
  const placed = chain(hs, me.mask);
  const after = me.mask | placed;
  const rem = hs & ~placed;
  // every missing prerequisite of the cards I might keep (not placed, not in hand)
  let need = 0;
  const stack = [];
  for (let r = 1; r <= 10; r++) if (rem & bit(r)) stack.push(r);
  while (stack.length) {
    const s = SUP[stack.pop()];
    if (!s) continue;
    for (const q of s) {
      if ((after & bit(q)) || (need & bit(q))) continue;
      need |= bit(q);
      stack.push(q);
    }
  }
  need &= ~hs;
  const live = liveCounts(st, me, opp);
  let scarce = 0;
  for (let r = 1; r <= 10; r++) if ((need & bit(r)) && live[r] <= 1) scarce |= bit(r);
  let k = after;
  k = k * 1024 + rem;
  k = k * 4 + oppBucket(opp.mask);
  k = k * 1024 + scarce;
  k = k * 2 + nearShuffle(st);
  return seat * KEEP_DIV + k;
}

// Undecided ranks: useful cards that cannot be placed this turn.
export function undecided(me) {
  const hs = rankSet(me.hand, me.mask);
  return hs & ~chain(hs, me.mask);
}

// Combined turn action (what apply() takes):
// 0 = swap hand with the middle.
// 1 + k = place everything placeable, keep the undecided ranks selected by
// bitmask k (bit j = j-th undecided rank in ascending order), discard the rest.
export function numActions(me) {
  return 1 + numKeepActions(me);
}
export function numKeepActions(me) {
  return 1 << popcount(undecided(me));
}

export function keepBitsFor(rem, k) {
  let keep = 0, j = 0;
  for (let r = 1; r <= 10; r++) {
    if (rem & bit(r)) {
      if ((k >> j) & 1) keep |= bit(r);
      j++;
    }
  }
  return keep;
}

// ------------------------------------------------------------------ state
export function makeRng(seed) {
  let s = (seed >>> 0) || 0x9e3779b9;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17;
    s ^= s << 5; s >>>= 0;
    return s / 4294967296;
  };
}

function shuffle(a, rng) {
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(rng() * (i + 1));
    const t = a[i]; a[i] = a[j]; a[j] = t;
  }
  return a;
}

export function newGame(rng) {
  const deck = [];
  for (let s = 0; s < 4; s++) for (let r = 1; r <= 13; r++) deck.push(r > 10 ? FACE : r);
  shuffle(deck, rng);
  const middle = deck.slice(0, 5);
  const h0 = [], h1 = [];
  let dp = 5;
  for (let i = 0; i < 5; i++) { h0.push(deck[dp++]); h1.push(deck[dp++]); }
  return {
    deck, dp, discard: [], middle,
    p: [{ mask: 0, hand: h0, known: null, oppStash: false }, { mask: 0, hand: h1, known: null, oppStash: false }],
    toMove: 0, round: 1, result: null, rng,
  };
}

export function clone(st, rng) {
  return {
    deck: st.deck, dp: st.dp, discard: st.discard.slice(), middle: st.middle.slice(),
    p: st.p.map(q => ({ mask: q.mask, hand: q.hand.slice(), known: q.known, oppStash: q.oppStash })),
    toMove: st.toMove, round: st.round, result: st.result, rng: rng || st.rng,
  };
}

function refill(st, q) {
  while (q.hand.length < 5) {
    if (st.dp >= st.deck.length) {
      if (!st.discard.length) return;
      st.deck = shuffle(st.discard, st.rng);
      st.discard = [];
      st.dp = 0;
    }
    q.hand.push(st.deck[st.dp++]);
  }
}

export function apply(st, a) {
  const s = st.toMove, me = st.p[s], op = st.p[1 - s];
  if (a === 0) {
    const old = me.hand;
    me.hand = st.middle;
    st.middle = old;
    me.known = old;     // I know exactly what I left in the middle
    me.oppStash = false;
    op.known = null;    // the opponent only knows it's my old hand
    op.oppStash = true;
  } else {
    const hs = rankSet(me.hand, me.mask);
    const placed = chain(hs, me.mask);
    const keep = keepBitsFor(hs & ~placed, a - 1);
    me.mask |= placed;
    const nh = [];
    let usedP = 0, usedK = 0;
    for (const r of me.hand) {
      if (r <= 10 && (placed & bit(r)) && !(usedP & bit(r))) usedP |= bit(r);
      else if (r <= 10 && (keep & bit(r)) && !(usedK & bit(r))) { usedK |= bit(r); nh.push(r); }
      else st.discard.push(r);
    }
    me.hand = nh;
  }
  if (s === 0) { st.toMove = 1; return; }
  refill(st, st.p[0]);
  refill(st, st.p[1]);
  const c0 = st.p[0].mask === FULL, c1 = st.p[1].mask === FULL;
  if (c0 || c1) st.result = c0 && c1 ? 0 : c0 ? 1 : -1;
  else if (st.round >= MAX_ROUNDS) st.result = 0;
  st.toMove = 0;
  st.round++;
}

// ------------------------------------------------------------------ heuristics
function numNotIn(hand, m) {
  let n = 0;
  for (const r of hand) if (r <= 10 && !(m & bit(r))) n++;
  return n;
}

// "Medium": port of keepNextRow + middle swapping (AI/Naive.hs), identical to
// the medium player in web/index.html.
export function mediumSwap(st) {
  const me = st.p[st.toMove];
  const m = me.mask;
  const placed = chain(rankSet(me.hand, m), m);
  const known = me.known;
  if (known) {
    if (popcount(chain(rankSet(known, m), m)) > popcount(placed)) return true;
    if (placed === 0 && numNotIn(known, m) < numNotIn(me.hand, m)) return true;
    return false;
  }
  return placed === 0 && numNotIn(me.hand, m) > 2;
}

// Keep an unplaceable card if at least one of its supports is in place.
export function mediumKeep(st) {
  const me = st.p[st.toMove];
  const m = me.mask;
  const hs = rankSet(me.hand, m);
  const placed = chain(hs, m);
  const rem = hs & ~placed;
  const after = m | placed;
  let k = 0, j = 0;
  for (let r = 1; r <= 10; r++) {
    if (rem & bit(r)) {
      const s = SUP[r];
      if (s && ((after & bit(s[0])) || (after & bit(s[1])))) k |= 1 << j;
      j++;
    }
  }
  return k;
}

export function mediumAction(st) {
  return mediumSwap(st) ? 0 : 1 + mediumKeep(st);
}

// "Easy": place what fits, throw everything else away, never swap.
export function easyAction() { return 1; }

export function playOut(st, policies) {
  while (st.result === null) apply(st, policies[st.toMove](st));
  return st.result;
}
