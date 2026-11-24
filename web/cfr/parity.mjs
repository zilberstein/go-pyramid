// End-to-end check that the browser's Hard player (web/index.html, hardDecide)
// makes exactly the same move as policy.mjs on the same exported policy, on
// thousands of sampled game states.   Usage: node parity.mjs  (needs Playwright)
import { createRequire } from 'node:module';
import { newGame, apply, mediumAction, numActions, undecided, makeRng } from './engine.mjs';
import { loadPolicy, hardPolicy } from './policy.mjs';

const require = createRequire(import.meta.url);
const { chromium } = require('playwright');
const SUITS = ['♠', '♥', '♣', '♦'];
const POLICY = new URL('../ai-hard-policy.js', import.meta.url).pathname;
const hard = hardPolicy(loadPolicy(POLICY));

const toCards = h => h.map((r, i) => ({ rank: r === 11 ? 11 + (i % 3) : r, suit: SUITS[(i * 7 + r) % 4] }));
const pyr = m => { const o = {}; for (let r = 1; r <= 10; r++) if (m & (1 << (r - 1))) o[r] = { rank: r, suit: '♠' }; return o; };

const rng = makeRng(42);
const cases = [];
while (cases.length < 20000) {
  const st = newGame(rng);
  while (st.result === null) {
    if (st.toMove === 1 && rng() < 0.4) {
      const me = st.p[1];
      const sorted = me.hand.slice().sort((a, b) => a - b);   // the browser keeps hands sorted
      cases.push({
        expect: hard(st),
        rem: undecided(me),
        players: [{ hand: toCards(st.p[0].hand), pyr: pyr(st.p[0].mask) }, { hand: toCards(sorted), pyr: pyr(me.mask) }],
        known: me.known ? toCards(me.known.slice().sort((a, b) => a - b)) : null,
      });
    }
    const n = numActions(st.p[st.toMove]);
    apply(st, rng() < 0.2 ? Math.floor(rng() * n) : mediumAction(st));
  }
}

const b = await chromium.launch();
const pg = await b.newPage();
await pg.goto('file://' + new URL('../index.html', import.meta.url).pathname);
const res = await pg.evaluate(cs => {
  let bad = 0, deviations = 0;
  const examples = [];
  for (const c of cs) {
    G = { players: c.players, ai: { knownMiddle: c.known } };
    const d = hardDecide();
    let got;
    if (d === 'swap') got = 0;
    else {
      let kept = 0;
      c.players[1].hand.forEach((card, i) => { if (d[i] === 'keep') kept |= 1 << (card.rank - 1); });
      let k = 0, j = 0;
      for (let r = 1; r <= 10; r++) if (c.rem & (1 << (r - 1))) { if (kept & (1 << (r - 1))) k |= 1 << j; j++; }
      got = 1 + k;
    }
    if (got !== c.expect) { bad++; if (examples.length < 3) examples.push({ got, expect: c.expect }); }
    G = { players: c.players, ai: { knownMiddle: c.known } };
    const md = mediumDecide();
    if ((md === 'swap') !== (got === 0) || (md !== 'swap' && got !== 0 && JSON.stringify(md) !== JSON.stringify(d))) deviations++;
  }
  return { bad, deviations, examples, policySize: HARD_SWAP.size + HARD_KEEP.size };
}, cases);
await b.close();
console.log(`${cases.length} states: ${res.bad} mismatches between browser and engine; ` +
  `hard differs from medium in ${res.deviations}; policy entries ${res.policySize}`, res.examples);
process.exit(res.bad ? 1 : 0);
