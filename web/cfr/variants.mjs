// Calibration: how much do simple keep/swap rule changes matter vs Medium?
import { newGame, apply, mediumAction, easyAction, makeRng, undecided, popcount } from './engine.mjs';

const keepAll = st => { const me = st.p[st.toMove]; return 1 + ((1 << popcount(undecided(me))) - 1); };
const mediumNoSwap = st => { const a = mediumAction(st); return a === 0 ? 1 : a; };

function match(pA, pB, n, seed) {
  const rng = makeRng(seed);
  let score = 0;
  for (let g = 0; g < n; g++) {
    const flip = g % 2 === 1;
    const st = newGame(rng);
    const pol = flip ? [pB, pA] : [pA, pB];
    while (st.result === null) apply(st, pol[st.toMove](st));
    score += flip ? -st.result : st.result;
  }
  return (score / n).toFixed(4);
}
console.log('keepAll vs medium     ', match(keepAll, mediumAction, 100000, 5));
console.log('mediumNoSwap vs medium', match(mediumNoSwap, mediumAction, 100000, 6));
console.log('easy vs medium        ', match(easyAction, mediumAction, 100000, 7));
