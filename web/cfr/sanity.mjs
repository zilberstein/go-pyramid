import { newGame, apply, mediumAction, easyAction, makeRng } from './engine.mjs';

const rng = makeRng(12345);
function match(pA, pB, n) {
  let w = 0, l = 0, d = 0, rounds = 0;
  for (let g = 0; g < n; g++) {
    const swap = g % 2 === 1;
    const st = newGame(rng);
    const pol = swap ? [pB, pA] : [pA, pB];
    while (st.result === null) apply(st, pol[st.toMove](st));
    const r = swap ? -st.result : st.result;
    if (r > 0) w++; else if (r < 0) l++; else d++;
    rounds += st.round - 1;
  }
  return { win: w / n, loss: l / n, draw: d / n, avgRounds: rounds / n };
}
let t = Date.now();
console.log('medium vs medium', match(mediumAction, mediumAction, 20000), (Date.now() - t) + 'ms');
t = Date.now();
console.log('medium vs easy', match(mediumAction, easyAction, 20000), (Date.now() - t) + 'ms');
