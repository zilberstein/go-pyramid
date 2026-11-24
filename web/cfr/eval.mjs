// Evaluates the shipped policy file the way the browser plays it (computer = seat 1).
//   node eval.mjs [games=200000] [policy=../ai-hard-policy.js]
import { newGame, apply, mediumAction, easyAction, makeRng } from './engine.mjs';
import { loadPolicy, hardPolicy } from './policy.mjs';

const GAMES = Number(process.argv[2] || 200000);
const hard = hardPolicy(loadPolicy(process.argv[3] || new URL('../ai-hard-policy.js', import.meta.url).pathname));

// `bot` always sits in seat 1 (moves second), like the computer in the browser.
function asSeat1(bot, opp, games, seed) {
  const r = makeRng(seed);
  let w = 0, l = 0;
  for (let g = 0; g < games; g++) {
    const st = newGame(r);
    const pol = [opp, bot];
    while (st.result === null) apply(st, pol[st.toMove](st));
    if (st.result < 0) w++; else if (st.result > 0) l++;
  }
  const se = Math.sqrt((w + l) / games) / Math.sqrt(games);
  return `win ${(100 * w / games).toFixed(1)}%  loss ${(100 * l / games).toFixed(1)}%  draw ${(100 * (games - w - l) / games).toFixed(1)}%  ` +
    `(score ${((w - l) / games).toFixed(3)} ± ${(2 * se).toFixed(3)})`;
}

console.log(`computer in seat 1 (moves second), ${GAMES} games each:`);
console.log('  medium vs medium', asSeat1(mediumAction, mediumAction, GAMES, 11));
console.log('  hard   vs medium', asSeat1(hard, mediumAction, GAMES, 11));
console.log('  hard   vs easy  ', asSeat1(hard, easyAction, GAMES, 12));
console.log('  medium vs easy  ', asSeat1(mediumAction, easyAction, GAMES, 12));
console.log('  easy   vs medium', asSeat1(easyAction, mediumAction, GAMES, 13));
