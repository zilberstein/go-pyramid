// Merge raw tables from independent training runs into ai-hard-policy.js.
//   MIN_VISITS=200 node merge.mjs out.js run1.json run2.json ...
import fs from 'node:fs';
import { exportPolicy } from './policy.mjs';

const [out, ...files] = process.argv.slice(2);
const minVisits = Number(process.env.MIN_VISITS || 200);
const { text, deviations, trusted, total } = exportPolicy(files.map(f => JSON.parse(fs.readFileSync(f, 'utf8'))), minVisits);
fs.writeFileSync(out, text);
console.log(`merged ${files.length} runs: ${deviations} deviations from medium (${trusted} trusted of ${total} infosets, min visits ${minVisits}) -> ${out}`);
