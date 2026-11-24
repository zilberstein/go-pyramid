# Hard computer player (CFR)

The browser game's **Hard** level plays a strategy computed offline with
Monte Carlo **Discounted CFR** (Brown & Sandholm 2019) on an abstraction of Go Pyramid.

| File | Purpose |
| --- | --- |
| `engine.mjs` | Fast rank-only game engine, the infoset abstraction, and the Easy/Medium heuristics |
| `train.mjs` | DCFR trainer; writes `../ai-hard-policy.js` |
| `parity.mjs` | Checks the browser's `hardKey()` matches `infoKey()` on sampled states |
| `sanity.mjs` | Quick heuristic-vs-heuristic match |

```bash
node train.mjs 1500          # seconds of training; prints win rates vs Medium/Easy
```
```bash
node parity.mjs              # needs Playwright + Chromium
```

## Abstraction

* **Card isomorphism.** Suits never matter, so a card is just its rank; J, Q and K
  are all the same worthless card. A hand is reduced further to the *set* of
  useful ranks (not yet in the pyramid), since duplicates and face cards are
  always thrown away.
* **Actions.** Placing a placeable card is dominant, so it is always done. Each
  turn is then two decisions, each with its own infoset:
  1. **Swap or play.** Key: seat, own pyramid, which ranks I'd place now, how many
     other useful cards I hold, opponent progress bucket (0–5 / 6–8 / 9 placed),
     and middle knowledge: the original deal (unknown), the hand the opponent
     just swapped away (unknown), or the hand I left there (known: how many of its
     cards I could place now).
  2. **Which unplaceable cards to keep.** Key: seat, pyramid after placing, the set
     of useful ranks that can't be placed yet, opponent progress bucket. Many
     different hands share this key, so samples pool together.

## Algorithm

* Alternating updates: each traversal updates one seat; seats alternate.
* Chance (the deck order) is sampled once per traversal. The history is sampled
  with the current regret-matching strategies (the updating seat adds ε-uniform
  exploration). At one decision of the updating seat, every action is evaluated
  by playing the game out under the current strategies with common random
  numbers, and the sampled counterfactual regrets are accumulated.
* DCFR discounting with the standard α = 1.5, β = 0, γ = 2, applied lazily per
  infoset (exact, since an infoset's regrets only change sign when it is updated).
  One DCFR iteration = a batch of traversals for each seat.
* Several independent runs (different seeds) are merged by summing their
  average-strategy tables (`merge.mjs`), which reduces sampling noise.
* The shipped policy is the purified (argmax) average strategy for seat 1 (the
  computer moves second). Infosets with too few pooled updates, or whose best
  action equals the Medium heuristic, are left out and fall back to Medium.

## Current policy

4 runs × 20 minutes (94M traversals), merged, min 1,000 visits; 1,413 entries.
Measured with `eval.mjs` from the computer's seat (moving second), 200k games each,
score = win rate − loss rate (±0.004):

| Computer | vs Medium | vs Easy |
| --- | --- | --- |
| Medium | −0.047 | +0.189 |
| **Hard** | **+0.050** | +0.182 |

To retrain:

```bash
for i in 1 2 3 4; do DUMP=run$i.json node train.mjs 1200 /dev/null $i & done; wait
```
```bash
MIN_VISITS=1000 node merge.mjs ../ai-hard-policy.js run1.json run2.json run3.json run4.json
```
```bash
node eval.mjs && node parity.mjs
```
