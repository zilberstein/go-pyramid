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
* **Infoset** = seat, own pyramid (10-bit mask), useful hand ranks (10-bit mask),
  opponent progress bucket (0–5 / 6–8 / 9 cards placed), and middle knowledge
  (unknown, or how many of the cards I left there I could place right now).
* **Actions.** Placing a placeable card is dominant, so it is always done. The
  choices are: swap with the middle, or keep any subset of the useful cards that
  can't be placed yet (discarding the rest) — up to 1 + 2⁵ actions.

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
* The shipped policy is the purified (argmax) average strategy for seat 1 (the
  computer moves second). Infosets with too few updates, or whose best action
  equals the Medium heuristic, are left out and fall back to Medium.
