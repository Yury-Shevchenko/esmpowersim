# esmpowersim — power and design planning for triggered experience sampling

**Hosted planner: [power.smaat.eu](https://power.smaat.eu/)** — runs in the browser, no install, no account.

Reproducible simulator and planning tool for the methods paper *"When does design matter for triggered
experience sampling? A simulation study of power, precision, and feasibility when the number of
observations is random."*

**Archives.** Software: [10.5281/zenodo.21487894](https://doi.org/10.5281/zenodo.21487894) · analysis
protocol (OSF): [osf.io/a5jdb](https://osf.io/a5jdb) · prospectively preregistered follow-up:
[osf.io/r8m2q](https://osf.io/r8m2q).

Every widely used ESM/EMA power tool assumes the number of measurement occasions per person is a
quantity the researcher **sets**. When assessments are **event-contingent** — triggered by a defined
experience the participant reports, a geofence crossing, a sensor state, or an external event — that
assumption breaks: each person's number of observations is a **random variable** driven by how often
their triggering context occurs, and that rate varies substantially between people.

This package implements the study's data-generating mechanism — a **trigger process** making
each person's observation count a random, between-person-varying, cap-truncated, compliance-thinned
count — and fits the same multilevel AR(1) model class as Lafit et al. (2021) `PowerAnalysisIL`, but
in the random-count case no existing tool handles.

## Models

The planner covers all **eleven multilevel models of Lafit et al. (2021)** — each a different research
question, so each powers a different effect. Pick one in the app, or deep-link it with `?model=9`.

| Model | Question | Effect powered |
|---|---|---|
| **3**, 4 | Does a momentary predictor predict the outcome, within person? (random / fixed slope) | within-person slope |
| 1, 2 | Does a group / person-level predictor shift the mean? | Level-2 main effect |
| 5–8 | Does a group or person-level variable **moderate** the within-person effect? | cross-level interaction |
| 9 | How strong is the **autoregressive** (carry-over) effect? | AR effect |
| 10, 11 | Does a group / person-level variable moderate the AR effect? | cross-level interaction |

**Model 3 is the paper's model**, and the one the study's own grid covers at R = 2,000. The other ten are
covered by tool-support slabs at R = 1,000, so **every model answers instantly** — from the table, not from
a simulation in your browser. All eleven inherit the trigger process, so the random-observation-count
problem applies to each of them, which is the part no existing tool handles.

Each model powers a **different coefficient**, so each slab was simulated at its own reference effect,
chosen by the criterion that set the paper's β₁ = 0.2: the grid has to span the 0.8 decision boundary, or
"how many participants do I need?" has no answer inside it. Models 1, 5 and 10 — the dummy-coded Level-2
models carrying a random slope — run at twice their twins' coefficient, because a balanced dummy has
SD(W) = 0.5 and so carries half the standardised effect of an N(0,1) continuous predictor. The value each
slab used travels with it in the CSV and is what the app's effect slider defaults to.

**One documented divergence from Lafit.** Their Models 1–8 carry temporal dependence as AR(1) *errors*
(which `lme4` cannot fit); their Models 9–11 use a lagged *outcome*. This tool uses a lagged outcome
throughout, as the paper does. So it matches Models 9–11 exactly, and for Models 1–8 it tests the identical
effect while carrying the AR as a lagged-outcome control rather than correlated errors. Set the AR
coefficient to 0 for the independent-errors case.

It also covers **fixed (time-contingent) schedules**, where the researcher plans exactly *k* prompts
per day: the count is set, not triggered, so between-person rate variation does not apply. Compliance
and fatigue still thin the planned prompts, so the realised count is *Binomial* — random around a
planned maximum. That missingness is the part `PowerAnalysisIL` does not model, and at full compliance
the engine reduces to the exactly-balanced design `PowerAnalysisIL` assumes (validated against it).
Choose the design type in the app, or deep-link it with `?design=fixed` (vs the default triggered case).

**All data are synthetic.** The simulator generates its own datasets; it never reads participant data.

> **Status: research code accompanying a manuscript that has not yet been peer reviewed.** The main
> simulation is reported as **exploratory**: of the four predictions its protocol set out, one held and
> three did not, and a separate **prospectively preregistered** follow-up did not confirm the headline
> between-person-rate-variance effect as statistically reliable. The paper reports this plainly and the
> planner's outputs reflect the same results. The default variance components in
> `default_pars()` are documented starting values, **not** authoritative estimates — set them from
> your own pilot data or from published ESM studies in your domain before relying on a power figure.

## Layout

| File | Role |
|---|---|
| `R/dgm.R` | Data-generating mechanism: trigger process + dynamic multilevel AR(1) outcome model |
| `R/fit.R` | Fits `y ~ x + y_lag + (1 + x \| id)` (lme4, REML); extracts β̂₁, Wald test, CI, convergence |
| `R/seeds.R` | Deterministic (master_seed, cell, rep) → seed; every dataset regenerable |
| `R/performance.R` | Runs R reps of a cell → power, Type-S/M, bias, RMSE, coverage, convergence, effective-n dist, all with Monte Carlo SE |
| `R/grid.R` | Study grids: primary (144 cells) + secondary S1–S5 + the prospectively-registered `subceiling` follow-up + a `smoke` grid. Also the **tool-support** grids (`models_grid`, `fixed_grid`), which are not part of the protocol |
| `R/run.R` | CLI driver → results CSV |
| `R/validate.R` | Pre-run checks (protocol §7): Lafit special case, recovery, DGM sanity |
| `R/analyze.R` | H1–H4 tests against the protocol's §5 criteria + power/convergence surfaces → `analysis-summary.md` |
| `R/repro.R` | Archives sessionInfo + git SHA + args next to every results file |
| `R/lookup.R` | Instant answers from the precomputed grid (same contract as `run_cell()`) |
| `app/app.R` | Companion planning tool (front end over the same engine) |
| `tools/build-app.R` | Assembles `app/` from an explicit allowlist, then validates it |
| `tools/run-tool-grids.sh` | Produces the tool-support slabs into `results-tool/`; resumable per slab |
| `tools/loo.R` | Leave-one-out validation of the interpolator, per slab → `LOO_P90_N_BY_SLAB` |
| `repro/rscript-4.3.1.sh` | Runs anything under the **pinned** R 4.3.1 / lme4 1.1-34 (see Reproducibility) |
| `renv.lock`, `DESCRIPTION`, `Dockerfile`, `repro/` | Reproducibility: pinned versions, env check, snapshot |

## Run

```sh
# fast end-to-end check (~15s)
Rscript R/run.R --grid=smoke --R=40 --seed=20260709

# pre-run validation (must pass before the main run)
Rscript R/validate.R --R=300 --seed=20260709

# main run (heavy — 144 cells x 2000 reps; --cores parallelises, fork-based)
# multicore output is bit-identical to serial at the same seed (per-rep deterministic seeding)
Rscript R/run.R --grid=primary   --R=2000 --seed=20260709 --cores=8 --out=results/primary.csv
Rscript R/run.R --grid=secondary --R=2000 --seed=20260709 --cores=8 --out=results/secondary.csv

# prospectively preregistered follow-up (run only after its OSF registration is posted)
Rscript R/run.R --grid=subceiling --R=2000 --seed=20260722 --out=results/subceiling.csv

# analysis: H1-H4 against the protocol's criteria + figures (§5)
Rscript R/analyze.R --primary=results/primary.csv \
    --secondary=results/secondary.csv --outdir=results/analysis
```

**Tool-support slabs** (the lookup table behind the planner — *not* study results, and not analysed by
`analyze.R`). These fill the models and schedules the study grid never simulated, so the planner answers
them from the table instead of running a simulation in the researcher's browser:

```sh
# all 21 slabs, ~26 h on 6 cores, resumable (a finished slab is skipped)
./tools/run-tool-grids.sh

# or one at a time
./repro/rscript-4.3.1.sh R/run.R --grid=model_9 --R=1000 --seed=20260709 --cores=6 \
    --out=results-tool/model_9.csv
./repro/rscript-4.3.1.sh R/run.R --grid=fixed_3 --R=1000 --seed=20260709 --cores=6 \
    --out=results-tool/fixed_3.csv

# re-measure the interpolator after any slab changes, then rebuild the app
./repro/rscript-4.3.1.sh tools/loo.R
./repro/rscript-4.3.1.sh tools/build-app.R
```

Run these under `repro/rscript-4.3.1.sh`, not `Rscript`: the table must come from one engine, and this
machine's default R is no longer the one that produced the study grid. `npm run test:repro` shows the
difference is real — on R 4.6.1 / lme4 2.0.1 the marginal fits at the sparse corner move
(convergence 0.570 → 0.560 on the reference cell).

Requires R with `lme4`, `MASS`, `Matrix`, `nlme`. Pin versions with `renv` before the main
run and archive `sessionInfo()` (§6). `brms`/`glmmTMB` cross-checks (protocol §3-M) are optional
and not required for the primary run.

## Status (2026-07-09)

- ✅ End-to-end verified on R 4.3.1 / lme4. Smoke grid + validation harness pass.
- ✅ DGM sanity: realised `mean_n` matches `rate × days × compliance`; `sd_n` roughly doubles from
  CV 0.3 → 0.9 (the headline between-person-variance lever) — e.g. 7.8 → 18.7 at rate 2, D 14.
- ✅ Recovery (large N & n): rel-bias < 1%, ~93% CI coverage.
- ✅ Sparse corner (mean_n ≈ 5): power ≈ 0.22 **and ~50% non-convergence** — RQ3 visible.
- **Calibration note:** the primary reference effect was set to **β₁ = 0.2** (not 0.3) after a smoke
  run showed 0.3 saturates power (>.95 at N ≥ 30), which would hide the CV effect. The effect-size
  factor still spans 0.1–0.5 (grid S4).

## Analysis layer (`R/analyze.R`) — verified

Reads the results CSV and applies the protocol's **fixed** §5 criteria (reported descriptively — the main simulation is exploratory, see Status above):
- **H1** — counts MCSE-sized monotonicity violations in N / rate / D.
- **H2 (headline)** — builds matched-expected-total (same N, rate, D) CV pairs; requires high-CV power
  lower in ≥80% of pairs, median gap ≤ −5 pts, **and** an adjusted high-CV penalty with 95% CI upper
  bound below −0.02.
- **H3** — ranks `P(n_i<10)` vs total prompts vs mean n by **LOOCV-R²** (exact, via hat values — no
  random folds, fully reproducible).
- **H4** — signed coefficients (with 95% CIs) for the drivers of non-convergence; compliance/cap signs
  pulled from the secondary grids when present.

Writes `analysis-summary.md` (verdict table) and four base-graphics PNGs: `power-surface.png`,
`convergence-surface.png`, `lower-tail-surface.png`, `h2-paired-diff.png`. Verified end-to-end on a
24-cell test grid: all four tests run, apply their criteria, and the surfaces render (the sparse-rate
panels already show the high-CV power penalty). *Verdicts on toy data are not meaningful — the real
run is 144 cells × 2000 reps.*

## Reproducibility

Single source of truth for versions is [`repro/versions.tsv`](repro/versions.tsv) (R 4.3.1 + the exact
engine closure: lme4 1.1-34, Matrix 1.6-1, MASS 7.3-60, nlme 3.1-162, and their deps). Three ways to
reproduce, strongest first:

```sh
docker build -t esm-power-sim .          # airtight: pins R 4.3.1 + exact package builds
Rscript -e "renv::restore()"             # from renv.lock (engine pinned; run in a fresh project lib)
Rscript repro/check-env.R                # verify the current machine matches; --install adds missing pkgs
./repro/rscript-4.3.1.sh repro/check-env.R   # ... or verify the pinned R directly, where it is installed
```

**If R 4.3.1 is installed alongside a newer R, you cannot reach it with `Rscript`.** The macOS framework's
`bin/R` wrapper hardcodes `R_HOME` to the `Current` symlink and explicitly discards any `R_HOME` you set,
so `Versions/4.3-arm64/Resources/bin/Rscript` silently runs the newest R instead — with the newest lme4.
[`repro/rscript-4.3.1.sh`](repro/rscript-4.3.1.sh) invokes the exec binary underneath the wrapper, which
does honour `R_HOME`, and pins the library path to 4.3's own tree. It is verified against the archived
grid: re-running primary cells through it reproduces `results-confirmatory/primary.csv` to 15 significant
digits. Use it for anything whose numbers ship.

Every real run writes a `*.run-meta.txt` next to its results CSV (timestamp, git SHA, dirty-state,
`sessionInfo()`), so any result carries a record of how it was produced. A reference snapshot is in
[`repro/sessionInfo.txt`](repro/sessionInfo.txt).

## Planning tool (`app/app.R`)

A researcher-facing front end over the **same engine** (so its numbers match the paper). Answers the
question no existing ESM power tool does: *when each person's observation count is random and triggered,
what is my power — and how starved is the lower tail?*

```sh
Rscript tools/build-app.R                                  # assemble app/ (engine + grid)
Rscript -e "shiny::runApp('app', launch.browser = TRUE)"   # needs shiny >= 1.8
```

Two engines behind one contract ([`R/lookup.R`](R/lookup.R)):

- **Lookup (default, instant).** Reads the precomputed grid (**4,584 cells**: the study's 504 at
  R = 2,000/1,000, plus 4,080 tool-support cells at R = 1,000 covering the other ten models and
  fixed schedules for all eleven). An exact hit carries MCSE ≤ ~1.6 points. This is not a cheap
  approximation of a live run — it is **~3× more precise *and* instant**, and the UI says so, because the
  instinct is the opposite. The badge over each answer names the R behind that particular row.
- **Simulate (opt-in, slow).** Runs the real engine in the browser at R = 100 (MCSE ~5 points, ~50 s for a
  typical design). Never auto-fires; the button carries a time estimate and refuses designs over ~10 min.

Interpolation is **measured, not assumed** (leave-one-out over every base slab, error on `power_all`):
N is interpolated; **D and rate are snapped to simulated levels**, because interpolating them reaches 14
and 12 points of error. CV has only two simulated levels, so it snaps too. Anything the grid cannot answer
— two levers off reference, an off-grid value, or N outside 20–150 — is **refused with a reason**, never
guessed.

The N-interpolation error band is **per slab**, not one shared number: the paper's grid interpolates best
at p90 2.2 points, while the moderation models reach 4.0, so quoting Model 3's precision on a cross-level
interaction would understate the uncertainty by about half. `tools/loo.R` regenerates the whole table and
prints it as the literal that belongs in `LOO_P90_N_BY_SLAB`; an unmeasured slab falls back to the *worst*
measured value, never the best.

**Fixed schedules got their own lever levels**, because a planned schedule saturates far earlier than a
triggered one — at N = 150 over 28 days, power reads 1.000 at every rate from 1 to 8 prompts/day. Their
grid therefore runs from 3-day bursts up to 8 prompts/day, where the decision boundary actually lives,
rather than reusing the triggered grid's 7–28 days at 1–4 triggers/day: rates 1–8 and durations
3, 7, 14, 21, 28.

Those levels are dense and regular **so the planner can offer sliders**. A Shiny slider steps along an
arithmetic sequence, so it can only land on simulated cells if the levels form one — and snapping to a
neighbour is not an option here, measured rather than assumed: across the fixed grid the 14 → 28 day gap
moves power by 15.6 points at p90 and the 1 → 2 prompts/day gap by 61. The app derives each slider's
stops from the shipped grid (`arith_tail`), so every stop is an exact lookup; where a partition's levels
cannot be stepped onto it falls back to radio buttons rather than guessing. The 3-day burst sits below a
step-7 duration slider and stays reachable by deep-link (`?D=3`) or a live run.

The headline is **power counting non-convergence as a failure to detect**, with the
convergence-conditional number a click away. They diverge by up to 42 points (at N=150, D=7, rate=1:
0.97 vs 0.54, at 56% convergence) and reporting the higher one would bless a design that fails 44% of the
time — the very finding the paper is built on.

- **Single-design mode:** power ± MCSE, convergence, mean obs/person, **P(< 10 obs)** (starved tail), a
  histogram of effective observations per person, a plain-language reading, and a "how was this computed?"
  panel naming the grid rows behind the number.
- **Power-curve mode:** power vs N with MCSE bars and the 0.80 line; in lookup mode the curve snaps to the
  grid's own N levels, so every point is an exact R = 2000 hit.
- Every design lever is exposed, and deep-linkable via query string
  (`?N=60&D=14&lambda=2&cv=0.3&trigger=0.75`) so a platform can prefill a study's design. Unknown params
  are ignored, values are clamped, and the app works perfectly with no params at all.
- **Round-trip with a planning platform.** Add `?return=<url>` (any `https://…smaat.eu` address) and the
  planner shows a one-click button that hands the chosen sample size back to that page as `?plannedN=<N>`,
  navigating the whole tab. The return URL is allow-listed to SMAAT hosts so the button can never become an
  open redirect; without the param the button never appears. This is the only SMAAT-specific behaviour in
  the tool — everything else is platform-agnostic.

## What is not here yet

- **An installable R package.** `DESCRIPTION` is in place, but the functions still need package layout
  (`NAMESPACE`, roxygen docs) — today they are scripts that `source()` each other.
- **The manuscript**, which is not yet submitted.

## Reproducibility

`Dockerfile` pins the exact environment the main run used (R 4.3.1, lme4 1.1-34, Matrix 1.6-1);
`repro/versions.tsv` records it. Same seed + same pinned environment → identical results.

**The web build deliberately uses a different environment, and that is tested rather than assumed.**
`shinylive` resolves each WebAssembly binary by matching the *local* package's major.minor, so a build
from the pinned R 4.3.1 silently ships empty `MASS`/`Matrix` and a bundle that fails at runtime — and
R 4.3.1 cannot host the matching versions (Matrix 1.7-5 and MASS 7.3-65 both need R ≥ 4.4). The web
build therefore runs on R ≥ 4.4 with packages floating to the wasm repository, and
[`tests/wasm-vs-native.mjs`](tests/wasm-vs-native.mjs) checks the shipped wasm engine against the
paper's recorded numbers (`tests/fixtures/paper-engine.json`) on every push:

```sh
npm ci && npm run test:repro
```

It currently reproduces the paper to six decimals on every metric across the lme4 1.1-34 → 2.0-1 gap.
If it ever fails, the "same engine as the study" claim is false and the wording — not
just the code — has to change.

## Integrity

Seeds are fixed constants (no wall-clock/runtime randomness in seeding); any per-cell reduction in R
is recorded in the `R_total` column, never silent; all cells (including non-converged) are reported.
