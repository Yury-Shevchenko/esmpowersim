#!/bin/sh
# =============================================================================
# run-tool-grids.sh — produce the tool-support lookup slabs.
#
#   ./tools/run-tool-grids.sh              # run everything still missing
#   ./tools/run-tool-grids.sh model_9      # run just these slabs
#
# Fills the gap that forces the planner to simulate in the researcher's browser:
# the precomputed grid covers Model 3 on a triggered schedule and nothing else,
# so the other ten models and EVERY fixed-schedule design fall through to a live
# R = 100 run (~50 s, MCSE ~5 points). These slabs answer them from the table at
# R = 1000 (MCSE ~1.6 points) instead.
#
# NOT study results. See the header of grid.R: these are unregistered, are not
# read by analyze.R, and land in results-tool/ rather than results-confirmatory/.
#
# Runs under the PINNED environment (R 4.3.1 / lme4 1.1-34), which is verified to
# reproduce the archived grid to 15 significant digits — a lookup table built
# from two engines could not claim to be one.
#
# Resumable by design: one invocation per slab, each writing its own CSV, and a
# slab whose CSV already exists is skipped. A crash at hour 30 costs one slab,
# not the run. Seed streams are kept disjoint per slab inside run.R
# (resolve_grid()), so re-running one slab never perturbs another.
# =============================================================================
set -eu

cd "$(dirname "$0")/.."

R_REPS=1000
SEED=20260709
# 6 of 8 cores: the box also runs the agent fleet on a 15-minute cron, and this
# occupies it for well over a day. The confirmatory secondary run used 6 too.
CORES=6
OUT=results-tool

ALL="model_1 model_2 model_4 model_5 model_6 model_7 model_8 model_9 model_10 model_11 \
fixed_1 fixed_2 fixed_3 fixed_4 fixed_5 fixed_6 fixed_7 fixed_8 fixed_9 fixed_10 fixed_11"

SLABS=${*:-$ALL}

mkdir -p "$OUT"
LOG="$OUT/run.log"

log() { printf '%s %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" | tee -a "$LOG"; }

log "=== run-tool-grids start (R=$R_REPS seed=$SEED cores=$CORES) ==="

# A slab is not simply present-or-absent: a grid can gain levels (see the frozen
# row order in grid.R), in which case the existing CSV is a correct PREFIX of the
# new grid and only the appended rows need running. Comparing counts turns that
# into an ordinary resume, so extending a grid never means re-running cells that
# have not changed.
for slab in $SLABS; do
  csv="$OUT/$slab.csv"
  want=$(./repro/rscript-4.3.1.sh R/run.R "--grid=$slab" --count 2>/dev/null | tr -d '[:space:]')
  have=0
  [ -f "$csv" ] && have=$(( $(wc -l < "$csv") - 1 ))

  if [ "$have" -eq "$want" ]; then
    log "skip  $slab (complete: $have/$want cells)"
    continue
  fi
  if [ "$have" -gt "$want" ]; then
    log "FAIL  $slab has $have cells but the grid defines $want — refusing to guess; investigate"
    continue
  fi

  first=$(( have + 1 ))
  if [ "$have" -gt 0 ]; then
    log "start $slab (extending: rows $first-$want, keeping the $have already run)"
  else
    log "start $slab (rows 1-$want)"
  fi
  t0=$(date +%s)
  # Write to a .part file and move on success, so an interrupted slab never
  # leaves a truncated CSV that the next run would happily skip.
  if nice -n 5 ./repro/rscript-4.3.1.sh R/run.R \
        "--grid=$slab" "--R=$R_REPS" "--seed=$SEED" "--cores=$CORES" \
        "--rows=$first:$want" \
        "--out=$OUT/$slab.part.csv" >> "$LOG" 2>&1; then
    if [ "$have" -gt 0 ]; then
      # Append without the header. The grid emits new levels AFTER the original
      # block, so concatenation reproduces the full grid's row order exactly.
      tail -n +2 "$OUT/$slab.part.csv" >> "$csv"
      rm -f "$OUT/$slab.part.csv"
    else
      mv "$OUT/$slab.part.csv" "$csv"
    fi
    # write_run_meta() strips the extension before appending, so the metadata for
    # <slab>.part.csv lands at <slab>.part.run-meta.txt — NOT <slab>.part.csv.run-meta.txt.
    [ -f "$OUT/$slab.part.run-meta.txt" ] &&
      mv "$OUT/$slab.part.run-meta.txt" "$OUT/$slab.run-meta.txt"
    log "done  $slab ($(( ($(date +%s) - t0) / 60 )) min)"
  else
    log "FAIL  $slab — see $LOG; continuing with the rest"
    rm -f "$OUT/$slab.part.csv"
  fi
done

log "=== run-tool-grids finished ==="
