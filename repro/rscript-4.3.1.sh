#!/bin/sh
# =============================================================================
# rscript-4.3.1.sh — run a script under the PINNED confirmatory environment.
#
#   ./repro/rscript-4.3.1.sh R/run.R --grid=smoke --R=40 --seed=20260709
#
# Why this exists. repro/versions.tsv pins R 4.3.1 + lme4 1.1-34 / Matrix 1.6-1,
# and that framework version is still installed alongside newer ones. But you
# cannot reach it with `Rscript`: the framework's bin/R wrapper hardcodes
# R_HOME_DIR to .../R.framework/Resources (the `Current` symlink, today 4.6.1)
# and explicitly *discards* an R_HOME from the environment — so
# `Versions/4.3-arm64/Resources/bin/Rscript` silently runs the newest R.
#
# The exec binary underneath the wrapper does honour R_HOME, so this invokes
# that directly and pins the library path to 4.3's own tree (otherwise the user
# library for the newer R leaks in and lme4 2.x wins).
#
# Use this for anything whose numbers ship: the confirmatory grid was produced
# on 4.3.1, and a grid built from two environments cannot claim one engine.
# The Dockerfile remains the airtight route where Docker is available.
# =============================================================================
set -eu

V=/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources

[ -x "$V/bin/exec/R" ] || {
  echo "rscript-4.3.1: R 4.3.1 is not installed at $V" >&2
  echo "               install it, or use the Dockerfile instead." >&2
  exit 1
}

[ $# -ge 1 ] || { echo "usage: $0 <script.R> [args...]" >&2; exit 2; }

FILE=$1
shift

# R_LIBS/R_LIBS_USER both pinned: R_LIBS_USER alone still leaves the newer
# version's site library on .libPaths().
R_HOME="$V" \
R_LIBS="$V/library" \
R_LIBS_USER="$V/library" \
exec "$V/bin/exec/R" --vanilla --no-echo --file="$FILE" --args "$@"
