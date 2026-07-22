# =============================================================================
# build-app.R — assemble the shinylive appdir.
#
#   Rscript tools/build-app.R          # from the repo root
#
# Why this exists rather than exporting the repo root directly:
# shinylive's read_app_files() embeds EVERY file under the appdir into app.json
# and does NOT read .gitignore. Exporting the root would publish SETUP.md (which
# names the private vault repo), results/ (1.3 MB of gitignored scratch),
# results-confirmatory/NOTES.md (unreviewed verdicts), repro/ and the Dockerfile.
#
# So the appdir is assembled from an EXPLICIT ALLOWLIST. "Ship exactly these
# files" is a security property; "export the directory and hope" is not.
#
# The engine keeps its single home in R/. app/R/ is a generated copy (gitignored)
# so it cannot drift into a stale second source of truth.
# =============================================================================

SIM  <- normalizePath(file.path(dirname(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(SIM) || !dir.exists(file.path(SIM, "R"))) SIM <- normalizePath(".")
APP  <- file.path(SIM, "app")
RC   <- file.path(SIM, "results-confirmatory")

# --- the allowlist: the ONLY engine files the app may ship --------------------
# Deliberately excluded: run.R, grid.R, analyze.R, validate.R, repro.R.
# They are not app dependencies, so shipping them would only widen what the
# bundle exposes for no benefit. repro.R also shells out to git, which is
# meaningless in wasm.
ENGINE <- c("seeds.R", "models.R", "dgm.R", "fit.R", "performance.R", "lookup.R")

say <- function(...) cat(sprintf(...), "\n")

# --- 1. engine ---------------------------------------------------------------
dir.create(file.path(APP, "R"), recursive = TRUE, showWarnings = FALSE)
unlink(list.files(file.path(APP, "R"), full.names = TRUE))
for (f in ENGINE) {
  src <- file.path(SIM, "R", f)
  if (!file.exists(src)) stop("build-app: missing engine file ", src)
  file.copy(src, file.path(APP, "R", f), overwrite = TRUE)
}
say("engine    : %d files -> app/R/ (%s)", length(ENGINE), paste(ENGINE, collapse = ", "))

# --- 2. grid -----------------------------------------------------------------
# Merge the confirmatory slabs into the single table lookup.R reads, and validate
# it here rather than in the browser: a bad grid must fail the build, not the user.
src <- file.path(RC, c("primary.csv", "secondary.csv"))
if (!all(file.exists(src))) stop("build-app: confirmatory results missing: ",
                                 paste(src[!file.exists(src)], collapse = ", "))
source(file.path(SIM, "R", "lookup.R"))          # load_grid() does the invariant checks
G <- load_grid(src)                              # errors loudly if the grid is malformed
grid <- G$cells
grid$.n_off <- NULL

dir.create(file.path(APP, "grid"), recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(APP, "grid", "grid.csv")
write.csv(grid, out_csv, row.names = FALSE)
if (!file.exists(out_csv) || file.size(out_csv) == 0)
  stop("build-app: grid.csv was not written — the export would ship a gridless app")
say("grid      : %d cells -> app/grid/grid.csv (%.0f KB)", nrow(grid), file.size(out_csv) / 1024)

# --- 3. provenance -----------------------------------------------------------
# This is a reproducibility tool: every number the app shows must trace back to a
# row of a specific grid built from a specific seed.
git_sha <- tryCatch(sub("\\s+$", "", system2("git", c("-C", SIM, "rev-parse", "--short", "HEAD"),
                                             stdout = TRUE, stderr = FALSE)),
                    error = function(e) NA_character_)
slabs <- aggregate(cbind(R_total) ~ grid, data = grid, FUN = function(x) x[1])
meta <- c(
  '{',
  sprintf('  "built": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf('  "git_sha": "%s",', git_sha),
  sprintf('  "master_seed": %s,', grid$master_seed[1]),
  sprintf('  "cells": %d,', nrow(grid)),
  '  "slabs": {',
  paste0(sprintf('    "%s": {"cells": %d, "R": %d}', slabs$grid,
                 as.integer(table(grid$grid)[slabs$grid]), as.integer(slabs$R_total)),
         collapse = ",\n"),
  '  },',
  '  "interpolation": {',
  '    "dims": ["N"],',
  '    "snapped": ["D", "lambda_bar", "cv"],',
  sprintf('    "loo_p90_N_power_all": %s,', LOO_P90_N),
  '    "note": "LOO over primary: N p90 2.16 pts; D 5.56; log-lambda 3.99. D and lambda are snapped because interpolating them reaches 14 and 12 points of error."',
  '  }',
  '}'
)
writeLines(meta, file.path(APP, "grid", "grid-meta.json"))
say("provenance: app/grid/grid-meta.json (seed %s, git %s)", grid$master_seed[1], git_sha)

# --- 4. what will actually ship ----------------------------------------------
if (!file.exists(file.path(APP, "app.R"))) stop("build-app: app/app.R is missing")
files <- list.files(APP, recursive = TRUE, all.files = FALSE)
say("\nappdir contents (this is EXACTLY what shinylive will publish):")
for (f in sort(files)) say("  %s", f)

leaks <- grep("SETUP|NOTES|Dockerfile|run-meta|\\.png$|manuscript|prereg", files, value = TRUE)
if (length(leaks)) stop("build-app: non-allowlisted file in the appdir: ", paste(leaks, collapse = ", "))

# The allowlist controls which FILES ship; it says nothing about what is written
# inside them. A comment quoting an internal doc leaks just as effectively as the
# doc itself — this check exists because exactly that happened during the build.
PRIVATE <- c("openlab-vault", "Not owner-reviewed", "owner may widen", "owner finalises",
             "Owner:", "owner-only", "Tier-0", "Strategy D", "Neubauer", "agents keep committing",
             "\\.env", "activity-log", "roles/")
hits <- unlist(lapply(file.path(APP, files), function(f) {
  txt <- tryCatch(paste(readLines(f, warn = FALSE), collapse = "\n"), error = function(e) "")
  found <- PRIVATE[vapply(PRIVATE, function(p) grepl(p, txt, perl = TRUE), logical(1))]
  if (length(found)) sprintf("%s: %s", basename(f), paste(found, collapse = ", "))
}))
if (length(hits)) stop("build-app: internal language inside a shipped file:\n  ",
                       paste(hits, collapse = "\n  "))
say("\nOK — %d files, no non-allowlisted files and no internal language in their contents.",
    length(files))
