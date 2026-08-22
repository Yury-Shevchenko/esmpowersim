# =============================================================================
# run.R — Top-level driver. Runs a design grid and writes a results table.
#
# Usage:
#   Rscript R/run.R --grid=smoke   --R=200  --seed=20260709 --out=results/smoke.csv
#   Rscript R/run.R --grid=primary --R=2000 --seed=20260709 --out=results/primary.csv
#   Rscript R/run.R --grid=secondary --R=2000 --seed=20260709 --out=results/secondary.csv
#   Rscript R/run.R --grid=subceiling --R=2000 --seed=20260722 --out=results/subceiling.csv
#     (prospective follow-up study; run ONLY after its OSF registration is posted)
#   Rscript R/run.R --grid=model_9 --R=2000 --seed=20260709 --out=results/model_9.csv
#   Rscript R/run.R --grid=fixed_3 --R=1000 --seed=20260709 --out=results/fixed_3.csv
#     (tool-support slabs — one model each, so a multi-day run is resumable
#      per model rather than all-or-nothing; see grid.R)
#
# All randomness is seeded from --seed (prereg §3-M); results are regenerable.
# Every cell reports its Monte Carlo SE; any per-cell R reduction is recorded in
# the R_total column, never silent (prereg §4, §9).
# =============================================================================

suppressWarnings(suppressMessages({
  here <- tryCatch(dirname(sub("^--file=", "",
           grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), error = function(e) ".")
  if (is.na(here) || here == "") here <- "R"
  source(file.path(here, "seeds.R"))
  source(file.path(here, "models.R"))
  source(file.path(here, "dgm.R"))
  source(file.path(here, "fit.R"))
  source(file.path(here, "performance.R"))
  source(file.path(here, "grid.R"))
  source(file.path(here, "repro.R"))
}))

parse_args <- function() {
  a <- commandArgs(TRUE)
  get <- function(key, default) {
    hit <- grep(paste0("^--", key, "="), a, value = TRUE)
    if (length(hit) == 0) return(default)
    sub(paste0("^--", key, "="), "", hit[1])
  }
  list(grid  = get("grid", "smoke"),
       R     = as.integer(get("R", "200")),
       seed  = as.integer(get("seed", "20260709")),
       cores = as.integer(get("cores", "1")),
       rows  = get("rows", ""),
       count = any(grepl("^--count$", a)),
       out   = get("out", ""))
}

# Run every cell of a grid, serial or multicore. Because each replication seeds
# itself deterministically from (master_seed, cell_id=row index, rep), the result
# is INDEPENDENT of execution order — so multicore output is bit-identical to
# serial. Uses fork-based mclapply (macOS/Linux; falls back to serial on Windows).
#
# `cell_offset` shifts cell_id off the 1..n row index. The study grids never need
# it, but the tool-support slabs are run ONE MODEL PER INVOCATION so a multi-day
# run is resumable — and without an offset every slab would restart at cell_id 1
# and reuse the same seed stream, making the models' errors correlated rather
# than independent. The offset is derived from the slab name (not a CLI flag) so
# a caller cannot get it wrong or forget it.
run_grid <- function(grid, seed, R, cores, cell_offset = 0L, cell_ids = NULL) {
  n <- nrow(grid)
  if (is.null(cell_ids)) cell_ids <- seq_len(n)
  stopifnot(length(cell_ids) == n)
  one <- function(i) {
    r <- run_cell(grid[i, , drop = FALSE], seed, R, cell_id = cell_offset + cell_ids[i])
    message(sprintf("  cell %3d/%d  N=%3d D=%2d rate=%g cv=%g -> power=%.3f (MCSE %.3f) conv=%.2f  n=%.1f",
                    i, n, r$N, r$D, r$lambda_bar, r$cv,
                    r$power, r$mcse_power, r$conv_rate, r$mean_n))
    r
  }
  if (cores <= 1L) {
    rows <- lapply(seq_len(n), one)
  } else {
    cores <- min(cores, parallel::detectCores())
    message(sprintf("[esm-power-sim] running on %d cores (fork)", cores))
    rows <- parallel::mclapply(seq_len(n), one, mc.cores = cores,
                               mc.preschedule = FALSE)  # dynamic: cells vary in cost
    bad <- vapply(rows, function(x) inherits(x, "try-error") || is.null(x), logical(1))
    if (any(bad))
      stop(sprintf("%d cell(s) failed under mclapply; rerun those with --cores=1 to debug", sum(bad)))
  }
  do.call(rbind, rows)
}

# Resolve --grid to (cells, cell_id offset). Study grids keep offset 0 — their
# seed streams are already published and must not move. The per-model tool slabs
# get a disjoint block each, wide enough that no slab can ever run into the next.
resolve_grid <- function(name) {
  fixed_slab <- regmatches(name, regexec("^fixed_([0-9]+)$", name))[[1]]
  model_slab <- regmatches(name, regexec("^model_([0-9]+)$", name))[[1]]
  if (length(fixed_slab) == 2L) {
    id <- as.integer(fixed_slab[2])
    return(list(cells = fixed_grid(id), offset = 500000L + id * 10000L))
  }
  if (length(model_slab) == 2L) {
    id <- as.integer(model_slab[2])
    return(list(cells = models_grid(id), offset = id * 10000L))
  }
  cells <- switch(name,
                  primary    = primary_grid(),
                  secondary  = secondary_grids(),
                  smoke      = smoke_grid(),
                  subceiling = subceiling_grid(),
                  stop("unknown --grid: ", name))
  list(cells = cells, offset = 0L)
}

main <- function() {
  args <- parse_args()
  g    <- resolve_grid(args$grid)
  grid <- g$cells

  # --count prints how many cells the grid defines and exits. Lets a driver ask
  # "is this slab complete?" without hardcoding sizes that change when a grid is
  # extended (tools/run-tool-grids.sh uses it to decide whether to extend).
  if (isTRUE(args$count)) { cat(nrow(grid), "\n"); return(invisible(nrow(grid))) }

  # --rows=a:b runs a CONTIGUOUS SUBSET while keeping each cell's original row
  # index as its cell_id, so a subset run is bit-identical to the same rows of a
  # full run. That is what makes it safe to extend a grid: append the new levels
  # (see grid.R), run only the appended rows, and concatenate onto the existing
  # CSV — no re-running 26 h of cells that have not changed, and no seeds moving
  # underneath the cells that were already published.
  sel <- seq_len(nrow(grid))
  if (nzchar(args$rows)) {
    ab <- as.integer(strsplit(args$rows, ":", fixed = TRUE)[[1]])
    if (length(ab) != 2L || anyNA(ab) || ab[1] < 1L || ab[2] > nrow(grid) || ab[1] > ab[2])
      stop("--rows must be a:b within 1:", nrow(grid), " (got ", args$rows, ")")
    sel <- ab[1]:ab[2]
  }

  message(sprintf("[esm-power-sim] grid=%s  cells=%d%s  R=%d  seed=%d  cores=%d%s",
                  args$grid, length(sel),
                  if (length(sel) != nrow(grid)) sprintf(" of %d (rows %d-%d)",
                      nrow(grid), min(sel), max(sel)) else "",
                  args$R, args$seed, args$cores,
                  if (g$offset) sprintf("  cell_id offset=%d", g$offset) else ""))
  t0 <- proc.time()[["elapsed"]]

  res <- run_grid(grid[sel, , drop = FALSE], args$seed, args$R, args$cores,
                  cell_offset = g$offset, cell_ids = sel)
  res$master_seed <- args$seed
  res$R_requested <- args$R

  dt <- proc.time()[["elapsed"]] - t0
  message(sprintf("[esm-power-sim] done in %.1fs", dt))

  if (nzchar(args$out)) {
    dir.create(dirname(args$out), showWarnings = FALSE, recursive = TRUE)
    write.csv(res, args$out, row.names = FALSE)
    message("[esm-power-sim] wrote ", args$out)
    write_run_meta(args$out, args, tag = paste0("run:", args$grid))
  } else {
    print(res[, c("grid","N","D","lambda_bar","cv","power","mcse_power",
                   "conv_rate","mean_n","below_k")])
  }
  invisible(res)
}

if (sys.nframe() == 0L) main()
