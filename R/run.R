# =============================================================================
# run.R — Top-level driver. Runs a design grid and writes a results table.
#
# Usage:
#   Rscript R/run.R --grid=smoke   --R=200  --seed=20260709 --out=results/smoke.csv
#   Rscript R/run.R --grid=primary --R=2000 --seed=20260709 --out=results/primary.csv
#   Rscript R/run.R --grid=secondary --R=2000 --seed=20260709 --out=results/secondary.csv
#   Rscript R/run.R --grid=subceiling --R=2000 --seed=20260722 --out=results/subceiling.csv
#     (prospective follow-up study; run ONLY after its OSF registration is posted)
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
       out   = get("out", ""))
}

# Run every cell of a grid, serial or multicore. Because each replication seeds
# itself deterministically from (master_seed, cell_id=row index, rep), the result
# is INDEPENDENT of execution order — so multicore output is bit-identical to
# serial. Uses fork-based mclapply (macOS/Linux; falls back to serial on Windows).
run_grid <- function(grid, seed, R, cores) {
  n <- nrow(grid)
  one <- function(i) {
    r <- run_cell(grid[i, , drop = FALSE], seed, R, cell_id = i)
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

main <- function() {
  args <- parse_args()
  grid <- switch(args$grid,
                 primary    = primary_grid(),
                 secondary  = secondary_grids(),
                 smoke      = smoke_grid(),
                 subceiling = subceiling_grid(),
                 stop("unknown --grid: ", args$grid))

  message(sprintf("[esm-power-sim] grid=%s  cells=%d  R=%d  seed=%d  cores=%d",
                  args$grid, nrow(grid), args$R, args$seed, args$cores))
  t0 <- proc.time()[["elapsed"]]

  res <- run_grid(grid, args$seed, args$R, args$cores)
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
