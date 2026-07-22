# =============================================================================
# repro.R — Archive the environment alongside every real run, so any results
# file carries a record of exactly how it was produced (prereg §6).
# =============================================================================

# Write a run-metadata file next to `outfile`: timestamp, git SHA, R + package
# versions (sessionInfo), and the invocation args. Best-effort; never fatal.
write_run_meta <- function(outfile, args = NULL, tag = "run") {
  dir <- if (nzchar(outfile)) dirname(outfile) else "."
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  base <- if (nzchar(outfile)) tools::file_path_sans_ext(basename(outfile)) else tag
  path <- file.path(dir, paste0(base, ".run-meta.txt"))

  git_sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE,
                             ignore.stderr = TRUE), error = function(e) NA)
  git_dirty <- tryCatch({
    st <- system("git status --porcelain", intern = TRUE, ignore.stderr = TRUE)
    if (length(st)) "dirty (uncommitted changes present)" else "clean"
  }, error = function(e) NA)

  con <- file(path, "w")
  on.exit(close(con))
  writeLines(c(
    sprintf("# run-meta (%s)", tag),
    sprintf("timestamp : %s", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    sprintf("git_sha   : %s", if (length(git_sha)) git_sha[1] else NA),
    sprintf("git_state : %s", git_dirty),
    sprintf("args      : %s", if (is.null(args)) "" else paste(unlist(args), collapse = " ")),
    "",
    "# sessionInfo()",
    capture.output(sessionInfo())
  ), con)
  message("[repro] wrote ", path)
  invisible(path)
}
