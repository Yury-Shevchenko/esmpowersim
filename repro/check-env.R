# =============================================================================
# check-env.R — Verify the running environment matches repro/versions.tsv.
#
# The single source of truth for versions is repro/versions.tsv. This script
# compares installed versions against it and reports match / mismatch / missing.
# Exits non-zero on any reproducibility-critical mismatch so it can gate a run.
#
#   Rscript repro/check-env.R              # check only
#   Rscript repro/check-env.R --install    # install any MISSING packages first
#
# `shiny` is treated as optional (needed only for the planning tool, not the
# numerical results), so a missing/mismatched shiny is a warning, not a failure.
# =============================================================================

args <- commandArgs(TRUE)
do_install <- "--install" %in% args

here <- tryCatch(dirname(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), error = function(e) "repro")
if (is.na(here) || here == "") here <- "repro"
man <- read.delim(file.path(here, "versions.tsv"), stringsAsFactors = FALSE)

optional <- c("shiny")
crit_fail <- FALSE
cat(sprintf("[check-env] manifest: %s\n", file.path(here, "versions.tsv")))
cat(sprintf("%-9s %-12s %-12s %s\n", "package", "wanted", "installed", "status"))

for (i in seq_len(nrow(man))) {
  pkg <- man$package[i]; want <- man$version[i]
  if (pkg == "R") {
    got <- paste(R.version$major, R.version$minor, sep = ".")
    status <- if (identical(got, want)) "ok" else "MISMATCH"
    if (status == "MISMATCH") crit_fail <- TRUE
    cat(sprintf("%-9s %-12s %-12s %s\n", pkg, want, got, status)); next
  }
  have <- requireNamespace(pkg, quietly = TRUE)
  if (!have && do_install && !(pkg %in% optional)) {
    message(sprintf("  installing %s %s ...", pkg, want))
    tryCatch({
      if (!requireNamespace("remotes", quietly = TRUE))
        install.packages("remotes", repos = "https://cloud.r-project.org")
      remotes::install_version(pkg, want, upgrade = "never", repos = "https://cloud.r-project.org")
      have <- requireNamespace(pkg, quietly = TRUE)
    }, error = function(e) message("    install failed: ", conditionMessage(e)))
  }
  if (!have) {
    status <- if (pkg %in% optional) "missing (optional)" else "MISSING"
    if (!(pkg %in% optional)) crit_fail <- TRUE
    cat(sprintf("%-9s %-12s %-12s %s\n", pkg, want, "-", status)); next
  }
  got <- as.character(packageVersion(pkg))
  # normalise 1.1.34 vs 1.1-34 for comparison
  norm <- function(v) gsub("[-.]", ".", v)
  ok <- identical(norm(got), norm(want))
  status <- if (ok) "ok" else if (pkg %in% optional) "mismatch (optional)" else "MISMATCH"
  if (!ok && !(pkg %in% optional)) crit_fail <- TRUE
  cat(sprintf("%-9s %-12s %-12s %s\n", pkg, want, got, status))
}

if (crit_fail) {
  cat("\n[check-env] FAIL — environment differs from the pinned manifest.\n",
      "         Use the Dockerfile or `renv::restore()` for an exact match,\n",
      "         or `Rscript repro/check-env.R --install` to add missing packages.\n", sep = "")
  quit(status = 1)
} else {
  cat("\n[check-env] OK — reproducibility-critical versions match the manifest.\n")
}
