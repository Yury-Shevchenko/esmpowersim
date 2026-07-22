# =============================================================================
# app-grid-snap.R — the grid-backed model uses the SAME sliders as every other
# design, and its rate slider snaps the one off-grid value (3/day) to the
# nearest simulated level.
#
#   Rscript tests/app-grid-snap.R      (needs shiny; run from the repo root)
#
# Guards the fix for a UX report: the within-person (grid) model used dropdowns
# for duration / rate / CV while all other designs used sliders. They are sliders
# now, stepped onto the grid; duration (step 7) and CV (step 0.6) land only on
# simulated levels, and a rate of 3/day (never simulated) snaps to the nearest
# level rather than refusing — with ties breaking to the lower rate so a snapped
# answer never overstates power.
# =============================================================================
suppressWarnings(suppressMessages(library(shiny)))
if (!file.exists("app/R/lookup.R")) system("Rscript tools/build-app.R")
fail <- 0L
ok <- function(cond, msg) if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") } else cat("  ok:", msg, "\n")
H <- function(x) paste(as.character(x), collapse = "")

testServer("app", {
  base <- list(mode="single", design_type="poisson", engine="lookup", show_advanced=FALSE,
               power_def="all", N=60, D=14, cv=0.3, cap=0, compliance=0.75, decay=0,
               phi=0.3, eff=0.2, target_power=0.8, seed=20260709, R=60,
               q_category="within", m_within="3")
  # on-grid rate: exact hit, no snap note
  do.call(session$setInputs, c(base, list(lambda_bar=2)))
  ok(isTRUE(grid_backed()), "within-person model is grid-backed")
  ok(estimate()$source == "grid_exact", "lambda 2 is an exact grid hit")
  ok(!grepl("not simulated", H(output$provenance)), "no snap note at lambda 2")

  # off-grid rate 3: snaps to 2, still exact, note shown
  session$setInputs(lambda_bar = 3)
  ok(build_cell()$lambda_bar == 2, "lambda 3 snaps to nearest simulated 2 (conservative)")
  ok(estimate()$source == "grid_exact", "snapped lambda stays an exact grid hit (not unsupported)")
  prov <- H(output$provenance)
  ok(grepl("3/day was not simulated", prov) && grepl("2/day", prov), "provenance names the snap 3 -> 2")

  # design inputs are sliders, not dropdowns, in grid mode
  di <- H(output$design_inputs)
  ok(grepl("shiny-input-container", di) && grepl("irs|js-range-slider|sliderInput|slider", di),
     "grid design inputs render as sliders")
  ok(!grepl("<select", di), "grid design inputs contain no <select> dropdown")
})
cat(if (fail==0) "\nSNAP: ALL PASSED\n" else sprintf("\nSNAP: %d FAILURE(S)\n", fail))
quit(status = if (fail==0) 0 else 1)
