# =============================================================================
# app-layout.R — the main panel's contract.
#
#   Rscript tests/app-layout.R      (needs shiny; run from the repo root)
#
# The result column was fifteen blocks stacked at one visual weight, with the
# estimate fourth. It is now: the result in a card, then the secondary material
# in a tab strip. These tests pin the parts of that which are behaviour rather
# than styling — what renders, in which mode, and where the power definition
# lives — so a later edit cannot quietly put a stale reading under a curve or
# hide a control that is still in force.
# =============================================================================
suppressWarnings(suppressMessages(library(shiny)))
if (!file.exists("app/R/lookup.R")) system("Rscript tools/build-app.R")

fail <- 0L
ok <- function(cond, msg) if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") }
H <- function(x) paste(as.character(x), collapse = "")

# --- the static UI: the answer comes before the things that explain it ------
# app.R resolves grid/grid.csv relative to the appdir, so evaluate it there.
ui <- local({
  owd <- setwd("app"); on.exit(setwd(owd), add = TRUE)
  e <- new.env(); sys.source("app.R", envir = e, keep.source = FALSE)
  H(e$ui)
})
ok(grepl("resultcard", ui, fixed = TRUE), "the result is wrapped in its own card")
ok(grepl("nav-tabs|tabsetPanel|data-toggle=\"tab\"", ui),
   "secondary content renders as a tab strip")
for (lbl in c("Reading this result", "How it was computed", "Share or cite", "Glossary"))
  ok(grepl(lbl, ui, fixed = TRUE), sprintf("tab present: %s", lbl))
# The View control offers exactly two ways of showing a design. The examples are
# a way of CHOOSING one, so they must not appear among them.
ok(!grepl('value="examples"', ui, fixed = TRUE),
   "Examples must not be an option in the View control")
ok(grepl('value="curve" checked', ui, fixed = TRUE) ||
   grepl('value="curve"[^>]*checked', ui),
   "the power curve is the default view")
ok(grepl('value="fixed" checked', ui, fixed = TRUE) ||
   grepl('value="fixed"[^>]*checked', ui),
   "a fixed schedule is the default sampling design")
ok(grepl("qdesc", ui, fixed = TRUE), "the View options carry explanations, not just names")
# The tabset must NOT carry an id: with one, tab state round-trips to the
# server, which is the pattern that is unreliable under shinylive/webR.
ok(!grepl('id="tabset', ui), "the tabset must not need a server round-trip")
# Order: the card must open before the tab strip appears.
ok(regexpr("resultcard", ui, fixed = TRUE) < regexpr("nav-tabs", ui),
   "the result card must precede the tabs in the document")

testServer("app", {
  base <- list(power_def = "all", N = 60, D = 14, lambda_bar = 2, cv = 0.3,
               cap = 0, compliance = 0.75, decay = 0, phi = 0.3, beta1 = 0.2,
               design_type = "poisson", seed = 20260709, R = 60)

  # --- single design: the result renders, and so does the toggle -----------
  do.call(session$setInputs, c(list(mode = "single"), base))
  ok(isTRUE(supported()), "a covered design is supported")
  ok(nzchar(H(output$power_def_ctl)), "the power-definition toggle renders in single view")
  ok(nzchar(H(output$interpretation)), "the reading renders in single view")
  ok(nzchar(H(output$howto)), "the method renders in single view")

  # --- the toggle still drives the number, under its new id-preserving UI --
  a <- pw()$v
  session$setInputs(power_def = "converged")
  b <- pw()$v
  ok(!isTRUE(all.equal(a, b)) || abs(a - b) < 1e-12,
     "switching the power definition must reach pw() (id and values unchanged)")
  session$setInputs(power_def = "all")

  # --- curve view ----------------------------------------------------------
  session$setInputs(mode = "curve")
  # The bug this layout change fixes: the power CURVE reads input$power_def for
  # its y axis, but the control used to live inside the single-design panel, so
  # in curve view the definition was in force yet invisible and unchangeable.
  ok(nzchar(H(output$power_def_ctl)),
     "the power-definition toggle must ALSO render in curve view (it drives the y axis)")
  # ... and the single-design prose must not sit underneath a curve. The client
  # used to hide these; now they are in a tab, so the guard has to be server-side.
  ok(is.null(output$interpretation) || !nzchar(H(output$interpretation)),
     "the single-design reading must not render in curve view")
  ok(is.null(output$howto) || !nzchar(H(output$howto)),
     "the single-design method must not render in curve view")
})

# --- the escape hatch: a design the grid refuses must still be runnable -----
# Regression test. The run button was gated on whether a SLAB EXISTS for the
# (model, schedule) partition, which was a fair proxy for "the grid can answer
# this" only while one partition had a slab. Once every partition had one, the
# gate was always open, so any refused design — a lever off its simulated
# values, a duration outside the range, a bracket that did not converge — left
# the user reading "run a live simulation" with no way to run one. The engine
# could answer all of them.
testServer("app", {
  base <- list(mode = "single", design_type = "fixed", N = 40, D = 14,
               lambda_bar = 3, cv = 0, eff = 0.2, cap = 0, compliance = 0.75,
               decay = 0, phi = 0.3, power_def = "all", target_power = 0.8,
               seed = 20260709, R = 40, show_advanced = FALSE)
  do.call(session$setInputs, base)
  ok(estimate()$source == "grid_exact", "the baseline design is answered by the grid")
  ok(!isTRUE(can_simulate()), "no run button while the grid answers")

  # every distinct way the grid can refuse must offer the run
  refusals <- list(
    "a lever off its simulated values" = list(compliance = 0.85),
    "a duration outside the range"     = list(D = 45),
    "an unsimulated effect"            = list(eff = 0.45),
    "N outside the hull"               = list(N = 250),
    "across the convergence cliff"     = list(lambda_bar = 1, D = 9))
  for (nm in names(refusals)) {
    do.call(session$setInputs, base)
    do.call(session$setInputs, refusals[[nm]])
    ok(estimate()$source == "unsupported", sprintf("%s: the grid refuses", nm))
    ok(isTRUE(can_simulate()), sprintf("%s: the run button must be offered", nm))
  }

  # and the run must actually reach the screen, or the button does nothing
  do.call(session$setInputs, base)
  session$setInputs(compliance = 0.85)
  session$setInputs(run = 1)
  r <- estimate()
  ok(r$source == "simulated", sprintf("a completed run replaces the refusal (got %s)", r$source))
  ok(isTRUE(supported()), "the simulated result counts as supported")
  ok(!is.na(r$power_all) && r$power_all >= 0 && r$power_all <= 1, "it carries a power estimate")
  # ... and must not linger once the design changes underneath it
  session$setInputs(compliance = 0.90)
  ok(estimate()$source != "simulated", "a stale run is discarded when the design changes")
})

cat(if (fail == 0) "\nLAYOUT: ALL PASSED\n" else sprintf("\nLAYOUT: %d FAILURE(S)\n", fail))
quit(status = if (fail == 0) 0 else 1)
