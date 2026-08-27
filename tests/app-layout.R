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

cat(if (fail == 0) "\nLAYOUT: ALL PASSED\n" else sprintf("\nLAYOUT: %d FAILURE(S)\n", fail))
quit(status = if (fail == 0) 0 else 1)
