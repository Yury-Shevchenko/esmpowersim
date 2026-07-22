# =============================================================================
# app-roundtrip.R — the SMAAT handoff: ?return= capture, "send N back" button,
# the open-redirect guard, and the contextual discovery CTA.
#
#   Rscript tests/app-roundtrip.R      (needs shiny; run from the repo root)
#
# The round-trip is the linchpin of the two-way SMAAT integration: a study
# deep-links in with ?return=<study URL>, the researcher plans N, and one button
# hands N back. The guard matters because the button navigates window.top — an
# unchecked return URL would be an open redirect. testServer drives the real
# reactive graph, the only place return_url()/send_back wiring can be exercised
# without a browser.
# =============================================================================
suppressWarnings(suppressMessages(library(shiny)))
if (!file.exists("app/R/lookup.R")) system("Rscript tools/build-app.R")
fail <- 0L
ok <- function(cond, msg) if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") } else cat("  ok:", msg, "\n")
H <- function(x) paste(as.character(x), collapse = "")

testServer("app", {
  base <- list(mode="single", power_def="all", N=40, D=14, lambda_bar=3, cap=0,
               compliance=0.75, decay=0, phi=0.3, beta1=0.2, seed=20260709, R=60)
  # fixed design + a run => a real simulated result on screen (supported)
  do.call(session$setInputs, c(list(design_type="fixed"), base))
  session$setInputs(run = 1)
  ok(estimate()$source == "simulated", "simulated result present after run")
  ok(isTRUE(supported()), "result is supported")
  ok(!isTRUE(grid_backed()), "fixed design is not grid-backed (N falls back to input$N)")

  # no return URL yet: send_back hidden, discovery CTA visible
  ok(is.null(return_url()), "no return_url before any deep-link")
  ok(is.null(output$send_back), "send_back hidden when no return URL")
  ok(nzchar(H(output$smaat_cta)) && grepl("smaat.eu", H(output$smaat_cta)),
     "discovery CTA shows (links to smaat.eu) with a result and no return URL")

  # a valid SMAAT return URL arrives -> button renders with plannedN = input$N (40)
  session$setInputs(url_query = "return=https%3A%2F%2Fsmaat.eu%2Fdashboard%2Fstudies%2Fabc")
  ok(!is.null(return_url()), "valid SMAAT return_url captured")
  html <- H(output$send_back)
  ok(grepl("plannedN=40", html), "send_back link carries plannedN=40 (current N)")
  ok(grepl("_top", html), "send_back navigates the top tab (target=_top)")
  ok(is.null(output$smaat_cta), "discovery CTA suppressed once inside a SMAAT flow")
})

guard <- function(ret, label, expect_null = TRUE) testServer("app", {
  session$setInputs(url_query = paste0("return=", ret))
  ok(if (expect_null) is.null(return_url()) else !is.null(return_url()), label)
})
guard("https%3A%2F%2Fevil.example.com%2Fsteal", "non-SMAAT return URL rejected")
guard("https%3A%2F%2Fsmaat.eu.evil.com%2Fx",   "smaat.eu.evil.com suffix trick rejected")
guard("http%3A%2F%2Fsmaat.eu%2Fx",             "http (non-TLS) SMAAT URL rejected")
guard("https%3A%2F%2Fpower.smaat.eu%2F%3Fx%3D1", "https://power.smaat.eu subdomain accepted", expect_null = FALSE)

cat(if (fail==0) "\nROUNDTRIP: ALL PASSED\n" else sprintf("\nROUNDTRIP: %d FAILURE(S)\n", fail))
quit(status = if (fail==0) 0 else 1)
