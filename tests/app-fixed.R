# =============================================================================
# app-fixed.R — reactive-wiring tests for the app's fixed-schedule mode.
#
#   Rscript tests/app-fixed.R      (needs shiny; run from the repo root)
#
# Guards the bug that shipped past unit tests and only showed in the browser:
# design_type and engine were separate inputs, so estimate() could observe one
# while the other still said something else and freeze on a stale answer.
# testServer exercises the real reactive graph, which plain function tests do not.
#
# The contract CHANGED once the fixed-schedule slabs were simulated: a planned
# schedule on a simulated cell is now answered from the grid, instantly, instead
# of always falling through to a live run. The race guard is kept, re-aimed at
# what can still fall through — a design the table does not cover.
# =============================================================================
suppressWarnings(suppressMessages(library(shiny)))
# build the appdir engine copies if they are not present (CI runs build first)
if (!file.exists("app/R/lookup.R")) system("Rscript tools/build-app.R")

fail <- 0L
ok <- function(cond, msg) if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") }

testServer("app", {
  # k=3/day for 14 days at N=40 is a simulated cell of the fixed slab.
  base <- list(mode = "single", power_def = "all", N = 40, D = 14, lambda_bar = 3,
               cap = 0, compliance = 0.75, decay = 0, phi = 0.3, beta1 = 0.2,
               seed = 20260709, R = 60)

  # 1. a simulated fixed design is answered from the grid, not sent to a run
  do.call(session$setInputs, c(list(design_type = "fixed"), base))
  ok(isTRUE(grid_capable()), "a fixed schedule has a precomputed slab")
  ok(!isTRUE(sim_mode()), "a simulated fixed cell must NOT need a live run")
  r <- estimate()
  ok(r$source == "grid_exact", sprintf("fixed design answers from the grid (got %s)", r$source))
  ok(isTRUE(supported()), "tiles populate immediately for a covered fixed design")
  # the mechanism is still the planned one: k*D*compliance = 3*14*0.75 = 31.5,
  # and a planned schedule thins Binomially, so the spread stays tight.
  ok(abs(r$mean_n - 31.5) < 1.5, sprintf("mean_n ~31.5 (got %.2f)", r$mean_n))
  ok(r$sd_n < 3.5, sprintf("planned schedule => tight sd_n (got %.2f)", r$sd_n))

  # 2. asking for a live run of the same design still works, and agrees with the
  #    grid. This is the real check on the new slabs: the table and the engine
  #    must describe the same design, or the lookup is quietly answering a
  #    different question than the one the button would run.
  session$setInputs(engine = "simulate", show_advanced = TRUE)
  ok(isTRUE(sim_mode()), "choosing 'simulate' must route a fixed design to a live run")
  session$setInputs(run = 1)
  s <- estimate()
  ok(s$source == "simulated", sprintf("after run, source is 'simulated' (got %s)", s$source))
  ok(abs(s$power_all - r$power_all) < 0.15,
     sprintf("live run must agree with the grid (grid %.3f vs live %.3f)",
             r$power_all, s$power_all))

  # 3. the race guard, re-aimed: a design OFF the fixed grid (60 days was never
  #    simulated) must route to simulation rather than freeze on a stale answer.
  session$setInputs(engine = "lookup", D = 60)
  ok(isTRUE(sim_mode()) || estimate()$source %in% c("unsupported", "awaiting", "simulated"),
     "an off-grid fixed design must not be answered from the table")

  # 4. switching back to a triggered design restores instant grid lookup.
  session$setInputs(design_type = "poisson", engine = "lookup",
                    N = 60, D = 14, lambda_bar = 2, cv = 0.3)
  ok(estimate()$source == "grid_exact", "triggered design returns to instant grid lookup")

  # 5. the fixed-mode design controls must be SLIDERS, not radio buttons.
  #    They were radios while the fixed levels were unevenly spaced (a Shiny
  #    slider can only step onto an arithmetic sequence); the extra rates and
  #    the 21-day duration were simulated specifically so the controls could
  #    match triggered mode. If a fixed slab ever goes missing the app falls
  #    back to radios by design, so this asserts the levels are still complete.
  session$setInputs(design_type = "fixed", engine = "lookup", D = 14, lambda_bar = 3)
  di <- paste(as.character(output$design_inputs), collapse = "")
  ok(grepl("irs|js-range-slider|slider", di), "fixed design inputs render as sliders")
  ok(!grepl('type="radio"', di, fixed = TRUE), "fixed design inputs are not radio buttons")
  ok(!grepl("<select", di, fixed = TRUE), "fixed design inputs contain no dropdown")
  # every stop the slider can reach must be an exact cell — that is the whole
  # reason the levels were filled in rather than snapped
  for (k in 1:8) {
    session$setInputs(lambda_bar = k)
    ok(estimate()$source == "grid_exact",
       sprintf("prompts/day = %d is an exact grid cell", k))
  }
  for (d in c(7, 14, 21, 28)) {
    session$setInputs(D = d)
    ok(estimate()$source == "grid_exact",
       sprintf("duration = %d days is an exact grid cell", d))
  }
  # The duration slider now moves a day at a time, so most stops are BETWEEN
  # simulated levels and must come back interpolated-and-labelled rather than
  # snapped to a neighbour or refused.
  session$setInputs(lambda_bar = 3)
  interp <- 0
  for (d in 1:28) {
    session$setInputs(D = d)
    e <- estimate()
    ok(e$source %in% c("grid_exact", "grid_interp"),
       sprintf("every day of the slider must answer (day %d gave %s)", d, e$source))
    if (identical(e$source, "grid_interp")) {
      interp <- interp + 1
      ok(e$interp_se_power > 0, sprintf("day %d must carry an interpolation SE", d))
    }
  }
  ok(interp > 0, "the slider must actually exercise interpolation")

  # ... and the sparse corner must be refused with a reason, not interpolated
  # across the convergence cliff.
  session$setInputs(lambda_bar = 1, N = 150, D = 9)
  g <- estimate()
  ok(g$source == "unsupported" && grepl("estimab", g$reason),
     "a design across the convergence cliff must be refused, and say why")
  session$setInputs(lambda_bar = 3, N = 40, D = 14)

  # 6. the cell a fixed design builds zeroes the inapplicable levers
  session$setInputs(design_type = "fixed", D = 14)
  cell <- build_cell()
  ok(identical(as.character(cell$trigger_mode), "fixed"), "cell carries trigger_mode=fixed")
  ok(cell$cv == 0 && cell$trigger_link == 0, "cv and trigger_link zeroed in fixed mode")
})

cat(if (fail == 0) "\nAPP-FIXED: ALL PASSED\n" else sprintf("\nAPP-FIXED: %d FAILURE(S)\n", fail))
quit(status = if (fail == 0) 0 else 1)
