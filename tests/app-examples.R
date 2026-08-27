# =============================================================================
# app-examples.R — every worked example must actually work.
#
#   Rscript tests/app-examples.R      (needs shiny; run from the repo root)
#
# This exists because two of the previous three examples did not. Both set an
# effect of 0.3 when the grid only ever simulated 0.1, 0.2 and 0.5, so the
# flagship "load a worked example" button landed a new user on a refusal; one of
# them also advertised a rate of 3/day that the grid does not have, which
# silently snapped to 2. Nothing caught either, because nothing loaded an
# example and looked at the answer.
#
# So: drive each example through the REAL reactive graph — the button, the
# controls it updates, the cell that gets built, the lookup that answers it —
# and require a usable result at the end.
# =============================================================================
suppressWarnings(suppressMessages(library(shiny)))
if (!file.exists("app/R/lookup.R")) system("Rscript tools/build-app.R")

fail <- 0L
ok <- function(cond, msg) if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") }

n_ex <- 0L; below <- 0L
testServer("app", {
  session$setInputs(mode = "examples", power_def = "all", target_power = 0.8,
                    cap = 0, compliance = 0.75, decay = 0, seed = 20260709, R = 60)
  ok(nzchar(paste(as.character(output$gallery), collapse = "")), "the gallery renders")
  n_ex <<- length(EX)
  ok(n_ex == 10L, sprintf("the gallery offers ten examples (found %d)", n_ex))

  for (nm in names(EX)) {
    e <- EX[[nm]]
    # press the button exactly as a user would
    session$setInputs(mode = "examples")
    apply_example(e)
    session$setInputs(mode = "single")
    # the controls the example claims to set must actually hold those values
    session$setInputs(N = e$N, D = e$D, lambda_bar = e$lambda, eff = e$eff,
                      design_type = if (is.null(e$design)) "poisson" else e$design,
                      cv = if (is.null(e$cv)) 0.3 else e$cv)
    ok(model_id() == e$model, sprintf("%s: loads model %d", nm, e$model))

    r <- estimate()
    # THE point of this file: an example must produce an answer, not a refusal.
    ok(r$source %in% c("grid_exact", "grid_interp"),
       sprintf("%s: must be answered from the grid, got '%s' (%s)", nm, r$source,
               if (is.na(r$reason)) "" else substr(r$reason, 1, 60)))
    if (r$source %in% c("grid_exact", "grid_interp")) {
      ok(!is.na(r$power_all) && r$power_all >= 0 && r$power_all <= 1,
         sprintf("%s: power is a proportion", nm))
      if (r$power_all < 0.8) below <<- below + 1L
    }

    # the strap line is generated, so it cannot contradict the design; check it
    # still names the levers it claims to
    strap <- ex_strap(e)
    ok(grepl(paste0("N = ", e$N), strap, fixed = TRUE), sprintf("%s: strap states N", nm))
    ok(grepl(paste0(e$D, " days"), strap, fixed = TRUE), sprintf("%s: strap states duration", nm))
    ok(grepl(as.character(e$lambda), strap, fixed = TRUE), sprintf("%s: strap states the rate", nm))
    ok(grepl(if (identical(e$design, "fixed")) "planned" else "triggered", strap, fixed = TRUE),
       sprintf("%s: strap states the schedule type", nm))
  }
})

# A gallery where every design succeeds teaches nothing about planning: the
# whole point of the tool is that plausible designs often fall short.
ok(below >= 3L, sprintf("examples should span the 80%% mark (only %d of %d fall short)", below, n_ex))
cat(sprintf("  %d of %d examples fall below 80%% power\n", below, n_ex))

cat(if (fail == 0) "\nEXAMPLES: ALL PASSED\n" else sprintf("\nEXAMPLES: %d FAILURE(S)\n", fail))
quit(status = if (fail == 0) 0 else 1)
