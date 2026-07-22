# =============================================================================
# app.R — ESM power planner: event- and location-contingent sampling.
#
# A researcher-facing front end over the SAME simulation engine used for the
# paper (R/dgm.R, R/fit.R, R/performance.R, R/seeds.R). It answers the question
# no existing ESM power tool does: when each person's number of observations is
# a random, triggered quantity, what is my power — and how starved is the tail?
#
# Two engines behind one contract (see R/lookup.R):
#   * LOOKUP (default, instant) — reads the preregistered confirmatory grid.
#     R = 2000 per cell, so MCSE <= ~1.6 points.
#   * SIMULATE (opt-in, slow)   — runs the real engine in your browser. R = 100
#     by default, so MCSE ~5 points, and ~50 s for a typical design.
# The grid is not a cheap approximation of a live run: it is ~3x MORE precise
# AND instant. The UI says so, because the instinct is the opposite.
#
# Run locally:  Rscript tools/build-app.R && Rscript -e "shiny::runApp('app')"
#
# This directory is assembled by tools/build-app.R from an explicit allowlist.
# Everything under app/ is published verbatim — do not add files here by hand.
# =============================================================================

library(shiny)
# renv::dependencies() scans every .R file under this appdir, so lme4/MASS are
# already found via R/fit.R and R/dgm.R. Kept as cheap insurance: if detection
# ever misses them, the export silently ships a bundle that 404s at runtime.
if (FALSE) { library(lme4); library(MASS) }

for (f in c("seeds", "models", "dgm", "fit", "performance", "lookup"))
  source(file.path("R", paste0(f, ".R")))

G  <- load_grid("grid/grid.csv")
PR <- G$cells[G$cells$grid == "primary", ]
LV <- list(N = range(PR$N), D = sort(unique(PR$D)),
           lambda = sort(unique(PR$lambda_bar)), cv = sort(unique(PR$cv)))

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
nearest <- function(x, lv) if (is.null(x)) NULL else lv[which.min(abs(as.numeric(x) - lv))]

# Cost model. The fitted form predicts ms/rep under *node* webR (reproduces the
# measured 324 / 1394 / 7603 ms/rep to ~3%); Chrome measured 2.8x faster than
# node on the typical design, hence BROWSER_SCALE. That is one data point, so
# treat it as a starting estimate — the user's machine is not ours.
BROWSER_SCALE <- 0.359
node_ms_per_rep <- function(cell)
  13.6 * cell$N + 0.46 * cell$N * cell$D * cell$lambda_bar * cell$compliance
est_seconds <- function(cell, R) R * node_ms_per_rep(cell) * BROWSER_SCALE / 1000
fmt_dur <- function(s) if (s < 90) sprintf("~%.0f s", s) else sprintf("~%.0f min", s / 60)

badge <- function(txt, col) span(style = sprintf(
  "background:%s;color:#fff;border-radius:10px;padding:2px 9px;font-size:11px;font-weight:600;",
  col), txt)

# =============================================================================
ui <- fluidPage(
  # shinylive runs this app inside an iframe whose own URL carries no query
  # string — the deep-link params sit on the PARENT page. So session$clientData
  # $url_search is always empty here and reading it silently ignores every
  # param. Read the parent's search instead (same origin: the iframe is served
  # from the same host as the page), falling back to our own if we are framed
  # cross-origin or not framed at all.
  tags$head(
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var qs = '';
      try { qs = window.parent.location.search || ''; } catch (e) { qs = ''; }
      if (!qs) qs = window.location.search || '';
      Shiny.setInputValue('url_query', qs, {priority: 'event'});
      // Tell the hosting page (index.html) the engine is up so it can dismiss the
      // first-load splash exactly when the tool becomes interactive.
      try { window.parent.postMessage('esm-planner-ready', '*'); } catch (e) {}
    });
  ")),
    tags$style(HTML("
    /* --- question cards: radios styled as selectable cards --- */
    #q_category .shiny-options-group { display:flex; flex-direction:column; gap:8px; margin-top:4px; }
    #q_category .radio { margin:0; }
    #q_category .radio > label { display:block; border:1px solid #d4dde4; border-radius:9px;
      padding:9px 12px; cursor:pointer; background:#fff; transition:all .12s; }
    #q_category .radio > label:hover { border-color:#7aa7cf; background:#f7fafd; }
    #q_category .radio input[type=radio] { position:absolute; opacity:0; }
    #q_category .radio:has(input:checked) > label { border-color:#1b6ca8; background:#eef4fb;
      box-shadow:inset 0 0 0 1px #1b6ca8; }
    .qtitle { font-weight:600; font-size:14px; }
    .qdesc  { font-size:12px; color:#667; margin-top:2px; }
    /* --- salient simulation progress (default notification is easy to miss) --- */
    #shiny-notification-panel { position:fixed; top:16px; left:50%; transform:translateX(-50%);
      right:auto; bottom:auto; width:480px; max-width:92%; }
    .shiny-notification { border-radius:12px; padding:16px 20px; font-size:15px;
      box-shadow:0 10px 34px rgba(0,0,0,.22); border:1px solid #1b6ca8; background:#eaf3fb; color:#123; }
    .shiny-notification .progress { height:16px; border-radius:8px; margin-top:10px; background:#d7e6f4; }
    .shiny-notification .progress-bar, .shiny-notification .bar { background-color:#1b6ca8; }
    .shiny-notification .shiny-notification-message,
    .shiny-notification .progress-message { font-weight:600; color:#14425f; }
    /* --- sidebar section headers --- */
    .sec { font-size:11px; letter-spacing:.06em; text-transform:uppercase; color:#8a97a3;
      font-weight:700; margin:14px 0 6px; }
    .excard { border:1px solid #cfe0f3; border-radius:10px; padding:12px; background:#fff; height:100%; }
    "))),
  tags$div(style = "padding:2px 0 6px",
    tags$h2(style = "margin:0;font-weight:700", "Experience-Sampling Power Planner"),
    tags$p(style = "color:#555;margin:4px 0 0;font-size:15px",
      "Plan the sample size and design of an intensive longitudinal (ESM / EMA) study — including ",
      tags$b("event- and location-triggered designs, where each person's number of observations is random"),
      ".")),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # ===== 1. Research question (card-style chooser) ======================
      # Radios (not a dropdown): under shinylive/webR a selectize value does not
      # round-trip to the server, but radio bindings do. Rich HTML choiceNames +
      # CSS render them as selectable CARDS. The server derives the model from
      # these (see model_rv), so the deep-link and the user's clicks agree.
      tags$div(class = "sec", "1 · Your research question"),
      radioButtons("q_category", NULL,
        choiceValues = c("within", "l2", "moderation", "ar"),
        choiceNames = list(
          HTML("<div class='qtitle'>🔍 A within-person effect</div><div class='qdesc'>Does a momentary predictor track the momentary outcome, inside a person?</div>"),
          HTML("<div class='qtitle'>👥 A person-level difference</div><div class='qdesc'>Does who a person is relate to their average outcome?</div>"),
          HTML("<div class='qtitle'>🔀 A moderator of the effect</div><div class='qdesc'>Does something change how strong the within-person effect is?</div>"),
          HTML("<div class='qtitle'>🔁 Carry-over (dynamics)</div><div class='qdesc'>How much does one moment persist into the next?</div>"))),
      conditionalPanel("input.q_category == 'within'",
        radioButtons("m_within", "More precisely:",
          c("The effect can differ between people (random slope) — recommended" = "3",
            "One common effect for everyone (fixed slope)"                      = "4"))),
      conditionalPanel("input.q_category == 'l2'",
        radioButtons("m_l2", "The characteristic is:",
          c("A group (e.g. patients vs controls)"          = "1",
            "A continuous score (e.g. baseline severity)"  = "2"))),
      conditionalPanel("input.q_category == 'moderation'",
        radioButtons("m_mod", "The within-person effect differs by:",
          c("Group — effect can differ between people"                = "5",
            "Group — one common effect within each group"            = "6",
            "A continuous characteristic — effect can differ"        = "7",
            "A continuous characteristic — one common effect"        = "8"))),
      conditionalPanel("input.q_category == 'ar'",
        radioButtons("m_ar", "More precisely:",
          c("The carry-over itself (can differ between people)"       = "9",
            "A difference in carry-over between two groups"           = "10",
            "Carry-over predicted by a continuous characteristic"    = "11"))),
      uiOutput("model_note"),

      # ===== 2. Sampling design =============================================
      tags$div(class = "sec", "2 · Sampling design"),
      radioButtons("design_type", "How are prompts triggered?",
        c("Event or location — the count is random" = "poisson",
          "Fixed schedule — the count is planned"   = "fixed")),
      radioButtons("mode", "View", c("Single design" = "single", "Power curve over N" = "curve"),
                   inline = TRUE),
      sliderInput("N", "Participants (N)", 10, 300, 60, step = 5),
      conditionalPanel("input.mode == 'curve'",
        sliderInput("Ncurve", "N range for the curve", 10, 300, c(20, 150), step = 10)),
      # In lookup mode D / rate / CV are grid selectors, not sliders (interpolating
      # them costs 12–14 points of error; only N interpolates safely).
      uiOutput("design_inputs"),

      # ===== 3. Effect of interest + target power ===========================
      tags$div(class = "sec", "3 · Effect of interest"),
      uiOutput("effect_inputs"),
      sliderInput("target_power", "Target power to plan for", 0.5, 0.99, 0.8, step = 0.05),
      helpText(style = "margin-top:-6px",
        "The result panel reports the participants needed to reach this."),

      # ===== Advanced (progressive disclosure) ==============================
      checkboxInput("show_advanced",
        "Show advanced options (compliance, fatigue, caps, carry-over, exact simulation)", FALSE),
      conditionalPanel("input.show_advanced",
        # Grid vs live only matters for the paper's model on a triggered schedule.
        conditionalPanel("input.design_type != 'fixed' && input.q_category == 'within' && input.m_within == '3'",
          radioButtons("engine", "How to answer",
            c("Look up the simulated grid (instant)" = "lookup",
              "Simulate my exact design (slower, exact)" = "simulate"))),
        sliderInput("compliance", "Compliance — chance a prompt is answered", 0.3, 1, 0.75, step = 0.05),
        numericInput("cap", "Per-day cap on answered prompts (0 = none)", 0, min = 0, max = 50),
        sliderInput("decay", "Fatigue — daily drop in compliance", 0, 0.1, 0, step = 0.01),
        uiOutput("phi_input"),
        conditionalPanel("input.design_type != 'fixed'",
          selectInput("trigger_link", "Triggering mechanism",
            c("Exogenous — rate independent of the momentary context" = "0",
              "Context-linked / geofenced — endogenous"               = "0.75")))),
      uiOutput("lever_status"),

      # ===== Run (live simulation only) =====================================
      # Duplicates sim_mode() as a JS expression (a client condition can't call the
      # server): any model but the grid-backed within-person + a triggered design.
      conditionalPanel(
        "input.design_type == 'fixed' || input.q_category != 'within' || input.m_within != '3' || (input.show_advanced && input.engine == 'simulate')",
        hr(),
        conditionalPanel("input.show_advanced",
          sliderInput("R", "Monte Carlo replications", 30, 500, 100, step = 10),
          numericInput("seed", "Random seed", 20260709, min = 1)),
        actionButton("run", "Run live simulation", class = "btn-primary", width = "100%"),
        uiOutput("run_estimate"))
    ),
    mainPanel(
      width = 8,
      uiOutput("intro"),          # #4 dismissible "why this tool exists"
      uiOutput("provenance"),
      uiOutput("send_back"),      # hand the planned N back to a linked SMAAT study
      conditionalPanel("input.mode == 'single'",
        uiOutput("verdict"),
        uiOutput("tiles"),        # #5 traffic-lit stat tiles
        radioButtons("power_def", NULL, inline = TRUE,
          c("Counting non-convergence as no detection (lower bound)" = "all",
            "Given the model converged (upper bound)"                = "converged")),
        plotOutput("hist", height = "280px"),
        uiOutput("interpretation"),
        uiOutput("howto")
      ),
      conditionalPanel("input.mode == 'curve'",
        uiOutput("curve_status"),
        plotOutput("curve", height = "400px"), tableOutput("curve_tbl")),
      uiOutput("smaat_cta"),      # field this design with SMAAT — sits under the result
      tags$hr(),
      # #6 / #7 — share, cite, glossary
      tags$details(tags$summary(tags$b("Share or cite this design")),
        div(style = "margin-top:8px",
          tags$p(style = "font-size:13px;margin-bottom:4px", "Link to this exact design:"),
          uiOutput("share_link"),
          tags$p(style = "font-size:13px;margin:12px 0 4px", "How to cite:"),
          uiOutput("cite_text"))),
      tags$details(tags$summary(tags$b("Glossary — the terms used here")),
        tags$dl(style = "font-size:13px;margin-top:8px",
          tags$dt("Within-person effect"), tags$dd("How strongly a momentary predictor tracks the momentary outcome inside one person, averaged over people."),
          tags$dt("Between-person variability in trigger rate (CV)"), tags$dd("How much people differ in how often they get triggered. CV = 0 means everyone the same; high CV means some people rarely trigger — the case no other power tool models."),
          tags$dt("Carry-over / AR(1)"), tags$dd("How much a moment persists into the next (e.g. emotional inertia)."),
          tags$dt("Convergence"), tags$dd("The share of simulated datasets the model could actually be fit to. Low convergence means the design is too sparse or unbalanced to estimate reliably."),
          tags$dt("Monte Carlo error (±)"), tags$dd("Uncertainty in the power estimate from using a finite number of simulated datasets. It shrinks as replications rise; the grid uses 2,000, so its estimates are tight."),
          tags$dt("Starved tail — P(<10 obs)"), tags$dd("The fraction of participants who answer fewer than ~10 times. Those people contribute little to a within-person effect."))),
      tags$hr(),
      tags$small(style = "color:#777",
        "Deterministic seeding; on a fixed schedule at full compliance the engine reduces to an ",
        "exactly balanced design and is validated against Lafit et al. (2021) PowerAnalysisIL at ",
        "matched inputs. Variance components use illustrative defaults — results are indicative until you ",
        "set effects from a pilot or a comparable study. Not a substitute for a preregistered analysis plan.")
    )
  )
)

# =============================================================================
server <- function(input, output, session) {

  # --- design inputs: snap to simulated levels in lookup mode ---------------
  output$design_inputs <- renderUI({
    if (identical(input$design_type, "fixed")) {
      # A planned schedule has no rate distribution and no between-person rate
      # variation to model, so cv is not shown — it does not apply. Free sliders,
      # because the grid never simulated this design: every answer is a live run.
      tagList(
        sliderInput("lambda_bar", "Prompts per day (planned)", 1, 8, 3, step = 1),
        sliderInput("D", "Duration (days)", 3, 60, 14, step = 1),
        helpText("Everyone is scheduled the same number of prompts. Compliance and ",
                 "fatigue below still thin them, so the realised count is ",
                 tags$b("Binomial"), " — random, but around a planned maximum. That ",
                 "missingness is the part PowerAnalysisIL does not model."))
    } else if (grid_backed()) {
      # Sliders, like every other design — but stepped onto the simulated grid so
      # each stop is an exact R = 2000 cell. Duration (step 7) and CV (step 0.6)
      # land only on simulated levels; the rate slider also exposes 3/day, which
      # was not simulated, so it snaps to the nearest simulated rate (see the note
      # under the results). Interpolating duration or rate instead would reach 14
      # and 12 points of error — which is why only N is interpolated.
      tagList(
        sliderInput("D", "Duration (days)", 7, 28,
                    nearest(isolate(input$D), LV$D) %||% 14, step = 7),
        sliderInput("lambda_bar", "Mean trigger rate (per day)", 1, 4,
                    nearest(isolate(input$lambda_bar), LV$lambda) %||% 2, step = 1),
        sliderInput("cv", "Between-person variability in trigger rate (CV)", 0.3, 0.9,
                    nearest(isolate(input$cv), LV$cv) %||% 0.3, step = 0.6),
        helpText("Each slider stop is a simulated design, so the answer is exact ",
                 "(R = 2000). Duration and CV land only on simulated levels; a rate of ",
                 "3/day was not simulated and snaps to the nearest that was."))
    } else {
      tagList(
        sliderInput("D", "Duration (days)", 3, 60, 14, step = 1),
        sliderInput("lambda_bar", "Mean trigger rate (per day)", 0.5, 8, 2, step = 0.5),
        sliderInput("cv", "Between-person variability in trigger rate (CV)", 0, 1.5, 0.5, step = 0.1),
        helpText("The lever no existing tool models: CV=0 means everyone triggers at the same ",
                 "rate; high CV means some people rarely trigger."))
    }
  })

  # --- model description card (plain question + concrete example + Model N) --
  output$model_note <- renderUI({
    info <- model_info(model_id())
    div(style = "background:#f3f6f9;border-radius:8px;padding:10px 12px;margin-top:2px",
      tags$div(style = "font-size:13px", info$question),
      tags$div(style = "color:#555;font-size:12px;margin-top:6px;font-style:italic", info$example),
      tags$div(style = "color:#888;font-size:11px;margin-top:6px",
               sprintf("Powers the %s · Lafit et al. (2021), Model %d.",
                       target_label(spec_now()), model_id())))
  })

  # --- effect-of-interest slider (relabelled per model) + anchors + context --
  output$effect_inputs <- renderUI({
    sp <- spec_now()
    lbl <- switch(sp$target,
      b10 = if (sp$l1 == "lag") "Carry-over effect to detect" else "Within-person effect to detect",
      b01 = if (sp$l2 == "dummy") "Group difference to detect" else "Person-level predictor effect to detect",
      b11 = "Moderation (interaction) effect to detect")
    anchor <- if (sp$target == "b10" && sp$l1 == "lag")
      "Autoregressive effects in ESM are often ~0.2–0.5."
    else "Rough anchors (outcome-SD units): ~0.1 small · ~0.3 medium · ~0.5 large."
    ui <- list(
      sliderInput("eff", lbl, 0.05, 1.0, 0.2, step = 0.05),
      helpText(style = "margin-top:-8px", anchor,
               " Set this from a pilot or a comparable published study, not the default."))
    # context effects that exist in this model but are not the thing being powered
    if (sp$target == "b11")
      ui <- c(ui, list(sliderInput("b_main",
        if (sp$l1 == "lag") "Average carry-over (context)" else "Average within-person effect (context)",
        0, 0.8, 0.3, step = 0.05)))
    if (sp$target != "b01" && sp$l2 != "none")
      ui <- c(ui, list(sliderInput("b_l2", "Person-level main effect (context)", 0, 0.8, 0.3, step = 0.05)))
    do.call(tagList, ui)
  })

  # phi (carry-over) is a nuisance control for Models 1-8, shown in Advanced; for
  # the AR models the lag IS the effect, so it is set by the effect slider instead.
  output$phi_input <- renderUI({
    if (spec_now()$l1 != "lag")
      sliderInput("phi", "Carry-over from the previous moment (AR)", 0, 0.7, 0.3, step = 0.05)
  })

  # The selected model is held server-side, derived from the question-first radios:
  # the category picks which refinement radio is active, and that radio's value IS
  # the model id. A reactiveVal is the source of truth so the URL deep-link and the
  # user's clicks agree (radio inputs round-trip in webR; this also survives the
  # brief moments when a refinement radio is absent).
  model_rv <- reactiveVal("3")
  # When SMAAT (or any platform) deep-links here with a ?return= URL, we can hand
  # the planned sample size back to it. NULL unless a valid SMAAT return URL came in.
  return_url <- reactiveVal(NULL)
  observeEvent(list(input$q_category, input$m_within, input$m_l2, input$m_mod, input$m_ar), {
    cat <- input$q_category %||% "within"
    m <- switch(cat,
      within     = input$m_within %||% "3",
      l2         = input$m_l2     %||% "1",
      moderation = input$m_mod    %||% "5",
      ar         = input$m_ar     %||% "9")
    if (!is.null(m)) model_rv(as.character(m))
  })
  model_id <- reactive(as.integer(model_rv()))
  spec_now <- reactive(model_spec(model_id()))

  # Set the question radios FROM a model id (deep-link, and keeps them consistent).
  set_model_widgets <- function(id) {
    info <- model_info(id); cat <- info$category
    updateRadioButtons(session, "q_category", selected = cat)
    updateRadioButtons(session, switch(cat, within = "m_within", l2 = "m_l2",
                                        moderation = "m_mod", ar = "m_ar"),
                       selected = as.character(id))
    model_rv(as.character(id))
  }

  # Map the single "effect of interest" slider onto the model's target coefficient,
  # and fill the non-target effects from context controls (or sensible defaults).
  effect_betas <- function() {
    sp <- spec_now()
    eff <- input$eff %||% 0.2
    b_main  <- input$b_main  %||% 0.3    # L1 main effect when it is not the target
    b_l2    <- input$b_l2    %||% 0.3    # L2 main effect when it is not the target
    switch(sp$target,
      b10 = list(beta1 = eff,    beta_l2 = b_l2,  beta_cross = 0),
      b01 = list(beta1 = 0.3,    beta_l2 = eff,   beta_cross = 0),
      b11 = list(beta1 = b_main, beta_l2 = b_l2,  beta_cross = eff))
  }

  # NULL-safe: several of these controls live in renderUI blocks, so they are
  # briefly absent whenever the panel swaps (grid selectors <-> sliders). An
  # as.numeric(NULL) is numeric(0), which makes data.frame() throw "differing
  # number of rows" — and a throw here breaks the whole reactive flush, freezing
  # unrelated outputs. Every field therefore falls back to a default.
  n1 <- function(v, default) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) != 1L || is.na(v)) default else v
  }
  # In grid mode the rate slider can rest on 3/day, which was never simulated.
  # Snap it to the nearest simulated level so the lookup stays an exact R = 2000
  # cell; ties break to the lower rate, so a snapped answer never overstates
  # power. Only affects grid mode — live simulation reads the rate as-is.
  grid_lambda <- function(lam) {
    if (any(abs(lam - LV$lambda) < 1e-9)) return(lam)
    LV$lambda[which.min(abs(lam - LV$lambda))]
  }
  build_cell <- function(N = NULL) {
    fixed <- identical(input$design_type, "fixed")
    bt    <- effect_betas()
    capv  <- n1(input$cap, 0)
    lam   <- n1(input$lambda_bar, 2)
    if (!fixed && isTRUE(grid_backed())) lam <- grid_lambda(lam)
    data.frame(
      N            = if (is.null(N)) n1(input$N, 60) else N,
      D            = n1(input$D, 14),
      lambda_bar   = lam,
      # cv and trigger_link do not exist for a planned schedule: the count is set,
      # and a scheduled prompt fires whatever the context is doing. Pinned rather
      # than read from controls that are not shown in this mode.
      cv           = if (fixed) 0 else n1(input$cv, 0.5),
      cap          = if (capv <= 0) Inf else capv,
      compliance   = n1(input$compliance, 0.75),
      decay        = n1(input$decay, 0),
      phi          = n1(input$phi, 0.3),
      beta1        = bt$beta1,
      beta_l2      = bt$beta_l2,
      beta_cross   = bt$beta_cross,
      trigger_link = if (fixed) 0 else n1(input$trigger_link, 0),
      trigger_mode = if (fixed) "fixed" else "poisson",
      model        = model_id(),
      stringsAsFactors = FALSE)
  }

  # Deliberately permissive. This used to require every design input to be
  # present, which suspended the whole panel whenever a renderUI-owned control
  # was briefly absent — and under shinylive those values do not always report
  # back promptly, so the panel could stay blank indefinitely. build_cell() now
  # defaults every field, so a momentarily-missing control costs at most one
  # recompute with a default, instead of an empty screen.
  ready <- reactive(TRUE)

  # Only Model 3, triggered, with the lookup engine is answered by the grid.
  # `engine` lives in the Advanced panel; it only counts when that panel is open
  # (matching the run-button's JS condition), and defaults to lookup otherwise —
  # else the default view would fall through to live simulation.
  eng_choice <- reactive(if (isTRUE(input$show_advanced)) (input$engine %||% "lookup") else "lookup")
  grid_backed <- reactive(model_id() == 3L &&
                          !identical(input$design_type, "fixed") &&
                          identical(eng_choice(), "lookup"))
  sim_mode <- reactive(!grid_backed())

  # --- estimate: instant lookup by default, simulation only on demand -------
  # The completed simulation lives in a reactiveVal, NOT gated on input$run's
  # live value. That matters: the Run button is (re)rendered dynamically and an
  # actionButton resets to 0 when recreated, so gating "do we have a result" on
  # input$run == 0 blanked finished results the moment anything re-rendered the
  # button. A reactiveVal survives that; it is cleared only when the design or
  # mode actually changes (below), which is exactly when a past run stops
  # describing what's on screen.
  simVal   <- reactiveVal(NULL)   # last single-design live run
  curveVal <- reactiveVal(NULL)   # last live-simulated power curve

  decorate_sim <- function(r) {
    r$source <- "simulated"; r$source_grid <- NA_character_; r$source_rows <- NA_character_
    r$interp_dims <- ""; r$n_corners <- 0L; r$interp_se_power <- 0
    r$mcse_power_all <- sqrt(r$power_all * (1 - r$power_all) / r$R_total)
    r$reason <- NA_character_
    r
  }

  # The Run button triggers the live simulation for WHATEVER the current view is.
  # Nothing simulates without it — the power curve used to run six run_cell()s
  # reactively (on any input change), which is why "Building power curve…" fired
  # on its own. Both paths now write to a reactiveVal only from here.
  observeEvent(input$run, {
    req(input$run > 0)                    # ignore the reset-to-0 on button re-render
    is_curve <- identical(input$mode, "curve")
    secs <- est_seconds(build_cell(), input$R) * (if (is_curve) 6 else 1)
    if (secs > 600) {                     # guard rail: refuse the truly slow runs
      showModal(modalDialog(easyClose = TRUE, title = "That run is too slow for the browser",
        sprintf("This design would take about %s here. Use a design the grid covers, fewer replications, or the Docker/CLI path in the repository.", fmt_dur(secs))))
      return()
    }
    if (is_curve) {
      Ns <- unique(round(seq(input$Ncurve[1], input$Ncurve[2], length.out = 6) / 5) * 5)
      withProgress(message = "⏳ Building the power curve in your browser…", value = 0, {
        rows <- lapply(seq_along(Ns), function(k) {
          incProgress(1 / length(Ns),
                      detail = sprintf("design %d of %d — N = %d", k, length(Ns), Ns[k]))
          decorate_sim(run_cell(build_cell(N = Ns[k]), input$seed, input$R, cell_id = k))
        })
      })
      curveVal(do.call(rbind, rows))
    } else {
      withProgress(message = "⏳ Simulating your design in your browser…", value = 0.15, {
        # nudge the bar so it visibly moves across the (single, blocking) fit
        setProgress(detail = "running the model — this takes about a minute")
        r <- decorate_sim(run_cell(build_cell(), input$seed, input$R, cell_id = 1L))
        setProgress(value = 1, detail = "done")
      })
      simVal(r)
    }
  })

  # A finished simulation describes one exact design. If the design, engine or
  # view changes, drop it so the panel asks for a fresh run rather than showing a
  # stale number as if it were current.
  # Watch the RAW inputs, not build_cell(): an observer that calls build_cell()
  # runs on every flush, including the moments when a renderUI-owned control is
  # briefly absent — and any error there would break the whole reactive graph.
  observeEvent(list(model_rv(), input$N, input$D, input$lambda_bar, input$cv,
                    input$cap, input$compliance, input$decay, input$phi,
                    input$eff, input$b_main, input$b_l2, input$trigger_link,
                    input$engine, input$design_type, input$mode, input$Ncurve),
               { simVal(NULL); curveVal(NULL) }, ignoreInit = TRUE)

  # A one-row sentinel so the panel shows a clean "ready to simulate" state
  # rather than freezing on whatever it last displayed. Same columns the
  # renderers read.
  awaiting_row <- function() {
    out <- unsupported(as.list(build_cell()),
      "This design is simulated live — press “Run live simulation” below.")
    out$source <- "awaiting"; out
  }

  estimate <- reactive({
    req(ready())
    if (sim_mode()) { r <- simVal(); if (is.null(r)) awaiting_row() else r }
    else lookup_cell(as.list(build_cell()), G)
  })
  # "awaiting" and "unsupported" both mean "no number to show yet", so the tiles
  # blank in both — but the provenance line distinguishes them.
  supported <- reactive(!estimate()$source %in% c("unsupported", "awaiting"))

  # --- which power? --------------------------------------------------------
  # power_all counts non-convergence as a failure to detect; power conditions on
  # the model having fitted. They diverge by up to 42 points (N=150, D=7, rate=1,
  # cv=0.3: 0.97 vs 0.54 at 56% convergence). A planner that shows 0.97 there is
  # blessing a design that fails 44% of the time — so power_all is the default.
  # It is a lower bound, though: a real researcher would simplify the random
  # effects and refit. Hence a toggle, and never a silent choice.
  pw <- reactive({
    r <- estimate()
    if (identical(input$power_def, "converged")) list(v = r$power, se = r$mcse_power)
    else                                         list(v = r$power_all, se = r$mcse_power_all)
  })

  # --- solve for N: smallest N reaching the target power ---------------------
  # Grid-backed only (Model 3, triggered): a fine scan over interpolated grid
  # lookups is instant. For live-simulated models this would need many runs, so
  # the verdict there points the user to the power-curve view instead.
  recommended_n <- reactive({
    req(grid_backed())
    tp <- input$target_power %||% 0.8
    base <- as.list(build_cell())
    Ns <- seq(min(PR$N), max(PR$N), by = 5)            # 20 … 150
    pwr <- vapply(Ns, function(n) {
      base$N <- n; r <- lookup_cell(base, G)
      if (identical(r$source, "unsupported")) NA_real_ else r$power_all
    }, numeric(1))
    hit <- which(pwr >= tp)
    if (length(hit)) list(enough = TRUE, n = Ns[min(hit)])
    else list(enough = FALSE, maxN = max(Ns), maxP = suppressWarnings(max(pwr, na.rm = TRUE)))
  })

  # --- plain-language verdict above the tiles --------------------------------
  output$verdict <- renderUI({
    r <- estimate(); if (!supported()) return(NULL)
    tp <- input$target_power %||% 0.8
    p  <- r$power_all                                  # planning-relevant lower bound
    met <- p >= tp
    cur <- sprintf("With %d participants you have %.0f%% power to detect this effect (target %.0f%%).",
                   r$N, 100 * p, 100 * tp)
    rec <- if (isTRUE(grid_backed())) {
      rn <- recommended_n()
      if (isTRUE(rn$enough)) {
        if (rn$n < r$N) sprintf(" As few as %d participants would reach %.0f%%.", rn$n, 100 * tp)
        else if (rn$n == r$N) " You are right at the target."
        else sprintf(" You would need about %d participants to reach %.0f%%.", rn$n, 100 * tp)
      } else sprintf(" Even %d participants stay below target (up to ~%.0f%%) for this design — increase the rate, duration, or effect, or lower the target.",
                     rn$maxN, 100 * rn$maxP)
    } else " To find the number of participants you need, switch to the “Power curve over N” view and press Run."
    col <- if (met) "#1e7e34" else "#b8860b"
    div(style = sprintf("border-left:4px solid %s;background:%s;padding:8px 12px;margin-bottom:12px;border-radius:4px",
                        col, if (met) "#eef7f0" else "#fbf6ea"),
        tags$b(cur), tags$span(rec))
  })

  # The N we would hand back to a linked study: the recommended sample size when
  # the grid can solve for it and it is reachable, otherwise whatever N the user
  # is currently looking at. Kept as one definition so button and label agree.
  n_to_return <- reactive({
    if (isTRUE(grid_backed())) {
      rn <- tryCatch(recommended_n(), error = function(e) NULL)
      if (!is.null(rn) && isTRUE(rn$enough)) return(as.integer(rn$n))
    }
    as.integer(n1(input$N, 60))
  })

  # --- round-trip: send the planned N back to the linked SMAAT study ----------
  # Only rendered when a valid SMAAT ?return= URL arrived. A plain link with
  # target="_top" navigates the whole tab (the app lives in shinylive's iframe),
  # so no cross-origin scripting is needed and there is no open-redirect surface
  # beyond the SMAAT-only allowlist enforced when return_url was captured.
  output$send_back <- renderUI({
    ru <- return_url(); if (is.null(ru)) return(NULL)
    n  <- n_to_return()
    sep <- if (grepl("\\?", ru)) "&" else "?"
    href <- sprintf("%s%splannedN=%d", ru, sep, n)
    div(style = "margin:0 0 14px;padding:10px 14px;background:#eef4fb;border:1px solid #cfe0f3;border-radius:8px;display:flex;align-items:center;gap:12px;flex-wrap:wrap",
        tags$span(style = "font-size:14px",
          "Planning a study in SMAAT — send a planned sample size of ", tags$b(n),
          " back to it:"),
        tags$a(href = href, target = "_top",
               style = "background:#2f6fb0;color:#fff;padding:6px 14px;border-radius:6px;text-decoration:none;font-size:14px;font-weight:600;white-space:nowrap",
               sprintf("Use N = %d in SMAAT →", n)))
  })

  # --- discovery CTA: field this design with SMAAT ---------------------------
  # Appears once the user has a result on screen, and never when they already
  # arrived from a linked study. A plain smaat.eu link — the paper still cites the
  # neutral repo/DOI, so only the hosted app points at the platform.
  output$smaat_cta <- renderUI({
    if (!is.null(return_url())) return(NULL)      # already inside a SMAAT flow
    if (!isTRUE(supported())) return(NULL)         # contextual: only with a result
    div(style = "margin:18px 0;padding:18px 20px;background:#eaf2fb;border:1px solid #cfe0f3;border-radius:10px",
        div(style = "font-size:16px;font-weight:700;color:#1b3a5b;margin-bottom:5px",
            "Ready to run this study?"),
        div(style = "font-size:14px;line-height:1.5;color:#33475b;margin-bottom:14px",
            "SMAAT fields event-, location-, and time-triggered experience-sampling ",
            "studies — the triggered designs this planner is built for — with ",
            "notification scheduling, compliance monitoring, and secure data collection."),
        tags$a(href = "https://smaat.eu/", target = "_blank", rel = "noopener",
               style = paste0("display:inline-block;background:#2f6fb0;color:#fff;",
                              "padding:10px 20px;border-radius:8px;text-decoration:none;",
                              "font-size:15px;font-weight:600"),
               "Explore SMAAT →"))
  })

  # --- #4 dismissible intro: why this tool exists --------------------------
  intro_dismissed <- reactiveVal(FALSE)
  observeEvent(input$dismiss_intro, intro_dismissed(TRUE))
  output$intro <- renderUI({
    if (isTRUE(intro_dismissed())) return(NULL)
    excard <- function(id, title, body, foot) column(4,
      div(class = "excard",
        tags$div(style = "font-weight:600;font-size:13px", title),
        tags$div(style = "font-size:12px;color:#555;margin:6px 0 10px;line-height:1.35", body),
        tags$div(style = "font-size:11px;color:#888;margin-bottom:8px", foot),
        actionButton(id, "Load this example", class = "btn btn-default btn-sm", width = "100%")))
    div(style = "margin-bottom:16px",
      div(style = "background:#eef4fb;border:1px solid #cfe0f3;border-radius:10px;padding:14px 16px",
        tags$div(style = "font-size:14px;line-height:1.45",
          tags$b("When prompts are triggered by events or places, each person's number of observations is random"),
          " — and it varies a lot between people. Someone who rarely enters the triggering context answers only a ",
          "handful of times and tells you little; someone who enters it constantly answers dozens. That uneven, ",
          "unplannable count is what ordinary power tools ignore, and it is exactly what this planner models. ",
          "Choose your research question on the left, describe your sampling design, and read the power you'd get — ",
          "and the number of participants you'd need."),
        tags$div(style = "text-align:right;margin-top:6px;font-size:13px",
          actionLink("dismiss_intro", "Dismiss this intro"))),
      tags$div(style = "font-weight:600;font-size:13px;margin:14px 0 8px;color:#444",
        "New here? Load a worked example and explore:"),
      fluidRow(
        excard("ex_typical",
          "A two-week mood study",
          paste0("Sixty undergraduates carry a phone for 14 days and answer about two prompts a day, at random ",
                 "times. You want to know whether momentary stress predicts momentary negative affect, within a ",
                 "person — and whether that link differs across people."),
          "Within-person effect · N = 60 · 14 days · ~2/day · triggered"),
        excard("ex_sparse",
          "A sparse geofenced study",
          paste0("A prompt fires only when a participant enters a bar. Some people go most evenings and answer ",
                 "often; others rarely go and answer only two or three times. Does the location-triggered urge to ",
                 "drink track their mood? Watch the starved tail and the convergence warning."),
          "Within-person effect · N = 40 · 7 days · ~1/day · high between-person variability"),
        excard("ex_ar",
          "Emotional inertia in depression",
          paste0("Ninety participants report their affect several times a day for two weeks. You are testing how ",
                 "strongly negative affect carries over from one moment to the next — emotional inertia — and how ",
                 "much that carry-over differs between people."),
          "Autoregressive effect · N = 90 · 14 days · ~3/day")))
  })

  # --- #5 traffic-lit stat tiles -------------------------------------------
  tile <- function(value, label, bg, border) column(3,
    div(style = sprintf("background:%s;border:1px solid %s;border-radius:8px;padding:12px;text-align:center;min-height:88px",
                        bg, border),
        h4(style = "margin:0", value), tags$small(style = "color:#555", label)))
  # green / amber / red backgrounds
  RGB <- list(g = c("#eef7f0", "#bfe3c8"), a = c("#fbf6ea", "#efd9a8"),
              r = c("#fbeeee", "#e6bcbc"), n = c("#f4f6f8", "#dde3e8"))
  output$tiles <- renderUI({
    if (!supported()) {
      return(fluidRow(
        tile("—", "power", RGB$n[1], RGB$n[2]), tile("—", "model convergence", RGB$n[1], RGB$n[2]),
        tile("—", "mean obs / person", RGB$n[1], RGB$n[2]), tile("—", "starved tail", RGB$n[1], RGB$n[2])))
    }
    r <- estimate(); p <- pw(); tp <- input$target_power %||% 0.8
    pc <- if (p$v >= tp) "g" else if (p$v >= 0.5) "a" else "r"
    cc <- if (r$conv_rate >= 0.95) "g" else if (r$conv_rate >= 0.9) "a" else "r"
    tc <- if (r$below_k <= 0.1) "g" else if (r$below_k <= 0.2) "a" else "r"
    fluidRow(
      tile(sprintf("%.0f%%", 100 * p$v), "power", RGB[[pc]][1], RGB[[pc]][2]),
      tile(sprintf("%.0f%%", 100 * r$conv_rate), "model convergence", RGB[[cc]][1], RGB[[cc]][2]),
      tile(sprintf("%.1f", r$mean_n), "mean obs / person", RGB$n[1], RGB$n[2]),
      tile(sprintf("%.0f%%", 100 * r$below_k), "answer < 10 times (starved tail)", RGB[[tc]][1], RGB[[tc]][2]))
  })

  # --- #7 example designs: one click fills a concrete scenario --------------
  apply_example <- function(cfg) {
    set_model_widgets(cfg$model)
    updateRadioButtons(session, "design_type", selected = cfg$design %||% "poisson")
    updateSliderInput(session, "N", value = cfg$N)
    updateSliderInput(session, "eff", value = cfg$eff)
    updateSliderInput(session, "target_power", value = cfg$target %||% 0.8)
    # D / rate / CV are sliders in every mode now, so one update path covers both
    # the grid-backed and the live-simulated examples.
    updateSliderInput(session, "D", value = cfg$D)
    updateSliderInput(session, "lambda_bar", value = cfg$lambda)
    if (!is.null(cfg$cv)) updateSliderInput(session, "cv", value = cfg$cv)
  }
  EX <- list(
    typical = list(model = 3, N = 60, D = 14, lambda = 2, cv = 0.3, eff = 0.3, grid = TRUE),
    sparse  = list(model = 3, N = 40, D = 7,  lambda = 1, cv = 0.9, eff = 0.2, grid = TRUE),
    ar      = list(model = 9, N = 90, D = 14, lambda = 3, cv = 0.3, eff = 0.3, grid = FALSE))
  observeEvent(input$ex_typical,  apply_example(EX$typical))
  observeEvent(input$ex_sparse,   apply_example(EX$sparse))
  observeEvent(input$ex_ar,       apply_example(EX$ar))

  # --- #6 share this design (URL) + how to cite ----------------------------
  design_query <- reactive({
    fixed <- identical(input$design_type, "fixed")
    p <- c(sprintf("model=%d", model_id()),
           sprintf("N=%d",     as.integer(n1(input$N, 60))),
           sprintf("D=%g",     n1(input$D, 14)),
           sprintf("lambda=%g", n1(input$lambda_bar, 2)),
           sprintf("cv=%g",    if (fixed) 0 else n1(input$cv, 0.5)),
           sprintf("eff=%g",   input$eff %||% 0.2),
           sprintf("target=%g", input$target_power %||% 0.8),
           sprintf("design=%s", if (fixed) "fixed" else "triggered"))
    paste0("?", paste(p, collapse = "&"))
  })
  output$share_link <- renderUI({
    q <- design_query()
    tagList(
      tags$input(id = "share_q", type = "text", readonly = NA, value = q,
                 style = "width:100%;font-size:12px;padding:4px;border:1px solid #ccc;border-radius:4px",
                 onclick = "this.select()"),
      tags$button("Copy full link", class = "btn btn-default btn-sm", style = "margin-top:6px",
        onclick = paste0(
          "var q=document.getElementById('share_q').value;",
          "var l=(window.parent&&window.parent.location)?window.parent.location:location;",
          "var u=l.origin+l.pathname.replace(/\\?.*$/,'')+q;",
          "if(navigator.clipboard){navigator.clipboard.writeText(u);}",
          "var b=this;b.innerText='Copied \\u2713';setTimeout(function(){b.innerText='Copy full link';},1500);")))
  })
  output$cite_text <- renderUI({
    div(style = "font-size:12px;background:#f7f7f7;padding:8px;border-radius:4px",
      "Shevchenko, Y. (2026). esmpowersim: power and design planning for event- and ",
      "location-contingent experience sampling (v0.1.0) [Computer software]. ",
      tags$a(href = "https://power.smaat.eu", "https://power.smaat.eu"),
      tags$span(style = "color:#888", "  · companion to the accompanying methods paper."))
  })

  # --- provenance welded to the number, from one source of truth ------------
  output$provenance <- renderUI({
    # The badge describes a SINGLE-design estimate; in curve view the curve panel
    # shows its own status, so rendering here too would double the "Ready to
    # simulate" line. Single view only.
    if (identical(input$mode, "curve")) return(NULL)
    r <- estimate()
    b <- switch(r$source,
      grid_exact  = badge("● Exact grid cell · R = 2000", "#1e7e34"),
      grid_interp = badge(sprintf("◐ Interpolated over %s · R = 2000", r$interp_dims), "#1b6ca8"),
      simulated   = badge(sprintf("▲ Live simulation · R = %s", r$R_total), "#b8860b"),
      awaiting    = badge("▷ Ready to simulate", "#1b6ca8"),
      unsupported = badge("✕ Not answerable from the grid", "#6c757d"))
    extra <- if (identical(r$source, "grid_interp") && r$interp_se_power > 0)
      tags$small(style = "color:#555;margin-left:8px", sprintf(
        "interpolation uncertainty ±%.1f pts — a property of the grid's resolution, separate from Monte Carlo error and it does not shrink with more replications.",
        100 * r$interp_se_power))
    # If the rate slider is between simulated levels (only 3/day is), say which
    # simulated rate the shown result is actually for.
    raw_lam <- n1(input$lambda_bar, 2)
    snap <- if (isTRUE(grid_backed()) && !is.na(r$lambda_bar) &&
                abs(raw_lam - r$lambda_bar) > 1e-9)
      div(style = "color:#b8860b;margin-top:6px;font-size:13px", sprintf(
        "A rate of %g/day was not simulated — showing the nearest simulated rate, %g/day.",
        raw_lam, r$lambda_bar))
    div(style = "margin-bottom:10px", b, extra, snap,
        if (!supported()) div(style = "color:#6c757d;margin-top:8px", r$reason))
  })

  # --- the histogram stays live even in lookup mode -------------------------
  # one_study_ns is dgm-only (no lmer): one dataset, sub-second even in wasm.
  output$hist <- renderPlot({
    req(ready())
    cell <- build_cell()
    set.seed(rep_seed(as.integer(input$seed %||% 20260709), 1L, 1L))
    pars <- modifyList(default_pars(), list(
      beta1 = cell$beta1, phi = cell$phi, lambda_bar = cell$lambda_bar, cv = cell$cv,
      cap = cell$cap, compliance = cell$compliance, decay = cell$decay,
      trigger_link = cell$trigger_link))
    df <- generate_dataset(cell$N, cell$D, pars)
    ns <- as.integer(table(factor(df$id, levels = seq_len(cell$N))))
    hist(ns, breaks = max(8, min(40, diff(range(ns)) + 1)), col = "#1b6ca8", border = "white",
         main = "Effective observations per person (one simulated study)",
         xlab = "observations answered by a participant")
    abline(v = 10, col = "#c0392b", lwd = 2, lty = 2)
    legend("topright", bty = "n", lty = 2, col = "#c0392b", legend = "typical minimum (10 obs)")
  })

  output$interpretation <- renderUI({
    r <- estimate(); if (!supported()) return(NULL)
    p <- pw()
    warn <- if (r$conv_rate < 0.99) tags$li(style = "color:#c0392b", sprintf(
      "Models fail to converge %.0f%% of the time. Those runs yield no usable estimate at all — which is why the headline counts them as failures to detect.",
      100 * (1 - r$conv_rate)))
    tail <- if (r$below_k > 0.2) tags$li(style = "color:#c0392b", sprintf(
      "%.0f%% of participants contribute fewer than 10 observations — a starved lower tail that drags power down.",
      100 * r$below_k))
    tags$div(tags$h4("Reading this design"), tags$ul(
      # Derive the prose from the tile's own FORMATTED value, not from the raw
      # number: sprintf() and round() disagree at boundaries (0.975 -> tile
      # "0.97" but round() 98%), and a tile and a sentence that contradict each
      # other about the same quantity destroy trust faster than either is worth.
      tags$li(sprintf("Power to detect the within-person effect: %.0f%% (Monte Carlo SE %s).",
                      100 * as.numeric(sprintf("%.2f", p$v)),
                      if (100 * p$se < 0.5) "under 0.5 pts" else sprintf("%.0f pts", 100 * p$se))),
      tags$li(sprintf("Each participant answers %.1f prompts on average (SD %.1f).", r$mean_n, r$sd_n)),
      warn, tail,
      if (is.null(warn) && is.null(tail))
        tags$li(style = "color:#1e7e34", "Healthy design: good convergence and few starved participants.")))
  })

  # --- every number must trace back to a row -------------------------------
  output$howto <- renderUI({
    r <- estimate(); if (!supported()) return(NULL)
    tags$details(tags$summary("How was this computed?"), tags$small(tags$ul(
      tags$li(sprintf("Source: %s%s", r$source,
                      if (!is.na(r$source_grid)) paste0(" (", r$source_grid, " slab)") else "")),
      if (!is.na(r$source_rows)) tags$li(sprintf("Grid row(s) %s of grid.csv", r$source_rows)),
      tags$li(sprintf("Replications: %s · master seed 20260709", r$R_total)),
      if (identical(r$source, "grid_interp"))
        tags$li(sprintf("Interpolated linearly on the empirical logit over %s, between %d bracketing cells.",
                        r$interp_dims, r$n_corners)))))
  })

  # --- lever badges: show which control took you off the grid ---------------
  output$lever_status <- renderUI({
    req(ready())
    if (!grid_backed()) return(NULL)
    off <- off_ref(as.list(build_cell()), G$ref)
    if (!length(off)) return(div(style = "margin-top:8px", badge("all levers on grid", "#1e7e34")))
    div(style = "margin-top:8px",
        badge(sprintf("%d lever%s off grid: %s", length(off),
                      if (length(off) > 1) "s" else "", paste(off, collapse = ", ")), "#b8860b"))
  })

  # --- the slow path: the time estimate sits beside the STATIC button, so it
  #     can update without recreating the button (which would reset input$run).
  output$run_estimate <- renderUI({
    req(ready())
    secs <- est_seconds(build_cell(), input$R) * (if (identical(input$mode, "curve")) 6 else 1)
    if (secs > 600) return(div(style = "color:#c0392b;font-size:12px;margin-top:6px", sprintf(
      "~%s in the browser — too slow to run here. Use the Docker/CLI path, or a design the grid covers.",
      fmt_dur(secs))))
    tags$small(style = "color:#777;display:block;margin-top:6px",
               sprintf("Estimated run time: %s. The grid answers instantly if a nearby design will do.",
                       fmt_dur(secs)))
  })

  # --- curve mode ----------------------------------------------------------
  # In lookup mode, snap to the grid's own N levels: every point is then an exact
  # R=2000 hit, for free — strictly better than 6 interpolated ones.
  curve_tab <- reactive({
    req(identical(input$mode, "curve"))
    if (grid_backed()) {
      # instant lookups — safe to compute reactively (no simulation)
      n <- sort(unique(PR$N)); Ns <- n[n >= input$Ncurve[1] & n <= input$Ncurve[2]]
      req(length(Ns) > 0)
      rows <- lapply(Ns, function(v) {
        r <- lookup_cell(as.list(build_cell(N = v)), G)
        if (identical(r$source, "unsupported")) NULL else r
      })
      do.call(rbind, Filter(Negate(is.null), rows))
    } else {
      # live-simulated curve: computed ONLY by the Run button (into curveVal),
      # never reactively — otherwise it fires six simulations on every input
      # change. NULL until the user runs it.
      curveVal()
    }
  })

  # In curve view, a live-simulated curve only exists after the user runs it.
  output$curve_status <- renderUI({
    if (identical(input$mode, "curve") && !grid_backed() && is.null(curveVal()))
      div(style = "color:#1b6ca8;margin:10px 0",
          badge("▷ Ready to simulate", "#1b6ca8"),
          tags$span(style = "margin-left:8px",
            "Press “Run live simulation” to build the power curve (six designs)."))
  })

  curve_y <- function(t) if (identical(input$power_def, "converged"))
    list(y = t$power, se = t$mcse_power) else list(y = t$power_all, se = t$mcse_power_all)

  # Smallest N on the curve where power first reaches the target (linear
  # interpolation between the two bracketing points). NA if it never does.
  crossing_n <- function(t, y, tp) {
    o <- order(t$N); N <- t$N[o]; y <- y[o]
    hit <- which(y >= tp)
    if (!length(hit)) return(NA_real_)
    i <- hit[1]
    if (i == 1) return(N[1])
    N[i - 1] + (tp - y[i - 1]) / (y[i] - y[i - 1]) * (N[i] - N[i - 1])
  }
  output$curve <- renderPlot({
    t <- curve_tab(); req(!is.null(t) && nrow(t) > 0)
    yy <- curve_y(t); tp <- input$target_power %||% 0.8
    plot(t$N, yy$y, type = "b", pch = 19, lwd = 2, col = "#1b6ca8", ylim = c(0, 1),
         xlab = "N (participants)", ylab = "power",
         main = sprintf("Power vs N — rate %g/day, D %g d, CV %g, effect %g",
                        t$lambda_bar[1], t$D[1], t$cv[1], t$beta1[1]))
    arrows(t$N, pmax(0, yy$y - yy$se), t$N, pmin(1, yy$y + yy$se),
           angle = 90, code = 3, length = 0.03, col = "#1b6ca8")
    abline(h = tp, col = "#c0392b", lty = 2)
    nc <- crossing_n(t, yy$y, tp)
    if (!is.na(nc)) {
      abline(v = nc, col = "#c0392b", lty = 3)
      text(nc, 0.05, sprintf("N ≈ %.0f for %.0f%% power", nc, 100 * tp),
           pos = 4, col = "#c0392b", cex = 0.9)
    }
  })
  output$curve_tbl <- renderTable({
    t <- curve_tab(); req(!is.null(t) && nrow(t) > 0)
    yy <- curve_y(t)
    data.frame(N = t$N, power = round(yy$y, 2), MCSE = round(yy$se, 3),
               convergence = round(t$conv_rate, 2), `mean n` = round(t$mean_n, 1),
               `P(<10 obs)` = round(t$below_k, 2), source = t$source, check.names = FALSE)
  })

  # --- URL prefill: read once, clamp everything, never error ----------------
  # This is what lets a platform deep-link a study's design into the planner.
  # Contract: every param optional; unknown params ignored (so ?utm_source= is
  # harmless); values clamped to the legal range; the app must never error on
  # bad input, and must work perfectly with no params at all.
  observeEvent(input$url_query, once = TRUE, {
    qs <- tryCatch(parseQueryString(input$url_query), error = function(e) list())
    if (!length(qs)) return()
    clamped <- character(0)
    num <- function(key, lo, hi, step = NULL) {
      raw <- qs[[key]]
      if (is.null(raw)) return(NULL)
      v <- suppressWarnings(as.numeric(raw))
      if (is.na(v) || !is.finite(v)) return(NULL)          # "banana" -> ignore, not error
      w <- min(max(v, lo), hi)
      if (!is.null(step)) w <- round(w / step) * step
      if (abs(w - v) > 1e-9) clamped <<- c(clamped, sprintf("%s %s→%s", key, v, w))
      w
    }
    set <- function(id, v, fn = updateSliderInput) if (!is.null(v)) fn(session, id, value = v)

    set("N", num("N", 10, 300, 5))
    set("D", num("D", 3, 60, 1))
    # `lambda` accepted as an alias so the deep-link contract reads naturally
    set("lambda_bar", num("lambda_bar", 0.5, 8, 0.5) %||% num("lambda", 0.5, 8, 0.5))
    set("cv", num("cv", 0, 1.5, 0.1))
    set("compliance", num("compliance", 0.3, 1, 0.05))
    set("decay", num("decay", 0, 0.1, 0.01))
    set("phi", num("phi", 0, 0.7, 0.05))
    # `eff` is the effect of interest for the selected model; `beta1` is accepted
    # as an alias for the common within-person-effect case.
    set("eff", num("eff", 0.05, 1.0, 0.05) %||% num("beta1", 0.05, 1.0, 0.05))
    set("cap", num("cap", 0, 50, 1), updateNumericInput)

    # model = one of Lafit's 11 (see models.R); lets a platform deep-link a
    # specific research question. Sets the question radios and the server truth.
    m <- suppressWarnings(as.integer(qs[["model"]] %||% NA))
    if (length(m) == 1L && !is.na(m) && as.character(m) %in% names(MODELS))
      set_model_widgets(m)
    if (!is.null(qs$target) && !is.na(suppressWarnings(as.numeric(qs$target))))
      updateSliderInput(session, "target_power",
                        value = min(max(as.numeric(qs$target), 0.5), 0.99))

    # trigger_link is an enum: snap to the nearest legal level, never clamp
    tl <- suppressWarnings(as.numeric(qs[["trigger_link"]] %||% qs[["trigger"]] %||% NA))
    if (!is.na(tl))
      updateSelectInput(session, "trigger_link",
                        selected = c("0", "0.75")[which.min(abs(tl - c(0, 0.75)))])
    # design=fixed lets a platform deep-link a time-contingent schedule (SMAAT's
    # fixed / random / interval types), where lambda is the PLANNED prompts per
    # day. Without this the count would be modelled as random — wrong design.
    if (!is.null(qs$design) && qs$design %in% c("poisson", "fixed", "triggered")) {
      dt <- if (identical(qs$design, "triggered")) "poisson" else qs$design
      updateRadioButtons(session, "design_type", selected = dt)
    }
    if (!is.null(qs$mode) && qs$mode %in% c("single", "curve"))
      updateRadioButtons(session, "mode", selected = qs$mode)
    if (!is.null(qs$power_def) && qs$power_def %in% c("all", "converged"))
      updateRadioButtons(session, "power_def", selected = qs$power_def)

    # return = the URL to hand the planned N back to (a SMAAT study page). Guarded
    # to https SMAAT hosts only, so a crafted deep-link cannot turn the "send N"
    # button into an open redirect to an arbitrary site.
    if (!is.null(qs$return) &&
        grepl("^https://([a-z0-9-]+\\.)*smaat\\.eu(/|\\?|$)", qs$return, ignore.case = TRUE))
      return_url(qs$return)

    if (length(clamped))
      showNotification(paste("Adjusted to the allowed range:", paste(clamped, collapse = "; ")),
                       type = "warning", duration = 8)
  })
}

shinyApp(ui, server)
