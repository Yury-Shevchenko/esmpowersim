# =============================================================================
# inject-splash.R — add a branded first-load splash to the exported index.html.
#
#   Rscript tools/inject-splash.R           (after shinylive::export("app","docs"))
#
# WHY. shinylive boots a whole R + lme4 stack in WebAssembly before app.R can
# draw anything. On a first visit that is a ~15-30 s download + compile, during
# which shinylive shows only a bare spinner — so the tool reads as "slow to even
# open". This overlays index.html with a titled splash that explains the one-time
# load, streams progress messages, and previews what the tool does, then removes
# itself the instant the app connects (app.R postMessages 'esm-planner-ready' on
# shiny:connected). A timeout and click-to-dismiss guarantee the user is never
# trapped behind it if that message is missed.
#
# It PATCHES the shinylive output rather than templating it, so it is independent
# of the shinylive version — but it asserts each anchor is present, so a future
# template change fails the build loudly instead of silently shipping no splash.
# =============================================================================

f <- "docs/index.html"
if (!file.exists(f)) stop("inject-splash: ", f, " not found — run shinylive::export first")
html <- paste(readLines(f, warn = FALSE), collapse = "\n")

# Idempotent: a fresh shinylive export is unpatched, but a re-run (or a local
# double build) must not fail or double-inject.
if (grepl('id="esm-splash"', html, fixed = TRUE)) {
  cat("inject-splash: splash already present in", f, "- skipping\n")
  quit(status = 0)
}

must_replace <- function(x, pattern, replacement, fixed = FALSE) {
  out <- sub(pattern, replacement, x, fixed = fixed)
  if (identical(out, x)) stop("inject-splash: anchor not found: ", pattern)
  out
}

# 1. a real, descriptive tab title (was "Shiny App")
html <- must_replace(html, "<title>[^<]*</title>",
                     "<title>Experience-Sampling Power Planner</title>")

splash_css <- '<style id="esm-splash-style">
#esm-splash{position:fixed;inset:0;z-index:99999;display:flex;align-items:center;justify-content:center;
  background:#f4f7fb;font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
  transition:opacity .5s ease;padding:24px;box-sizing:border-box}
#esm-splash.esm-hide{opacity:0;pointer-events:none}
.esm-splash-card{max-width:560px;text-align:center;color:#33475b}
.esm-splash-title{font-size:24px;font-weight:800;color:#12314f;margin-bottom:6px}
.esm-splash-sub{font-size:15px;color:#5a6b7d;margin-bottom:26px}
.esm-splash-spinner{width:44px;height:44px;border:4px solid #d3e0ee;border-top-color:#2f6fb0;
  border-radius:50%;margin:0 auto 18px;animation:esm-spin .9s linear infinite}
@keyframes esm-spin{to{transform:rotate(360deg)}}
.esm-splash-msg{font-size:16px;font-weight:600;color:#1b3a5b;margin-bottom:14px}
.esm-splash-note{font-size:13px;line-height:1.5;color:#6a7889;margin:0 auto 20px;max-width:470px}
.esm-splash-preview{list-style:none;padding:0;margin:0 auto;max-width:430px;text-align:left;
  font-size:13px;color:#4a5a6b}
.esm-splash-preview li{position:relative;padding:5px 0 5px 22px}
.esm-splash-preview li:before{content:"\\203A";position:absolute;left:6px;color:#2f6fb0;font-weight:700}
.esm-splash-dismiss{display:none;margin-top:22px;font-size:13px;color:#2f6fb0;font-weight:600}
</style>'

splash_html <- '<div id="esm-splash" role="status" aria-live="polite">
  <div class="esm-splash-card">
    <div class="esm-splash-title">Experience-Sampling Power Planner</div>
    <div class="esm-splash-sub">Power, precision &amp; feasibility for event- and location-triggered ESM/EMA studies</div>
    <div class="esm-splash-spinner"></div>
    <div class="esm-splash-msg" id="esm-splash-msg">Starting the simulation engine…</div>
    <div class="esm-splash-note">The full statistical engine (R + lme4) runs entirely in your browser — nothing you enter is sent to a server. The first visit downloads it once (about 15–30 seconds on a typical connection); after that it starts instantly.</div>
    <ul class="esm-splash-preview">
      <li>Describe your research question — the planner picks the right multilevel model.</li>
      <li>Read power straight from a 2,000-replication grid, or simulate your exact design live.</li>
      <li>Solve for the sample size you need — even when the number of prompts per person is random.</li>
    </ul>
    <div class="esm-splash-dismiss" id="esm-splash-dismiss">Taking a while? Click anywhere to continue →</div>
  </div>
</div>
<script>
(function(){
  var splash = document.getElementById("esm-splash");
  if (!splash) return;
  var msg = document.getElementById("esm-splash-msg");
  var done = false;
  function hide(){ if(done) return; done = true; splash.classList.add("esm-hide");
    setTimeout(function(){ if(splash && splash.parentNode) splash.parentNode.removeChild(splash); }, 600); }
  // Precise signal: app.R postMessages this on shiny:connected.
  window.addEventListener("message", function(e){ if(e && e.data === "esm-planner-ready") hide(); });
  // Keep a long download feeling alive.
  var steps = [[6000,"Downloading the statistical engine…"],
               [16000,"Loading the multilevel modelling libraries (lme4)…"],
               [30000,"Almost there — warming up the simulator…"],
               [50000,"Still loading — a slow connection can take a little longer…"]];
  steps.forEach(function(s){ setTimeout(function(){ if(!done && msg) msg.textContent = s[1]; }, s[0]); });
  // Never trap the user: offer manual dismissal, then force it.
  setTimeout(function(){ if(done) return; var d=document.getElementById("esm-splash-dismiss");
    if(d) d.style.display="block"; splash.style.cursor="pointer"; splash.addEventListener("click", hide); }, 22000);
  setTimeout(hide, 180000);
})();
</script>'

html <- must_replace(html, "</head>", paste0(splash_css, "\n</head>"), fixed = TRUE)
html <- must_replace(html, "</body>", paste0(splash_html, "\n</body>"), fixed = TRUE)

writeLines(html, f)
cat("inject-splash: patched", f, "(title + first-load splash)\n")
