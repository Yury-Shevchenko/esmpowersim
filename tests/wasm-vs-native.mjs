// =============================================================================
// wasm-vs-native.mjs — the test that licenses the tool's central claim.
//
// The planner tells researchers it runs the "same engine as the preregistered
// simulation study". That is a property to TEST, not to assert:
//   * webR serves whatever is current in repo.r-wasm.org — today lme4 2.0-1 on
//     R 4.6 — and the version cannot be pinned;
//   * so a future release could silently break the agreement.
// If this test fails, the manuscript's wording changes, not just the code.
//
//   npm ci && npm run test:repro
//
// WHY THE REFERENCE IS A FIXTURE, NOT A LIVE NATIVE RUN
// -----------------------------------------------------
// The claim is about the PAPER's engine, not about whatever R is installed on
// the build machine. Those are different things, and measurably so: at the
// sparse corner, native R 4.6.1 / lme4 2.0.1 gives conv_rate 0.560 where the
// paper's R 4.3.1 / lme4 1.1-34 gives 0.570 — one marginal fit in 100 flips.
// The wasm build (lme4 2.0-1) reproduces the PAPER's 0.570, not native-4.6.1's
// 0.560, so the divergence tracks the numerical environment rather than the
// package version. A live native run is therefore a moving yardstick; the
// recorded fixture is a fixed one. See tests/fixtures/paper-engine.json.
//
// The live native run is still executed, but only as INFORMATION: it detects
// drift in the local environment without failing the build.
// =============================================================================
import { WebR } from 'webr';
import { readFileSync } from 'fs';
import { execFileSync } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const FIX = JSON.parse(readFileSync(`${ROOT}/tests/fixtures/paper-engine.json`, 'utf8'));
const METRICS = ['power', 'mcse_power', 'conv_rate', 'mean_n'];
const TOL = 1e-6; // six decimals — the precision the tool reports at

const cellR = (d) => `data.frame(${Object.entries(d)
  .map(([k, v]) => `${k}=${v === 'Inf' ? 'Inf' : v}`).join(',')})`;
const runR = (c) =>
  `r <- run_cell(${cellR(c.design)}, ${FIX.seed}L, ${c.R}L, cell_id=1L); ` +
  `sprintf("%.10f|%.10f|%.10f|%.10f", r$power, r$mcse_power, r$conv_rate, r$mean_n)`;

console.log(`reference: the paper's environment — R ${FIX.environment.R}, lme4 ${FIX.environment.lme4}, ` +
            `Matrix ${FIX.environment.Matrix} (recorded fixture)`);

// --- wasm: the build we actually ship ---------------------------------------
const webr = new WebR({ interactive: false });
await webr.init();
await webr.installPackages(['lme4', 'MASS', 'Matrix', 'nlme']);
console.log('wasm     :', await webr.evalRString(
  'paste(R.version.string, "| lme4", packageVersion("lme4"), "| Matrix", packageVersion("Matrix"))'));
await webr.FS.mkdir('/sim'); await webr.FS.mkdir('/sim/R');
for (const f of ['seeds.R', 'models.R', 'dgm.R', 'fit.R', 'performance.R'])
  await webr.FS.writeFile(`/sim/R/${f}`, new Uint8Array(readFileSync(`${ROOT}/R/${f}`)));
await webr.evalRVoid(`setwd("/sim"); suppressWarnings(suppressMessages(
  for (f in c("seeds","models","dgm","fit","performance")) source(file.path("R", paste0(f,".R")))))`);

const wasm = [];
for (const c of FIX.cells)
  wasm.push((await webr.evalRString(`suppressWarnings(suppressMessages({ ${runR(c)} }))`))
    .split('|').map(Number));
await webr.close();

// --- native: informational only ---------------------------------------------
let native = null, nativeVer = '(unavailable)';
try {
  nativeVer = execFileSync('Rscript', ['-e',
    'cat(R.version.string, "| lme4", as.character(packageVersion("lme4")), "| Matrix", as.character(packageVersion("Matrix")))'],
    { cwd: ROOT, encoding: 'utf8' }).trim();
  const script = `suppressWarnings(suppressMessages({
      for (f in c("seeds","models","dgm","fit","performance")) source(file.path("R", paste0(f,".R")))
    }))\n` + FIX.cells.map((c) => `cat({${runR(c)}}, "\\n")`).join('\n');
  native = execFileSync('Rscript', ['-e', script], { cwd: ROOT, encoding: 'utf8' })
    .trim().split('\n').filter((l) => l.includes('|')).map((l) => l.split('|').map(Number));
} catch { /* informational only — never fail the build on this */ }
console.log('native   :', nativeVer, '(informational — not the reference)');

// --- compare wasm against the paper ------------------------------------------
let failed = 0, drift = 0;
console.log('\n' + '='.repeat(74));
FIX.cells.forEach((c, i) => {
  console.log(`\n${c.label}`);
  METRICS.forEach((m, k) => {
    const want = c.expected[m], got = wasm[i][k];
    const ok = Math.abs(want - got) <= TOL;
    if (!ok) failed++;
    let line = `  ${m.padEnd(11)} paper=${want.toFixed(6)}  wasm=${got.toFixed(6)}  ` +
               (ok ? '✓' : `✗ differs by ${Math.abs(want - got).toExponential(2)}`);
    if (native) {
      const nv = native[i][k];
      const nok = Math.abs(want - nv) <= TOL;
      if (!nok) drift++;
      line += `   | native=${nv.toFixed(6)}${nok ? '' : ' (drift)'}`;
    }
    console.log(line);
  });
});

console.log('\n' + '='.repeat(74));
if (drift)
  console.log(`note: the local native R differs from the paper's environment on ${drift} metric(s).
That is expected and NOT a failure — this machine is no longer running R ${FIX.environment.R}.
It is recorded because it is evidence about re-running the confirmatory grid on a newer R:
the marginal fits at the sparse corner do move.`);

if (failed) {
  console.error(`\n✗ ${failed} metric(s) diverged from the paper's engine beyond ${TOL}.
The tool claims "same engine as the preregistered study" (app/app.R, README.md).
That claim is now FALSE for the shipped wasm build. This is MANUSCRIPT-FACING:
fix the engine, pin the wasm packages, or change the wording — do not ignore it.`);
  process.exit(1);
}
console.log(`\n✓ the shipped wasm build reproduces the paper's engine to ${TOL} on every metric,
across a major version gap (lme4 ${FIX.environment.lme4} → 2.0-1). The "same engine as the
preregistered simulation study" claim holds for this build.`);
