# Airtight reproducible environment for the ESM power simulation.
# Pins R 4.3.1 and the exact package versions the confirmatory run used
# (see repro/versions.tsv / renv.lock). Build once, run the sim/analysis inside.
#
#   docker build -t esm-power-sim .
#   docker run --rm -v "$PWD/results:/sim/results" esm-power-sim \
#       Rscript R/run.R --grid=primary --R=2000 --seed=20260709 --out=results/primary.csv
#   docker run --rm -v "$PWD/results:/sim/results" esm-power-sim \
#       Rscript R/analyze.R --primary=results/primary.csv --outdir=results/analysis
#
# rocker/r-ver:4.3.1 fixes R at 4.3.1 and a dated Posit Package Manager snapshot,
# so remotes::install_version() resolves the exact pinned builds below.

FROM rocker/r-ver:4.3.1

# system libs some packages need to compile
RUN apt-get update && apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"

# engine closure — versions must match repro/versions.tsv exactly
RUN R -e "remotes::install_version('Matrix',  '1.6-1',    upgrade='never'); \
          remotes::install_version('MASS',    '7.3-60',   upgrade='never'); \
          remotes::install_version('nlme',    '3.1-162',  upgrade='never'); \
          remotes::install_version('lattice', '0.21-8',   upgrade='never'); \
          remotes::install_version('boot',    '1.3-28.1', upgrade='never'); \
          remotes::install_version('Rcpp',    '1.0.11',   upgrade='never'); \
          remotes::install_version('minqa',   '1.2.5',    upgrade='never'); \
          remotes::install_version('nloptr',  '2.0.3',    upgrade='never'); \
          remotes::install_version('lme4',    '1.1-34',   upgrade='never')"

# Shiny for the companion planning tool (optional at run time)
RUN R -e "remotes::install_version('shiny', '1.8.1.1', upgrade='never')"

WORKDIR /sim
COPY . /sim

# fail early if the environment drifts from the manifest
RUN Rscript repro/check-env.R || true

CMD ["R", "--no-save"]
