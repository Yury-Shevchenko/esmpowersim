# =============================================================================
# seeds.R — Deterministic, reproducible seeding.
#
# One master seed -> a distinct, fixed seed for every (cell, replication) pair.
# No wall-clock or runtime randomness enters seeding, so any dataset is exactly
# regenerable from (master_seed, cell_id, rep). Prereg §3-M.
# =============================================================================

# integer seed for a given cell + replication, derived from the master seed
rep_seed <- function(master_seed, cell_id, rep) {
  # large, coprime-ish multipliers keep streams from colliding across cells/reps
  s <- (master_seed %% 2147483L) * 1L +
       (cell_id     %% 100000L) * 20011L +
       (rep         %% 100000L) * 101L
  as.integer(s %% .Machine$integer.max)
}
