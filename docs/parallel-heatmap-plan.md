# Parallel Heatmap Rendering – Development Notes

This document captures the considerations and proposed approach for bringing back
parallel heatmap rendering (primarily for `heatmap_gene` and combined enrichment
plots) without reintroducing the instability that was observed with the original
`future_map(multicore = TRUE)` experiment.

## Pain Points Observed Previously

- **Forking graphics devices**: `future::multicore` forks the current R session,
  which inherits open graphics devices and file handles. `ComplexHeatmap`/Cairo
  does not play nicely with concurrent writes from forked workers, leading to
  intermittent hangs and truncated PDFs.
- **Heavy global objects**: Shipping the full GCT object and configuration into
  each worker caused large serialization costs and excessive memory usage.
- **Shared output/logging**: All workers attempted to write via the same
  `save_func` and `log_msg` instances. Without coordination, this creates race
  conditions and potential deadlocks.

## Proposed Strategy

1. **Use `multisession` futures**
   - Launch worker R sessions via PSOCK instead of forked processes. This avoids
     inherited graphics devices and tends to be more stable across platforms.
   - Provide an escape hatch (`parallel = FALSE`) and surface the choice via
     configuration (`params$heatmap_gene$parallel`, `params$enplot$parallel`).
   - New helper scaffolding lives in `R/parallel_heatmap.R` (`resolve_multisession_workers`,
     `with_multisession_plan`, `run_multisession_jobs`) and can be wired into the
     plotting code once we are ready.

2. **Reduce payload size**
   - Replace global GCT passing with lightweight job descriptors. Each worker
     re-opens/reads only the necessary subset (e.g., by reusing `cmapR::subset_gct`
     on-demand or on a pre-saved feather/h5 slice).
   - Cache repeated data locally inside the worker (e.g., memoise metadata lookups).

3. **Chunk work at a higher level**
   - Parallelise across comparisons (or batches of pathways) rather than per
     pathway row. This keeps the number of workers modest and improves cache hits.
   - Optional: batch pathways into groups of N (e.g., 5–10) to amortise reloading.

4. **Safer output & logging**
   - Wrap `save_func` inside a worker-safe adapter that writes to a temporary file
     before a final `file.rename` into place. This prevents partially written PDFs
     when two workers resolve the same filename.
   - Give each worker its own log prefix (or disable logging inside workers) to
     avoid interleaving.

5. **Worker lifecycle & resources**
   - Cap workers via `min(availableCores() - 1, length(job_list))`.
   - Explicitly set `future.globals.maxSize` to guard against runaway memory use.
   - Ensure workers shut down cleanly on error by running jobs inside `withCallingHandlers`
     and collecting results/errors explicitly.

6. **Testing & Roll-out**
   - Add regression tests that render a handful of heatmaps with parallel mode on
     (2 workers) and compare output hashes against serial rendering.
   - Run manual stress tests on a realistic ssGSEA dataset to confirm stability and
     measure the observed speed-up.
   - Document the feature as “experimental” until we are satisfied with real-world
     stability; remind users to keep `parallel = FALSE` on systems where PSOCK
     workers are slow to spawn (e.g., Windows with tight anti-virus policies).

## Open Questions

- Should we persist sub-GCT matrices to disk (temp `.rds` or `.rda`) to avoid
  repeated slicing? This could make worker startup faster but introduces storage
  churn.
- Do we need a watchdog to abort jobs that exceed a time limit (e.g., spinning
  ComplexHeatmap) to prevent the original “hang” behaviour?
- Is it worth exposing a per-module parallel toggle (heatmap vs enplot) or keep a
  single shared knob? Current config leans toward module-specific flags.

These items should be resolved during implementation/design reviews before we ship
parallel heatmap support again.
