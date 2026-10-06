devtools::load_all(quiet = TRUE)
set.seed(5102026)
n <- 500L
p <- 33L
f <- matrix(rnorm(n * 2L), ncol = 2L)
data <- as.data.frame(sapply(seq_len(p), function(i) {
  latent <- .7 * f[, if (i <= 17L) 1L else 2L] + rnorm(n, sd = .7)
  as.integer(cut(latent, c(-Inf, -1, -.3, .3, 1, Inf)))
}))
names(data) <- paste0("M", seq_len(p))
engines <- lapply(c("artifacts/correlation-cache-before/R", "R"), function(path) {
  env <- new.env(parent = asNamespace("ItemRest"))
  for (file in list.files(path, pattern = "[.]R$", full.names = TRUE)) sys.source(file, env)
  # Fix the explored sets across both versions; the EFA itself is not mocked.
  env$identify_problem_items <- function(efa_res, ...) {
    problems <- env$empty_problem_items()
    problems$low_loading <- intersect(c("M1", "M2", "M3"), rownames(efa_res$loadings))
    problems
  }
  env$calls <- 0L
  original_cor <- env$cor_matrix_custom
  env$cor_matrix_custom <- function(...) {
    env$calls <- env$calls + 1L
    original_cor(...)
  }
  env
})
names(engines) <- c("before", "after")
fit <- function(engine) engine$itemrest(data, cor_method = "polychoric", n_factors = 2,
  seed = 5102026, verbose = FALSE)
results <- lapply(engines, fit)
columns <- names(results$before$removal_summary)
stopifnot(isTRUE(all.equal(results$before$removal_summary, results$after$removal_summary[, columns],
  tolerance = 1e-7)))
stopifnot(engines$before$calls == 8L, engines$after$calls == 1L)
cat("500 observations, 33 five-category items, 8 retained sets; alpha and omega enabled.\n")
cat("Same full-search removal summary within tolerance 1e-7.\n")
cat("Correlation calculations per search: before =", engines$before$calls,
  "; after =", engines$after$calls, "\n")
times <- lapply(engines, function(engine) replicate(3, system.time(fit(engine))[["elapsed"]]))
cat("Before seconds:", times$before, "\n")
cat("After seconds:", times$after, "\n")
cat("Median speedup:", median(times$before) / median(times$after), "\n")
compact <- results$after
full <- engines$after$itemrest(data, cor_method = "polychoric", n_factors = 2,
  seed = 5102026, verbose = FALSE, store_fits = TRUE)
cat("Stored result bytes, full:", as.numeric(object.size(full)), "; compact:",
  as.numeric(object.size(compact)), "\n")
saveRDS(list(times = times, before = results$before, after = results$after),
  "artifacts/correlation-cache-benchmark.rds")
