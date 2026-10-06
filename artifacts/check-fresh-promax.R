devtools::load_all(quiet = TRUE)
set.seed(29)
factors <- matrix(rnorm(800), ncol = 2)
data <- as.data.frame(sapply(rep(1:2, each = 4), function(i) factors[, i] + rnorm(400, sd = .4)))
first <- itemrest(data, n_factors = 2, rotate = "promax", reliability = "none", verbose = FALSE, seed = 17)
second <- itemrest(data, n_factors = 2, rotate = "promax", reliability = "none", verbose = FALSE, seed = 17)
stopifnot(identical(first$candidate_solutions, second$candidate_solutions),
  nrow(first$candidate_solutions) == 1L,
  identical(first$initial_efa$loadings, second$initial_efa$loadings))
cat("Fresh R process: promax candidates =", nrow(first$candidate_solutions),
  "and", nrow(second$candidate_solutions), "; tables and loadings identical.\n")
