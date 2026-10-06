set.seed(5102026)
n <- 500L
p <- 33L
f <- rnorm(n)
dat <- as.data.frame(replicate(p, as.integer(cut(0.6 * f + rnorm(n),
  breaks = c(-Inf, -1, -0.3, 0.3, 1, Inf)))))
names(dat) <- paste0("M", seq_len(p))
qfun <- function() qgraph::cor_auto(dat, missing = "listwise", forcePD = FALSE, verbose = FALSE)
efun <- function() EGAnet::auto.correlate(dat, corr = "pearson", na.data = "listwise",
  forcePD = FALSE, empty.method = "none", empty.value = "none")
q <- qfun()
e <- efun()
cat("Versions: qgraph", as.character(packageVersion("qgraph")),
  "EGAnet", as.character(packageVersion("EGAnet")), "\n")
cat("500 complete rows, 33 items, 5 categories; no empty-cell correction or PD repair\n")
cat("qgraph elapsed seconds:", replicate(3, system.time(qfun())[["elapsed"]]), "\n")
cat("EGAnet elapsed seconds:", replicate(3, system.time(efun())[["elapsed"]]), "\n")
cat("Maximum absolute matrix difference:", max(abs(q - e)), "\n")
devtools::load_all(quiet = TRUE)
source("tests/testthat/helper-fixtures.R")
result <- itemrest(fixture_data(p = 3), n_factors = 2, verbose = FALSE)
slogan <- "Let algorithms be your compass, not your captain."
for (report in c("candidates", "all")) {
  printed <- capture.output(print(result, report = report))
  stopifnot(identical(tail(printed, 1L), slogan))
}
cat("Slogan is the final line for both printed report modes.\n")
devtools::test()
