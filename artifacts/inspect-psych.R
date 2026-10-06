cat("psych version", as.character(packageVersion("psych")), "\n")
src <- deparse(psych::fa.parallel)
indices <- which(grepl("fa.values", src, fixed = TRUE) | grepl("fa.sim", src, fixed = TRUE) |
  grepl("values <-", src, fixed = TRUE) | grepl("nfact", src, fixed = TRUE))
indices <- sort(unique(unlist(lapply(indices, function(i) seq.int(max(1L, i - 2L), min(length(src), i + 3L))))))
cat(paste(src[indices], collapse = "\n"), "\n")
cat("clue installed:", requireNamespace("clue", quietly = TRUE), "\n")
