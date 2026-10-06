Sys.setenv(
  `_R_CHECK_CRAN_INCOMING_REMOTE_` = "FALSE",
  `_R_CHECK_CRAN_INCOMING_` = "FALSE",
  `_R_CHECK_FORCE_SUGGESTS_` = "FALSE",
  `_R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_` = "FALSE",
  NOT_CRAN = "true"
)
archive <- pkgbuild::build(path = ".", dest_path = "artifacts", vignettes = TRUE, manual = FALSE)
check_dir <- file.path(tempdir(), "ItemRest-correlation-cache-check")
result <- rcmdcheck::rcmdcheck(path = archive, args = c("--no-manual", "--as-cran"),
  check_dir = check_dir, error_on = "never", quiet = FALSE)
file.copy(file.path(check_dir, "ItemRest.Rcheck", "00check.log"), "artifacts/00check.log", overwrite = TRUE)
print(result)
stopifnot(length(result$errors) == 0L, length(result$warnings) == 0L, length(result$notes) == 0L)
