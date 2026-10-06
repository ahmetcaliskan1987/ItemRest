check_itemrest_result <- function(x) {
  if (!inherits(x, "itemrest_result") || is.null(x$solution_details))
    stop("x must be a current itemrest_result with solution_details.", call. = FALSE)
}

select_validation_ids <- function(x, solution_ids) {
  if (is.null(solution_ids)) solution_ids <- unique(c("S00001", x$candidate_solutions$Solution_ID))
  if (!is.character(solution_ids) || anyNA(solution_ids) || !length(solution_ids) ||
      any(!solution_ids %in% names(x$solution_details)))
    stop("solution_ids must contain known Solution_ID values.", call. = FALSE)
  unique(solution_ids)
}

validation_cfa <- function(detail, data, settings, ordered_items) {
  if (is.null(detail$assessment)) stop("The discovery solution has no factor assignment.", call. = FALSE)
  assignments <- detail$assessment$assignment
  items <- detail$remaining_items
  assignments <- assignments[items]
  if (anyNA(assignments) || any(tabulate(assignments, nbins = settings$n_factors) < 2L))
    stop("The discovery CFA specification needs at least two assigned items per factor.", call. = FALSE)
  safe_names <- stats::setNames(paste0("V", seq_along(items)), items)
  syntax <- vapply(seq_len(settings$n_factors), function(factor)
    paste0("F", factor, " =~ ", paste(safe_names[items[assignments == factor]], collapse = " + ")),
    character(1))
  model <- paste(syntax, collapse = "\n")
  names(data) <- unname(safe_names[items])
  ordered <- unname(safe_names[intersect(ordered_items, items)])
  estimator <- if (length(ordered)) "WLSMV" else "MLR"
  missing <- if (settings$missing == "pairwise") {
    if (length(ordered)) "pairwise" else "fiml"
  } else "listwise"
  fit <- lavaan::cfa(model, data = data, std.lv = TRUE, estimator = estimator,
                     ordered = if (length(ordered)) ordered else NULL, missing = missing)
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  post_check <- converged && isTRUE(lavaan::lavInspect(fit, "post.check"))
  phi <- if (converged) lavaan::lavInspect(fit, "cor.lv") else NULL
  max_phi <- if (!is.null(phi) && nrow(phi) > 1L) max(abs(phi[lower.tri(phi)])) else 0
  measures <- if (converged) lavaan::fitMeasures(fit) else numeric()
  get_measure <- function(name) if (name %in% names(measures)) unname(measures[name]) else NA_real_
  list(fit = fit, syntax = model, item_map = safe_names, estimator = estimator,
       missing = missing, ordered_items = intersect(ordered_items, items),
       summary = data.frame(
         Converged = converged, Admissible = post_check,
         Max_Factor_Correlation = max_phi,
         Review_Required = max_phi >= settings$max_factor_correlation,
         N_Obs = sum(lavaan::lavInspect(fit, "nobs")),
         CFI = get_measure("cfi"), TLI = get_measure("tli"),
         RMSEA = get_measure("rmsea"), SRMR = get_measure("srmr"),
         CFI_Robust = get_measure("cfi.robust"), TLI_Robust = get_measure("tli.robust"),
         RMSEA_Robust = get_measure("rmsea.robust"),
         Estimator = estimator, Missing_Method = missing, stringsAsFactors = FALSE
       ))
}

#' Reevaluate Prespecified Candidate Sets on Validation Data
#'
#' @description
#' Refits discovery item sets on user-supplied validation observations, without
#' selecting or removing additional items. The user is responsible for supplying
#' independent observations. EFA replication and optional simple-structure CFA
#' are distinct checks; neither automatically establishes validity.
#' @param x An itemrest_result from discovery data.
#' @param data Numeric validation data containing every discovery item.
#' @param solution_ids Solution_ID values. NULL evaluates candidates plus baseline.
#' @param method "efa" (default) or "cfa". CFA requires optional lavaan and fixes
#'   each retained item's primary factor to its discovery assignment; all other
#'   cross-loadings are constrained to zero. Factors remain correlated.
#' @param ordered_items For CFA, names of ordinal items. NULL uses integer-valued
#'   discovery items identified under cor_method = "polychoric". A character()
#'   value explicitly requests continuous treatment. Ordinal CFA uses WLSMV;
#'   continuous CFA uses MLR. Pairwise EFA uses pair counts; continuous CFA with
#'   missing = "pairwise" uses FIML, explicitly labelled in the output.
#' @param seed Integer seed; NULL uses the local date in DDMMYYYY format.
#' @return A list containing validation_summary, solution_details, data_summary,
#'   source_solution_ids, method, seed, and independence_note. CFA fit indices
#'   are descriptive and are not converted to an automatic pass/fail verdict.
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' f <- rnorm(300)
#' d <- as.data.frame(replicate(4, f + rnorm(300)))
#' discovery <- itemrest(d[1:150, ], n_factors = 1, verbose = FALSE)
#' check <- itemrest_validate(discovery, d[151:300, ])
#' }
itemrest_validate <- function(x, data, solution_ids = NULL, method = c("efa", "cfa"),
                              ordered_items = NULL, seed = NULL) {
  check_itemrest_result(x)
  method <- match.arg(method)
  ids <- select_validation_ids(x, solution_ids)
  if (method == "cfa" && !requireNamespace("lavaan", quietly = TRUE))
    stop("Install the optional 'lavaan' package for CFA validation.", call. = FALSE)
  if (!(is.data.frame(data) || is.matrix(data))) stop("data must be a numeric data.frame or matrix.", call. = FALSE)
  data <- as.data.frame(data, check.names = FALSE)
  items <- x$provenance$item_names
  if (any(!items %in% names(data))) stop("Validation data must include all discovery items.", call. = FALSE)
  prepared <- prepare_itemrest_data(data[, items, drop = FALSE], x$settings$missing)
  identical_sample <- identical(data_fingerprint(prepared$data), x$provenance$data_fingerprint)
  if (identical_sample) warning("Validation data match the discovery data; this is not independent validation.", call. = FALSE)
  data <- apply_item_keys(prepared$data, x$settings$keys)$data
  if (is.null(ordered_items)) ordered_items <- x$provenance$ordinal_items
  if (!is.character(ordered_items) || anyNA(ordered_items) || any(!ordered_items %in% items))
    stop("ordered_items must contain known item names.", call. = FALSE)
  seed_info <- resolve_itemrest_seed(seed)
  with_itemrest_seed(seed_info$value, {
    rows <- details <- list()
    options <- x$settings[c("missing", "min_items_per_factor", "max_factor_correlation", "pd_action", "reliability")]
    for (id in ids) {
      source <- x$solution_details[[id]]
      retained <- source$remaining_items
      subset <- data[, retained, drop = FALSE]
      if (method == "efa") {
        evaluated <- evaluate_item_set(subset, x$settings$n_factors, x$settings$cor_method,
          x$settings$extract, x$settings$rotate, x$settings$min_loading, x$settings$loading_diff, options)
        state <- list(removed = source$removed_items, step = character(), iteration = NA_integer_,
                      min_loading = x$settings$min_loading)
        row <- solution_row(id, state, items, evaluated, subset)
        row$Baseline <- id == "S00001"
        row$Discovery_Candidate <- x$removal_summary$Candidate[match(id, x$removal_summary$Solution_ID)]
        details[[id]] <- evaluated
        if (!is.null(source$analysis$loadings) && !is.null(evaluated$out$loadings)) {
          congruence <- capture_analysis(psych::factor.congruence(source$analysis$loadings,
                                                                 evaluated$out$loadings))
          details[[id]]$factor_congruence <- congruence$value
        }
      } else {
        fitted <- capture_analysis(validation_cfa(source, subset, x$settings, ordered_items))
        if (!is.null(fitted$error)) {
          row <- data.frame(Solution_ID = id, Baseline = id == "S00001", Analysis_Status = "failed",
            Error_Message = fitted$error, Warnings = paste(fitted$warnings, collapse = "; "),
            Converged = FALSE, Admissible = NA, Max_Factor_Correlation = NA_real_,
            Review_Required = NA, N_Obs = NA_real_, CFI = NA_real_, TLI = NA_real_, RMSEA = NA_real_,
            SRMR = NA_real_, CFI_Robust = NA_real_, TLI_Robust = NA_real_, RMSEA_Robust = NA_real_,
            Estimator = NA_character_, Missing_Method = NA_character_, stringsAsFactors = FALSE)
        } else {
          row <- cbind(data.frame(Solution_ID = id, Baseline = id == "S00001",
            Analysis_Status = if (fitted$value$summary$Admissible) "ok" else "inadmissible",
            Error_Message = "", Warnings = paste(fitted$warnings, collapse = "; "),
            stringsAsFactors = FALSE), fitted$value$summary)
          if (length(fitted$warnings)) row$Review_Required <- TRUE
        }
        details[[id]] <- fitted
      }
      rows[[id]] <- row
    }
    list(validation_summary = do.call(rbind, rows), solution_details = details,
         data_summary = prepared$summary, source_solution_ids = ids, method = method,
         seed = seed_info, independence_note = if (identical_sample) "same_as_discovery" else
           "Independence is the user's responsibility; no reselection performed.")
  })
}

#' Split Observations into Discovery and Validation Samples
#' @param data Numeric data.frame or matrix.
#' @param train_fraction Fraction assigned to discovery, default 0.70.
#' @param seed Integer seed; NULL uses the local date in DDMMYYYY format.
#' @param validation_method "efa" or "cfa".
#' @param ... Arguments to itemrest, excluding data and seed.
#' @return A list with discovery, validation, disjoint discovery_rows and
#'   validation_rows, and seed. Splitting precedes missing-data handling.
#' @export
itemrest_split <- function(data, train_fraction = 0.70, seed = NULL,
                           validation_method = c("efa", "cfa"), ...) {
  check_scalar(train_fraction, "train_fraction", 0.01, 0.99)
  validation_method <- match.arg(validation_method)
  if (!(is.data.frame(data) || is.matrix(data))) stop("data must be a numeric data.frame or matrix.", call. = FALSE)
  n <- nrow(data)
  n_train <- floor(n * train_fraction)
  if (n_train < 3L || n - n_train < 3L) stop("Each split needs at least three observations.", call. = FALSE)
  seed_info <- resolve_itemrest_seed(seed)
  with_itemrest_seed(seed_info$value, {
    discovery_rows <- sort(sample.int(n, n_train))
    validation_rows <- setdiff(seq_len(n), discovery_rows)
    discovery <- itemrest(data[discovery_rows, , drop = FALSE], seed = seed_info$value, ...)
    discovery$settings$seed_label <- seed_info$label
    discovery$settings$seed_source <- paste0("split_", seed_info$source)
    discovery$provenance$seed_label <- seed_info$label
    discovery$provenance$seed_source <- paste0("split_", seed_info$source)
    validation <- itemrest_validate(discovery, data[validation_rows, , drop = FALSE],
                                   method = validation_method, seed = seed_info$value)
    validation$seed <- seed_info
    list(discovery = discovery, validation = validation, discovery_rows = discovery_rows,
         validation_rows = validation_rows, seed = seed_info)
  })
}
