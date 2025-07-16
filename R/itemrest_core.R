#' Check and Install Required Packages
#'
#' Installs and loads a package if not already installed.
#'
#' @param pkg A character string naming the package to load/install.
#'
#' @return Loads the requested package into the session.
#' @keywords internal
check_and_install <- function(pkg){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
required_packages <- c("psych", "GPArotation", "EFAtools", "qgraph")
sapply(required_packages, check_and_install)

#' Display Basic Descriptive Statistics for an Item Set
#'
#' Prints number of items, number of observations, and min/max values in the dataset.
#'
#' @param data A data.frame or matrix of item responses.
#'
#' @return Printed summary output to the console.
#' @keywords internal
descriptive_stats <- function(data) {
  cat("\n=== Descriptive Statistics ===\n")
  cat("\nNumber of items:", ncol(data), "\n")
  cat("Number of observations:", nrow(data), "\n")
  cat("Minimum value:", min(data, na.rm = TRUE), "\n")
  cat("Maximum value:", max(data, na.rm = TRUE), "\n\n")
}

#' Compute Correlation Matrix
#'
#' Computes a correlation matrix using either polychoric or Pearson correlations.
#'
#' @param data A data.frame or matrix of item responses.
#' @param method Correlation method: "polychoric" (default) or "pearson".
#'
#' @return A correlation matrix.
#' @keywords internal
cor_matrix_custom <- function(data, method = "polychoric") {
  if (method == "polychoric") {
    cor_mat <- suppressMessages(qgraph::cor_auto(data))
  } else {
    cor_mat <- cor(data, use = "pairwise.complete.obs", method = method)
  }
  return(cor_mat)
}

#' Determine Number of Factors via Parallel Analysis
#'
#' Uses parallel analysis to estimate the optimal number of factors for EFA.
#'
#' @param data A data.frame or matrix of item responses.
#' @param cor_method Correlation method to use ("pearson" or "polychoric").
#'
#' @return An integer indicating the suggested number of factors.
#' @keywords internal
determine_n_factors <- function(data, cor_method = "pearson") {
  cor_mat <- cor_matrix_custom(data, cor_method)
  n_obs <- nrow(data)
  fa_parallel <- suppressMessages(psych::fa.parallel(cor_mat, fa = "fa", n.iter = 20,
                                                     n.obs = n_obs,
                                                     show.legend = FALSE, main = NULL))
  return(fa_parallel$nfact)
}

#' Run Exploratory Factor Analysis (EFA)
#'
#' Performs EFA with specified number of factors, correlation method, extraction method, and rotation.
#' Also calculates Cronbach's alpha and explained variance.
#'
#' @param data A data.frame or matrix of item responses.
#' @param n_factors Number of factors to extract.
#' @param cor_method Correlation method: "polychoric" or "pearson".
#' @param extract Extraction method (e.g., "uls", "ml").
#' @param rotate Rotation method (e.g., "oblimin", "varimax").
#'
#' @return A list containing the EFA object, Cronbach's alpha, explained variance, and loading matrix.
#' @keywords internal
efa_custom <- function(data, n_factors = 1, cor_method = "polychoric", extract = "uls", rotate = "oblimin") {
  cor_mat <- cor_matrix_custom(data, cor_method)
  efa <- NULL
  invisible(capture.output({
    efa <- psych::fa(r = cor_mat, nfactors = n_factors, rotate = rotate, fm = extract)
  }))
  alpha_val <- psych::alpha(data)$total$raw_alpha
  loading <- efa$loadings[]
  explained_var <- sum(efa$Vaccounted["SS loadings", 1:n_factors]) / ncol(data)
  list(
    efa = efa,
    alpha = alpha_val,
    explained_var = explained_var,
    loadings = loading
  )
}

#' Sort Item IDs by Numeric Order
#'
#' Sorts item identifiers based on embedded numeric values.
#'
#' @param x A character vector of item names or IDs.
#'
#' @return A sorted character vector of item names.
#' @keywords internal
sort_item_ids <- function(x) {
  x[order(as.numeric(gsub("\\D", "", x)))]
}

#' Identify Problematic Items in EFA Loadings
#'
#' Detects items with cross-loadings (on 2 or more factors) and items with low loadings across all factors.
#'
#' @param efa_res Output from `efa_custom()`, containing factor loadings.
#' @param primary_cutoff Minimum acceptable primary factor loading.
#' @param secondary_max Maximum acceptable secondary loading.
#' @param diff_min Minimum difference between primary and secondary loadings.
#' @param low_loading_thresh Threshold for identifying low-loading items.
#'
#' @return A list with three character vectors: \code{cross_3}, \code{cross_2}, and \code{low_loading}.
#' @keywords internal
identify_problem_items <- function(efa_res, primary_cutoff = 0.40, secondary_max = 0.30, diff_min = 0.20, low_loading_thresh = 0.30) {
  loadings <- abs(efa_res$loadings)
  items <- rownames(loadings)
  cross_3 <- c()
  cross_2 <- c()
  low_loading <- c()

  for (i in 1:nrow(loadings)) {
    item_loads <- loadings[i, ]
    primary <- max(item_loads, na.rm = TRUE)

    if (ncol(loadings) < 2 || all(is.na(sort(item_loads, decreasing = TRUE)[2]))) {
      secondary_check <- FALSE
    } else {
      secondary <- sort(item_loads, decreasing = TRUE)[2]
      secondary_check <- !(primary >= primary_cutoff && secondary <= secondary_max && (primary - secondary) >= diff_min)
    }

    if (secondary_check) {
      if (sum(item_loads >= secondary_max, na.rm = TRUE) >= 3) {
        cross_3 <- c(cross_3, items[i])
      } else if (sum(item_loads >= secondary_max, na.rm = TRUE) == 2) {
        cross_2 <- c(cross_2, items[i])
      }
    }

    if (all(item_loads < low_loading_thresh, na.rm = TRUE)) {
      low_loading <- c(low_loading, items[i])
    }
  }

  list(cross_3 = cross_3, cross_2 = cross_2, low_loading = low_loading)
}

#' Generate All Non-Empty Combinations of Items
#'
#' Produces a list of all possible non-empty combinations of items for systematic testing.
#'
#' @param items A character vector of item names.
#'
#' @return A list of character vectors, each representing a combination of items.
#' @keywords internal
get_combinations <- function(items) {
  n <- length(items)
  if (n == 0) return(list(list()))
  all_combs <- unlist(lapply(1:n, function(i) combn(items, i, simplify = FALSE)), recursive = FALSE)
  return(c(list(list()), all_combs))
}

#' Sort Removal Strategy Labels for Summary Table
#'
#' Orders removal strategy labels based on numeric item identifiers.
#'
#' @param df A data.frame with a column named \code{Removed_Items}.
#'
#' @return A data.frame sorted by item ID order.
#' @keywords internal
sort_strategy_labels <- function(df) {
  if (!"Removed_Items" %in% names(df)) return(df)
  extract_numeric_keys <- function(x) {
    if (x == "None") return(rep(-Inf, 20))
    parts <- unlist(strsplit(x, "-"))
    nums <- as.numeric(gsub("\\D", "", parts))
    length(nums) <- 20
    nums[is.na(nums)] <- Inf
    return(nums)
  }
  key_matrix <- do.call(rbind, lapply(df$Removed_Items, extract_numeric_keys))
  df[do.call(order, as.data.frame(key_matrix)), ]
}

#' Evaluate Item Removal Strategies
#'
#' Tests all combinations of problematic items by removing them and re-running EFA.
#' Collects summary statistics such as explained variance, factor loadings, and reliability.
#'
#' @param data A data.frame of item responses.
#' @param base_items A character vector of all item names in the original set.
#' @param combs A list of item combinations to remove.
#' @param n_factors Number of factors to extract in EFA.
#' @param cor_method Correlation method: "pearson" or "polychoric".
#' @param extract Extraction method (e.g., "uls", "ml").
#' @param rotate Rotation method (e.g., "oblimin", "varimax").
#'
#' @return A list containing EFA results and a summary table for each tested strategy.
#' @keywords internal
test_removals <- function(data, base_items, combs, n_factors, cor_method, extract, rotate) {
  results <- list()
  summary_table <- data.frame()
  total <- length(combs)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in seq_along(combs)) {
    c <- combs[[i]]
    remaining <- setdiff(base_items, c)
    if (length(remaining) > 1) {
      setTxtProgressBar(pb, i)
      efa_out <- efa_custom(data[, remaining, drop = FALSE], n_factors, cor_method, extract, rotate)
      prob_items_test <- identify_problem_items(efa_out)
      cross_items_test <- unique(c(prob_items_test$cross_3, prob_items_test$cross_2))
      cross <- if (length(cross_items_test) > 0) "Yes" else "No"
      load_mat <- as.matrix(efa_out$loadings)
      threshold <- 0.30
      valid_loads <- abs(load_mat)[abs(load_mat) > threshold]
      load_min <- if (length(valid_loads) > 0) formatC(min(valid_loads), digits = 2, format = "f") else NA
      load_max <- if (length(valid_loads) > 0) formatC(max(valid_loads), digits = 2, format = "f") else NA
      removed_label <- if (length(c) == 0) "None" else paste(sort(c), collapse = "-")
      summary_table <- rbind(summary_table, data.frame(
        Removed_Items = removed_label,
        Total_Explained_Var = paste0("% ", formatC(efa_out$explained_var * 100, format = "f", digits = 2)),
        Factor_Loading_Range = paste0(load_min, "-", load_max),
        Cronbachs_Alpha = round(efa_out$alpha, 3),
        Cross_Loading = cross,
        stringsAsFactors = FALSE
      ))
      results[[removed_label]] <- list(
        removed = c,
        explained_var = efa_out$explained_var,
        alpha = efa_out$alpha,
        loadings = efa_out$loadings
      )
    }
  }
  close(pb)
  summary_table <- unique(summary_table)
  summary_table <- sort_strategy_labels(summary_table)
  return(list(results = results, summary_table = summary_table))
}

#' Automated Item Removal for Exploratory Factor Analysis (EFA)
#'
#' Applies EFA to a given dataset and identifies problematic items based on cross-loading and low-loading criteria.
#' Automatically tests all combinations of flagged items and summarizes explained variance, Cronbach's alpha,
#' and factor loading ranges. Offers optimal strategy selection based on absence of cross-loading.
#'
#' @param data A data.frame or matrix containing item responses.
#' @param cor_method Correlation method: "pearson" or "polychoric". Default is "pearson".
#' @param n_factors Number of factors to extract. If NULL, parallel analysis is used.
#' @param extract Extraction method for EFA. Common options include "uls", "ml", etc.
#' @param rotate Rotation method for EFA. Common options include "oblimin", "varimax", etc.
#' @param report Whether to display all tested strategies or only the optimal ones ("all" or "optimal").
#'
#' @return Console output and a summary table of evaluated removal strategies.
#' @export
itemrest <- function(data,
                     cor_method = "pearson",
                     n_factors = NULL,
                     extract = "uls",
                     rotate = "oblimin",
                     report = "optimal") {

  colnames(data) <- gsub("\\.", "_", colnames(data))

  cor_method <- tolower(cor_method)
  extract    <- tolower(extract)
  rotate     <- tolower(rotate)
  report     <- tolower(report)

  allowed_cor_methods <- c("pearson", "polychoric")
  allowed_extract     <- c("minres", "uls", "ml", "pa", "wls", "gls", "ols", "alpha", "beta", "pc")
  allowed_rotate      <- c("none", "varimax", "quartimax", "equamax", "bentlerT", "geominT", "oblimin", "promax", "quartimin", "bentlerQ", "geominQ", "target")
  allowed_report      <- c("optimal", "all")

  if (!cor_method %in% allowed_cor_methods)
    stop("Invalid 'cor_method'. Use one of: ", paste(allowed_cor_methods, collapse = ", "))
  if (!extract %in% allowed_extract)
    stop("Invalid 'extract'. Use one of: ", paste(allowed_extract, collapse = ", "))
  if (!rotate %in% allowed_rotate)
    stop("Invalid 'rotate'. Use one of: ", paste(allowed_rotate, collapse = ", "))
  if (!report %in% allowed_report)
    stop("Invalid 'report'. Use one of: ", paste(allowed_report, collapse = ", "))

  if (is.null(n_factors)) {
    n_factors <- determine_n_factors(data, cor_method)
    cat("\n[Info] Number of factors determined by parallel analysis:", n_factors, "\n")
  }

  descriptive_stats(data)
  efa_out <- efa_custom(data, n_factors, cor_method, extract, rotate)
  prob_items <- identify_problem_items(efa_out)
  cross_items <- sort_item_ids(unique(c(prob_items$cross_3, prob_items$cross_2)))
  low_items <- sort_item_ids(prob_items$low_loading)

  cat("\n=== Initial EFA Results ===\n")
  cat("\nCronbach's Alpha:", formatC(efa_out$alpha, digits = 3, format = "f"), "\n")
  cat("Total Explained Variance:", paste0("% ", formatC(efa_out$explained_var * 100, digits = 2, format = "f")), "\n")
  cat("Low-loading Items:", ifelse(length(low_items) == 0, "None", paste(low_items, collapse = ", ")), "\n")
  cat("Cross-loading Items:", ifelse(length(cross_items) == 0, "None", paste(cross_items, collapse = ", ")), "\n")

  combined_items <- sort_item_ids(unique(unlist(prob_items)))
  cat("\nAll identified Low Quality Items:", ifelse(length(combined_items) == 0, "None", paste(combined_items, collapse = ", ")), "\n")

  if (length(combined_items) > 0) {
    all_combs <- get_combinations(combined_items)
    cat("\nTrying", length(all_combs) - 1, "different combinations...\n")
    out <- test_removals(data, colnames(data), all_combs, n_factors, cor_method, extract, rotate)
    summary_table <- out$summary_table

    if (report == "all") {
      cat("\n=== All Removal Strategies ===\n")
      print(summary_table)
    } else if (report == "optimal") {
      cat("\n=== Optimal Removal Strategies (No cross-loading) ===\n")
      optimal_table <- subset(summary_table, Cross_Loading == "No")
      if (nrow(optimal_table) == 0) {
        cat("No cross-loading-free removal strategy found.\n")
      } else {
        extract_variance <- function(x) {
          as.numeric(gsub("[^0-9\\.]", "", x))
        }
        optimal_table$Explained_Variance_Numeric <- sapply(optimal_table$Total_Explained_Var, extract_variance)
        optimal_table <- optimal_table[order(-optimal_table$Explained_Variance_Numeric), ]
        optimal_table$Explained_Variance_Numeric <- NULL
        print(optimal_table)
      }
    }
  } else {
    cat("\nNo Low Quality Item detected.\n")
  }

  cat("\nAll operations completed.\n")
  message("Final Reminder: Let algorithms be your compass, not your captain. Valid item removal also requires theoretical competence.")
}
