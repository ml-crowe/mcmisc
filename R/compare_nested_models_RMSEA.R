#' Compare Nested Lavaan Models Using RMSEAd Statistics
#'
#' Calculates difference-based RMSEA (RMSEAd) and associated confidence intervals
#' to compare nested lavaan models, based on Savalei et al. (2024).
#'
#' This function accepts either individual `lavaan` model objects or a list of models,
#' computes chi-square differences, degrees of freedom differences, difference in RMSEA,
#' the RMSEAd, confidence intervals, and equivalence test p-values for each consecutive pair of models.
#' It is intended to facilitate assessment of nested model comparisons beyond standard likelihood ratio tests.
#'
#' @param ... Individual `lavaan` model objects or a single list containing multiple `lavaan` models.
#' @param model_names Optional character vector specifying model names; length must match number of models.
#'   If `NULL` (default), models are named sequentially as `"Model_1"`, `"Model_2"`, etc.
#' @param CI Numeric scalar specifying the confidence level for RMSEAd confidence intervals (default is `0.90`).
#'
#' @details
#' The function requires at least two nested lavaan models. All models must have the same number of groups and total sample size.
#' RMSEAd and its confidence intervals are computed according to the approach described in Savalei et al. (2024).
#' Equivalence tests are provided for multiple RMSEA thresholds.
#'
#' @return A data frame summarizing model comparison pairs with columns:
#' \describe{
#'   \item{Comparison}{Character, description of the model pair compared (e.g., "Model_1 vs Model_2").}
#'   \item{Delta_ChiSq}{Difference in chi-square test statistics between models.}
#'   \item{Delta_df}{Difference in degrees of freedom between models.}
#'   \item{Delta_RMSEA}{Difference in RMSEA values between models.}
#'   \item{RMSEA_d}{Calculated RMSEAd statistic.}
#'   \item{RMSEA_d_Lower_CI}{Lower bound of the RMSEAd confidence interval.}
#'   \item{RMSEA_d_Upper_CI}{Upper bound of the RMSEAd confidence interval.}
#'   \item{p_RMSEA_gt0.05, p_RMSEA_gt0.06, p_RMSEA_gt0.08, p_RMSEA_gt0.10}{P-values for equivalence tests based on various RMSEA thresholds.}
#' }
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' # Define and fit nested models (example)
#' mod1 <- 'visual  =~ x1 + x2 + x3 + x4 + x5 + x6'
#' mod2 <- 'visual  =~ x1 + x2 + x3
#'          textual =~ x4 + x5 + x6'
#' mod3 <- 'visual  =~ x1 + x2 + x3 + x4
#'          textual =~ x4 + x5 + x6'
#' fit1 <- cfa(mod1, data = HolzingerSwineford1939)
#' fit2 <- cfa(mod2, data = HolzingerSwineford1939)
#' fit3 <- cfa(mod3, data = HolzingerSwineford1939)
#'
#' compare_nested_models_RMSEA(fit1, fit2, model_names = c("Model 1", "Model 2"))
#'
#' # Using a list of models is also valid
#' models_list <- list(fit1, fit2, fit3)
#' compare_nested_models_RMSEA(models_list, model_names = c("Model 1", "Model 2", "Model 3"))
#' }
#'
#' @seealso [lavaan::fitMeasures()], [lavaan::inspect()]
#' @references
#' Savalei, V., et al. (2024). *We need to change how we compute RMSEA for nested model comparisons in structural equation modeling*.
#'
#' @importFrom lavaan fitMeasures inspect
#' @export
compare_nested_models_RMSEA <- function(..., model_names = NULL, CI = .90) {
  require(lavaan)

  # Capture all input arguments
  models_input <- list(...)

  # Check if the first argument is a list of models
  if (length(models_input) == 1 && is.list(models_input[[1]]) &&
      all(sapply(models_input[[1]], function(x) inherits(x, "lavaan")))) {
    models <- models_input[[1]]
  } else {
    # Assume that individual models are passed directly
    models <- models_input
  }

  # Check if at least two models are provided
  if (length(models) < 2) {
    stop("Please provide at least two nested lavaan models for comparison.")
  }

  # Assign default model names if not provided
  if (is.null(model_names)) {
    model_names <- paste0("Model_", 1:length(models))
  } else {
    # Check if the number of names matches the number of models
    if (length(model_names) != length(models)) {
      stop("Length of model_names must match the number of models provided.")
    }
  }

  # Ensure that all elements in the models list are lavaan objects
  if (!all(sapply(models, function(x) inherits(x, "lavaan")))) {
    stop("All elements in the models list must be lavaan model objects.")
  }

  # Extract number of groups from each model
  n_groups <- sapply(models, function(x) lavaan::inspect(x, "ngroups"))

  # Check if all models have the same number of groups
  if(length(unique(n_groups)) != 1){
    stop("All models must have the same number of groups.")
  }

  G <- n_groups[1]  # Number of groups

  # Internal RMSEA.CI function
  RMSEA.CI <- function(T, df, N, G, CI){

    lower.lambda <- function(lambda) {
      (pchisq(T, df=df, ncp=lambda) - (1 - (1-CI)/2))
    }
    upper.lambda <- function(lambda) {
      (pchisq(T, df=df, ncp=lambda) - ((1-CI)/2))
    }

    # RMSEA CI Lower Bound
    lambda.l <- try(uniroot(f=lower.lambda, lower=0, upper=T)$root, silent=TRUE)
    if(inherits(lambda.l, "try-error")) {
      lambda.l <- NA
      RMSEA.CI.l <- NA  # Could consider setting lower CI to 0 if if uniroot fails
    } else {
      if(lambda.l < 0){
        RMSEA.CI.l <- 0
      } else {
        RMSEA.CI.l <- sqrt(lambda.l * G / ((N - 1) * df))
      }
    }

    # RMSEA CI Upper Bound
    N.RMSEA <- max(N, T * 4)
    lambda.u <- try(uniroot(f=upper.lambda, lower=0, upper=N.RMSEA)$root, silent=TRUE)
    if(inherits(lambda.u, "try-error")) {
      lambda.u <- NA
      RMSEA.CI.u <- NA
    } else {
      if(lambda.u < 0){
        RMSEA.CI.u <- 0
      } else {
        RMSEA.CI.u <- sqrt(lambda.u * G / ((N - 1) * df))
      }
    }

    RMSEA.CI <- c(RMSEA.CI.l, RMSEA.CI.u)
    names(RMSEA.CI) <- c("Lower_CI", "Upper_CI")
    return(RMSEA.CI)
  }

  # Internal pvals function
  pvals <- function(T, df, N, G){
    RMSEA0 <- c(0, 0.01, 0.05, 0.06, 0.08, 0.1)
    eps0 <- df * RMSEA0^2 / G
    nonc <- eps0 * (N - G)
    pvals <- pchisq(T, df=df, ncp=nonc)
    names(pvals) <- c("RMSEA>0", "RMSEA>.01", "RMSEA>.05", "RMSEA>.06", "RMSEA>.08", "RMSEA>.10")
    return(pvals)
  }

  # Extract fit indices, sample size, and number of parameters from each model
  fit_indices <- data.frame(
    Model = model_names,
    ChiSq = sapply(models, function(x) fitMeasures(x, "chisq")),
    df = sapply(models, function(x) fitMeasures(x, "df")),
    RMSEA = sapply(models, function(x) fitMeasures(x, "rmsea")),
    stringsAsFactors = FALSE
  )

  # Extract sample size (assuming all models have the same sample size)
  # changed "nobs" to "ntotal" as "nobs" didn't work when there were multiple groups
  # my read of the Savalei et al. 2024 manuscript is that all of the calculations using sample size use total sample rather than group sample
  N_values <- sapply(models, function(x) inspect(x, "ntotal"))
  if(length(unique(N_values)) != 1){
    stop("All models must have the same sample size (ntotal).")
  }
  N <- N_values[1]

  # Initialize vectors to store comparison results
  comparison <- paste(model_names[-length(model_names)], "vs", model_names[-1])
  Delta_ChiSq <- vector("numeric", length = length(models) - 1)
  Delta_df <- vector("numeric", length = length(models) - 1)
  Delta_RMSEA <- vector("numeric", length = length(models) - 1)
  RMSEA_d <- vector("numeric", length = length(models) - 1)
  RMSEA_d_Lower_CI <- vector("numeric", length = length(models) - 1)
  RMSEA_d_Upper_CI <- vector("numeric", length = length(models) - 1)
  pvals_list <- vector("list", length = length(models) - 1)

  # Loop through consecutive model pairs
  for(i in 2:length(models)) {
    # Chi-square and df differences
    Delta_ChiSq[i - 1] <- fit_indices$ChiSq[i] - fit_indices$ChiSq[i - 1]
    Delta_df[i - 1] <- fit_indices$df[i] - fit_indices$df[i - 1]

    # G is number of groups
    # Already extracted before the loop

    # Compute RMSEAd
    numerator <- Delta_ChiSq[i - 1] - Delta_df[i - 1]
    denominator <- Delta_df[i - 1] * (N - G) / G

    if(denominator <= 0) {
      warning(paste("Non-positive denominator for RMSEAd computation between",
                    model_names[i - 1], "and", model_names[i], ". Setting RMSEAd to NA."))
      rmsea_d_value <- NA
    } else {
      if(numerator <= 0){
        rmsea_d_value <- 0
      } else {
        rmsea_d_value <- sqrt(numerator / denominator)
      }
    }
    RMSEA_d[i - 1] <- rmsea_d_value

    # Compute RMSEA CI for RMSEAd
    if(!is.na(rmsea_d_value)) {
      ci <- RMSEA.CI(T = Delta_ChiSq[i - 1], df = Delta_df[i - 1], N = N, G = G, CI = CI)
      RMSEA_d_Lower_CI[i - 1] <- ci["Lower_CI"]
      RMSEA_d_Upper_CI[i - 1] <- ci["Upper_CI"]
    } else {
      RMSEA_d_Lower_CI[i - 1] <- NA
      RMSEA_d_Upper_CI[i - 1] <- NA
    }

    # Equivalence tests based on RMSEAd
    if(!is.na(rmsea_d_value)) {
      pvals_comp <- pvals(T = Delta_ChiSq[i - 1], df = Delta_df[i - 1], N = N, G = G)
      pvals_list[[i - 1]] <- round(pvals_comp, 3)
    } else {
      pvals_list[[i - 1]] <- rep(NA, 6)
      names(pvals_list[[i - 1]]) <- c("RMSEA>0", "RMSEA>.01", "RMSEA>.05",
                                      "RMSEA>.06", "RMSEA>.08", "RMSEA>.10")
    }

    # Compute Delta RMSEA
    Delta_RMSEA[i - 1] <- fit_indices$RMSEA[i] - fit_indices$RMSEA[i - 1]
  }

  # Compile p-values into separate columns
  pvals_df <- do.call(rbind, pvals_list)
  rownames(pvals_df) <- NULL

  # Create the final comparison data frame
  comparison_df <- data.frame(
    Comparison = comparison,
    Delta_ChiSq = Delta_ChiSq,
    Delta_df = Delta_df,
    Delta_RMSEA = Delta_RMSEA,
    RMSEA_d = RMSEA_d,
    RMSEA_d_Lower_CI = RMSEA_d_Lower_CI,
    RMSEA_d_Upper_CI = RMSEA_d_Upper_CI,
    #p_RMSEA_gt0 = pvals_df[, "RMSEA>0"],
    #p_RMSEA_gt0.01 = pvals_df[, "RMSEA>.01"],
    p_RMSEA_gt0.05 = pvals_df[, "RMSEA>.05"],
    p_RMSEA_gt0.06 = pvals_df[, "RMSEA>.06"],
    p_RMSEA_gt0.08 = pvals_df[, "RMSEA>.08"],
    p_RMSEA_gt0.10 = pvals_df[, "RMSEA>.10"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  return(comparison_df)
}
