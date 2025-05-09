# Helper function to calculate p-values from z-scores
# p_from_z <- function(z) {
#   2 * pnorm(abs(z), lower.tail = FALSE)
# }
p_from_z <- function(z, two_tail = T){
  if(two_tail == T){
    2*pnorm(abs(z), lower.tail = F)
  }else{
    pnorm(abs(z), lower.tail = F)
  }
}

# Helper function for formatting numbers (removes leading zero, rounds)
format_num <- function(val, digits = 2) {
  if (is.na(val)) return(NA_character_)
  # Check if it's already formatted as "n=..."
  if (is.character(val) && grepl("^n=", val)) return(val)

  # Ensure val is numeric before formatting, handle potential characters from 'n=' on diagonal
  num_val <- suppressWarnings(as.numeric(val))
  if(is.na(num_val)) return(as.character(val)) # If it wasn't 'n=' and not numeric, return as is

  formatted_val <- sprintf(paste0("%.", digits, "f"), num_val)
  sub("^(-?)0.", "\\1.", formatted_val)
}

#' Create a Detailed Correlation Matrix
#'
#' Calculates and formats a correlation matrix, allowing for various correlation types,
#' variable selection, and significance flagging.
#'
#' @param data A data.frame.
#' @param vars Optional. A character vector of variable names to be used as rows
#'        in the correlation matrix. If NULL, all variables in `data` are used.
#' @param with Optional. A character vector of variable names to be used as columns
#'        in the correlation matrix. If NULL and `vars` is also specified, a symmetric
#'        matrix for `vars` is produced. If NULL and `vars` is also NULL, a symmetric
#'        matrix for all variables in `data` is produced. If specified, an
#'        asymmetric matrix is produced with `vars` as rows and `with` as columns.
#' @param type Character string specifying the type of correlation:
#'        "pearson" (default), "spearman", or "mixed".
#'        For "mixed", `polycor::hetcor` is used.
#' @param flag Logical. If TRUE (default), flag significant correlations.
#' @param strict Logical. If TRUE (default is FALSE), uses a stricter criterion for flags
#'        (e.g., only '*' for p < .01). If FALSE, uses '*' for p < .05, '**' for p < .01,
#'        and "'" for p < .10.
#' @param round Integer. Number of decimal places for displayed correlations (default 2).
#' @param use_for_hetcor Passed to `polycor::hetcor`'s `use` argument when `type = "mixed"`.
#'        Default is "pairwise.complete.obs". For `Hmisc::rcorr`, handling is internal.
#' @param ... other inputs for `Hmisc::rcorr` or `polycor::hetcor`.
#'
#' @return A list with the following components:
#'   \item{formatted_table}{The formatted correlation matrix with flags and Ns on diagonal (if applicable).}
#'   \item{r}{The raw correlation matrix.}
#'   \item{p}{The raw p-value matrix.}
#'   \item{n}{The matrix of sample sizes for each correlation.}
#'
#' @examples
#' # Basic Pearson correlation
#' r_table(mtcars[, 1:5])
#'
#' # Spearman with specific variables
#' r_table(mtcars, vars = c("mpg", "wt"), with = c("cyl", "hp"), type = "spearman")
#'
#' # Mixed data types example
#' set.seed(123)
#' mixed_data <- data.frame(
#'   cont1 = rnorm(50),
#'   cont2 = rnorm(50),
#'   ord1 = factor(sample(1:3, 50, TRUE), ordered = TRUE),
#'   bin1 = factor(sample(0:1, 50, TRUE))
#' )
#' result_mixed <- r_table(mixed_data, type = "mixed", round = 3)
#' print(result_mixed$formatted_table)
#' print(result_mixed$r)
#'
#' @importFrom Hmisc rcorr
#' @importFrom polycor hetcor
#' @export
r_table <- function(data,
                    vars = NULL,
                    with = NULL,
                    type = 'pearson',
                    flag = TRUE,
                    strict = FALSE,
                    round = 2,
                    use_for_hetcor = "pairwise.complete.obs",
                    ...) {

  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
    message("Input 'data' was coerced to a data.frame.")
  }

  all_colnames <- colnames(data)

  # Determine row_vars and col_vars for the matrix
  if (is.null(vars)) {
    row_vars <- all_colnames
  } else {
    if (!all(vars %in% all_colnames)) stop("Some 'vars' not found in data column names.")
    row_vars <- vars
  }

  if (is.null(with)) {
    col_vars <- row_vars # For a symmetric matrix
    symmetric_output <- TRUE
  } else {
    if (!all(with %in% all_colnames)) stop("Some 'with' variables not found in data column names.")
    col_vars <- with
    symmetric_output <- length(row_vars) == length(col_vars) && all(sort(row_vars) == sort(col_vars))
    # If symmetric_output is true here, we still need to ensure order matches for diagonal Ns
    # Best to ensure col_vars is used for actual column selection in rcorr or hetcor subsetting
  }

  # Store original full data variable names for reference if needed later
  original_full_var_names <- colnames(data)

  # Identify all variables involved in correlation
  vars_for_cor <- unique(c(row_vars, col_vars))
  data_subset_cor <- data[, vars_for_cor, drop = FALSE]

  # Initialize matrices
  Rmat_r <- NULL
  Rmat_P <- NULL
  Rmat_n <- NULL

  if (type == 'mixed') {
    if (!requireNamespace("polycor", quietly = TRUE)) {
      stop("Package 'polycor' needed for type='mixed'. Please install it.", call. = FALSE)
    }

    # hetcor might fail if a factor has only one level after NAs removed
    # or if a variable is constant.
    # Check for single-level factors or constant variables after NA removal for pairwise operations.
    # This check is complex for pairwise; polycor::hetcor will usually error gracefully.

    # Convert character columns to factors for hetcor if they are not already
    # This is a common preprocessing step as hetcor expects factors for categorical data
    for(col_name in colnames(data_subset_cor)){
      if(is.character(data_subset_cor[[col_name]])){
        data_subset_cor[[col_name]] <- factor(data_subset_cor[[col_name]])
        message(paste("Column '", col_name, "' was character and has been converted to factor for hetcor.", sep=""))
      }
    }


    hc_results <- tryCatch({
      polycor::hetcor(data_subset_cor, use = use_for_hetcor, std.err = TRUE, ...)
    }, error = function(e) {
      stop(paste("polycor::hetcor failed. Error:", e$message,
                 "\nThis can happen with insufficient data, constant variables, or single-level factors after NA removal for a pair."), call. = FALSE)
    })

    raw_output <- hc_results

    Rmat_r_full <- hc_results$correlations
    Rmat_std_err_full <- hc_results$std.errors
    Rmat_n_full <- hc_results$n

    z_values_full <- Rmat_r_full / Rmat_std_err_full
    # Suppress warnings for NaNs produced by 0/0 if SE is 0 and R is 0
    z_values_full[is.nan(z_values_full)] <- 0
    Rmat_P_full <- p_from_z(z_values_full, two_tail = TRUE)

    # Ensure matrix dimensions if only one var in row_vars or col_vars
    if (!is.matrix(Rmat_r_full) | is.null(dimnames(Rmat_r_full))) Rmat_r_full <- matrix(Rmat_r_full, nrow = length(row_vars), ncol = length(col_vars), dimnames = list(row_vars, col_vars))
    if (!is.matrix(Rmat_P_full) | is.null(dimnames(Rmat_P_full))) Rmat_P_full <- matrix(Rmat_P_full, nrow = length(row_vars), ncol = length(col_vars), dimnames = list(row_vars, col_vars))
    if (!is.matrix(Rmat_n_full) | is.null(dimnames(Rmat_n_full))) Rmat_n_full <- matrix(Rmat_n_full, nrow = length(row_vars), ncol = length(col_vars), dimnames = list(row_vars, col_vars))

    # Subset to the desired row_vars and col_vars
    Rmat_r <- Rmat_r_full[row_vars, col_vars, drop = FALSE]
    Rmat_P <- Rmat_P_full[row_vars, col_vars, drop = FALSE]
    Rmat_n <- Rmat_n_full[row_vars, col_vars, drop = FALSE]

  } else { # pearson or spearman
    if (!requireNamespace("Hmisc", quietly = TRUE)) {
      stop("Package 'Hmisc' needed for pearson/spearman. Please install it.", call. = FALSE)
    }

    # Hmisc::rcorr takes matrix as input
    data_x <- as.matrix(data_subset_cor)

    # Hmisc::rcorr requires at least 2 columns
    if(nrow(data_x) <=2) {
      stop("At least two variables are needed for a correlation.")
    }

    rcorr_result <- Hmisc::rcorr(data_x, type = type, ...)
    Rmat_r_full <- rcorr_result$r
    Rmat_P_full <- rcorr_result$P
    Rmat_n_full <- rcorr_result$n

    raw_output <- rcorr_result

    # Subset to the desired row_vars and col_vars
    Rmat_r <- Rmat_r_full[row_vars, col_vars, drop = FALSE]
    Rmat_P <- Rmat_P_full[row_vars, col_vars, drop = FALSE]
    Rmat_n <- Rmat_n_full[row_vars, col_vars, drop = FALSE]

    # Ensure matrix dimensions if only one var in row_vars or col_vars
    # Hmisc::rcorr output is already correctly dimensioned for single row/col cases
    if (!is.matrix(Rmat_r) | is.null(dimnames(Rmat_r))) Rmat_r <- matrix(Rmat_r, nrow = length(row_vars), ncol = length(col_vars), dimnames = list(row_vars, col_vars))
    if (!is.matrix(Rmat_P) | is.null(dimnames(Rmat_P))) Rmat_P <- matrix(Rmat_P, nrow = length(row_vars), ncol = length(col_vars), dimnames = list(row_vars, col_vars))
    if (!is.matrix(Rmat_n) | is.null(dimnames(Rmat_n))) Rmat_n <- matrix(Rmat_n, nrow = length(row_vars), ncol = length(col_vars), dimnames = list(row_vars, col_vars))
  }

  # Create the formatted table
  # 1. Round correlations
  formatted_r <- apply(Rmat_r, c(1,2), format_num, digits = round)

  # 2. Add significance flags
  if (flag) {
    # Initialize flags matrix
    flags_matrix <- matrix("", nrow = nrow(Rmat_P), ncol = ncol(Rmat_P))

    # NA p-values should not get flags
    valid_p_indices <- !is.na(Rmat_P)

    if (strict) { # Only one level of significance: p <= .01
      flags_matrix[valid_p_indices & Rmat_P <= .01] <- "*"
    } else { # Multiple levels: .10 ('), .05 (*), .01 (**)
      flags_matrix[valid_p_indices & Rmat_P <= .01] <- "**"
      flags_matrix[valid_p_indices & Rmat_P > .01 & Rmat_P <= .05] <- "*"
      flags_matrix[valid_p_indices & Rmat_P > .05 & Rmat_P <= .10] <- "'"
    }
    formatted_table <- matrix(paste0(formatted_r, flags_matrix),
                              nrow = nrow(Rmat_r),
                              dimnames = dimnames(Rmat_r))
  } else {
    formatted_table <- formatted_r
  }

  # 3. Put Ns on the diagonal where appropriate
  # A cell (r, c) is on the "conceptual diagonal" if rownames[r] == colnames[c]
  # For these cells, we replace the correlation with N
  # The N is taken from the (r,c) cell of Rmat_n
  for (r_idx in 1:nrow(formatted_table)) {
    for (c_idx in 1:ncol(formatted_table)) {
      if (rownames(formatted_table)[r_idx] == colnames(formatted_table)[c_idx]) {
        # Ensure Rmat_n[r_idx, c_idx] is not NULL or NA before using it.
        # This N refers to the N for that variable (if type is pearson/spearman from Hmisc)
        # or the N for the pair (var_i, var_i) from polycor which is N for that var.
        if(!is.null(Rmat_n[r_idx, c_idx]) && !is.na(Rmat_n[r_idx, c_idx])) {
          formatted_table[r_idx, c_idx] <- paste0("n=", Rmat_n[r_idx, c_idx])
        } else {
          # If N is NA (e.g. variable all NAs), reflect that
          formatted_table[r_idx, c_idx] <- paste0("n=NA") # Or some other indicator
        }
      }
    }
  }


  # Replace NA correlations in formatted table with blanks or "NA" for clarity
  formatted_table[is.na(Rmat_r)] <- "NA"

  output <- list(
    formatted_table = as.data.frame(formatted_table, stringsAsFactors = FALSE),
    r = as.data.frame(Rmat_r),
    p = as.data.frame(Rmat_P),
    n = as.data.frame(Rmat_n),
    raw_output = raw_output,
    cor_type = type, # store the correlation type used
    strict_flag = strict # store the strict flagging option
  )

  class(output) <- "r_tableObject" # Assign a class for custom printing
  return(output)
}

#' Print method for r_tableObject
#'
#' Custom print output for objects of class `r_tableObject`.
#' (Users typically do not call this directly, but via `print(object)`).
#'
#' @param x An object of class `r_tableObject`.
#' @param ... Further arguments passed to or from other methods (not used by this method).
#' @return Invisibly returns the original object `x`.
#' @export
print.r_tableObject <- function(x, ...) {
  type <- x$cor_type
  cat(type, "correlation matrix:\n")
  print(x$formatted_table)
  if(x$strict_flag == T){
    cat("  '*' p <= .01\n")
  } else {
    cat(" '**' p <= .01; '*' p <= .05; ''' p <= .10\n")
  }
  invisible(x)
}
