#' Calculate Row Means with Allowed Number of NAs
#'
#' This function computes the mean of each row in a data frame or matrix,
#' similar to the SPSS `mean.n` function. Unlike the base `rowMeans()`
#' which returns `NA` if any values are missing by default, this function allows
#' a specified tolerance (`n`) for the number of `NA` values per row.
#'
#' For example, if you have a 15-item scale and a participant completed only 14,
#' `rowMeans()` would yield `NA` for that participant, but `mean_n()` can
#' still return the mean if the number of missing items is less than or equal to `n`.
#'
#' @param df A data frame or matrix of numeric variables (rows = observations, columns = items).
#' @param n An integer specifying the maximum number of allowed NAs in a row to still calculate the mean.
#'
#' @return A numeric vector of row means. Rows with more than `n` missing values return `NA`.
#'
#' @examples
#' df <- data.frame(
#'   item1 = c(1, 2, NA, 4),
#'   item2 = c(2, NA, NA, 4),
#'   item3 = c(3, 4, 4, NA)
#' )
#' mean_n(df, n = 1)
#' #> [1] 2 3 NA 4
#'
#' @export mean_n
mean_n <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  notvalid <- apply(as.matrix(df), 1, function(row) sum(is.na(row)))
  ifelse(notvalid <= n, means, NA)
}
