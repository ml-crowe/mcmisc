#' Compute the Mode (Most Frequent Value) of a Vector
#'
#' Returns the mode (most frequent value) of the input vector.
#' If there are multiple values tied for the highest frequency,
#' this function returns the mean of those modes and issues a warning
#' indicating which values were averaged.
#'
#' @param x A numeric vector (or vector of values) from which to find the mode.
#' @param na.rm Logical; if \code{TRUE}, missing values (\code{NA}) are removed before computation. Default is \code{FALSE}.
#'
#' @return A numeric value representing the mode of \code{x}. If multiple modes exist,
#'  the mean of those modes is returned.
#'
#' @details
#' This function mimics the concept of mode, but when there are multiple modes,
#' instead of returning all modes or \code{NA}, it returns their average.
#' A warning is emitted to alert the user to this behavior, listing the values
#' that were averaged.
#'
#' @examples
#' x1 <- c(1, 2, 2, 3, 4)
#' Mode(x1) # Single mode - no warning expected
#'
#' x2 <- c(1, 2, 2, 3, 3, 4)
#' Mode(x2) # Multiple modes - warning expected, returns mean of 2 and 3
#'
#' Mode(c(1, 2, 2, 3, 3, 4)) # Warning, returns 2.5
#' Mode(c(5, 5, 5, 6))       # Returns 5 without warning
#' Mode(c(1, 2, NA, 2), na.rm = TRUE)
#'
#' x3 <- c(1, 2, 2, NA, NA, NA, 3, 3, 4)
#' Mode(x3, na.rm = TRUE) # NA should be removed, returns 2.5
#' Mode(x3, na.rm = FALSE) # NA counted, returns NA

#' x4 <- c(1, 2, 2, 2, NA, NA, NA, 3, 3, 4)
#' Mode(x4, na.rm = FALSE) # NA counted, average of NA and 2 is considered NA
#'
#'# Empty vector or all NAs
#' x5 <- c(NA, NA)
#' Mode(x5, na.rm = TRUE) # Returns NA
#' x5 <- c()
#' Mode(x5, na.rm = TRUE) # Returns NULL
#' @export
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  tabs <- tabulate(match(x, ux))
  max_tab <- max(tabs)
  getmode <- ux[which(tabs == max_tab)]

  if (length(getmode) > 1) {
    warning(
      "Multiple modes found (",
      paste(getmode, collapse = ", "),
      "): returning the mean of these modes."
    )
    return(mean(getmode))
  } else {
    return(getmode)
  }
}
