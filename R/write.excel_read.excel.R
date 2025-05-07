#' Copy a Data Frame or Matrix to the Clipboard for Excel
#'
#' Write a data frame or matrix to the Windows clipboard in a tab-delimited format
#' so that it can be easily pasted into Excel or other spreadsheet software.
#' This function handles `NA` values by replacing them with empty strings.
#' It intelligently manages row and column names for compatibility.
#'
#' @param x A data frame or matrix to write to the clipboard.
#' @param row.names Logical; if `TRUE`, write row names. Defaults to `FALSE`.
#' @param col.names Logical; if `TRUE`, write column names. Defaults to `TRUE`.
#' @param ... Additional arguments passed to [utils::write.table()].
#'
#' @return No return value; called for side effects.
#'
#' @details
#' On Windows, the clipboard acts like a temporary file. The content written by this function
#' can be pasted directly into Excel or similar programs by using Ctrl+V or paste commands.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(A = 1:3, B = c("x", "y", NA))
#' write.excel(df)
#' # Now, paste into Excel with Ctrl+V
#' }
#'
#' @export
write.excel <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(
    x,
    "clipboard-10000",
    sep = "\t",
    na = "",
    row.names = row.names,
    col.names = ifelse(row.names == TRUE & col.names == TRUE, NA, col.names),
    ...
  )
}

#' Read Tab-Delimited Data from the Clipboard (Excel)
#'
#' Read data from the Windows clipboard assuming tab-delimited content.
#' This is particularly useful for copying data from Excel or other spreadsheet
#' software into R quickly without having to save files.
#'
#' @param header Logical; if `TRUE`, the first row is used as column names. Defaults to `TRUE`.
#' @param ... Additional arguments passed to [utils::read.table()].
#'
#' @return A data frame containing the imported clipboard data.
#'
#' @examples
#' \dontrun{
#' # Copy some cells in Excel, then run:
#' df <- read.excel()
#' head(df)
#' }
#'
#' @export
read.excel <- function(header = TRUE, ...) {
  read.table("clipboard", sep = "\t", header = header, ...)
}
