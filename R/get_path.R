#' Reformat file path in clipboard
#'
#' @description
#' By default windows pastes file paths with backslashes, which is inconsistent
#' with R's use of forward slashes. This function will reformat file paths copied to clipboard to be compatible with R.
#' @param to_clipboard Logical. If TRUE, the function will write the new path to the clipboard. If FALSE, it will return the new path.
#' @return If to_clipboard is FALSE, the function returns the new path. If TRUE, it writes the new path to the clipboard.
#' @export
#' @examples
#' \dontrun{
#' # Can copy a file path in windows explorer using Shift + Right Click and selecting "Copy as path"
#' get_path() # reformat the path in the clipboard, can then use Ctrl + V to paste where needed
#' get_path(F) # pastes the new path to the console}
get_path <- function(to_clipboard = T){
  path_from_clipboard <- readClipboard()
  # Remove leading and trailing quotes
  trimmed_path <- gsub('^"|"$', '', path_from_clipboard)
  # Replace backslashes with forward slashes
  new_path <- gsub("\\\\", "/", trimmed_path, fixed = F)
  if(to_clipboard == T){
    writeClipboard(new_path, format = 13)
  }
  if(to_clipboard == F)
    return(new_path)
}
