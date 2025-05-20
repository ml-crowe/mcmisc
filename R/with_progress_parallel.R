#' Run a Function in Parallel with Progress Bar and Optional Beep
#'
#' This function provides a convenient wrapper for parallelized application of any function
#' using \code{future_lapply} (for single input) or \code{future_mapply} (for multiple inputs),
#' while displaying a progress bar. It supports flexible core allocation and optionally plays a sound when complete.
#'
#' @param FUN The function to apply. Should take as its first argument the elements of the input (as in \code{lapply}) or the arguments to be mapped over (as in \code{mapply}).
#' @param ... Arguments to be passed as inputs to \code{FUN}, typically one or more lists or vectors to iterate over.
#' @param steps Number of steps for the progress bar. If \code{NULL} (default), will be automatically set to the length of the first input.
#' @param future_mode \code{"lapply"} (default) for a single mapped input, or \code{"mapply"} for mapping over multiple inputs simultaneously.
#' @param show_beep Logical, whether to play a sound upon completion. Default is \code{TRUE}. Requires the \code{beepr} package.
#' @param workers Number of parallel workers. Default is \code{parallelly::availableCores() - 1}.
#' @param progress_format A string template for the progress bar, passed to \code{progressr::handler_progress}.
#' @param .seed Seed for parallel random number generation. Default is \code{NULL}.
#'
#' @return A list of the results from \code{FUN} applied in parallel to the input(s).
#'
#' @examples
#' # lapply example: squaring numbers in parallel
#' x <- 1:10
#' squares <- with_progress_parallel(FUN = function(x) x^2, x)
#'
#' # mapply example: adding two vectors in parallel
#' a <- 1:5
#' b <- 6:10
#' sums <- with_progress_parallel(FUN = function(x, y) x + y, a, b, future_mode = "mapply")
#'
#' @import future.apply
#' @importFrom future plan
#' @importFrom progressr handlers handler_progress with_progress progressor
#' @importFrom parallelly availableCores
#' @importFrom beepr beep
#' @export
with_progress_parallel <- function(FUN, ...,
                                   steps = NULL,
                                   future_mode = c("lapply", "mapply"),
                                   show_beep = TRUE,
                                   workers = parallelly::availableCores() - 1,
                                   progress_format = "[:bar] :percent :eta :message",
                                   .seed = NULL) {
  progressr::handlers(progressr::handler_progress(format = progress_format))
  future::plan(multisession, workers = workers)

  old_opt <- getOption("progressr.enable")
  options(progressr.enable = TRUE)
  on.exit({
    options(progressr.enable = old_opt)
    plan(sequential)
  }, add = TRUE)

  args <- list(...)
  future_mode <- match.arg(future_mode)
  if (is.null(steps)) steps <- length(args[[1]])
  result <- NULL

  progressr::with_progress({
    p <- progressr::progressor(steps = steps)
    if (future_mode == "lapply") {
      result <- future.apply::future_lapply(
        args[[1]],
        function(x, ...) {
          p()
          FUN(x, ...)
        },
        ...,
        future.seed = .seed
      )
    } else if (future_mode == "mapply") {
      result <- future.apply::future_mapply(
        function(...) {
          p()
          FUN(...)
        },
        ...,
        future.seed = .seed,
        SIMPLIFY = FALSE
      )
    }
  })
  future::plan(sequential)
  options(old_o)
  if (show_beep && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep()
  }
  return(result)
}
