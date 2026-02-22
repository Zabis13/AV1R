#' AV1R encoding options
#'
#' @param crf     Constant Rate Factor: 0 (lossless) to 63 (worst). Default 28.
#'   Lower = better quality, larger file.
#' @param preset  Encoding speed preset: 0 (slowest/best) to 13 (fastest).
#'   Default 8 (good balance for microscopy batch jobs).
#' @param threads Number of CPU threads. 0 = auto-detect.
#' @param backend \code{"auto"} (GPU if available, else CPU), \code{"vulkan"},
#'   or \code{"cpu"}.
#'
#' @return A named list of encoding parameters.
#'
#' @examples
#' # Default options
#' av1r_options()
#'
#' # High-quality lossless-ish for publication figures
#' av1r_options(crf = 15, preset = 4)
#'
#' # Fast batch conversion of large TIFF stacks
#' av1r_options(crf = 32, preset = 12, threads = 16)
#'
#' @export
av1r_options <- function(crf     = 28L,
                          preset  = 8L,
                          threads = 0L,
                          backend = "auto") {
  backend <- match.arg(backend, c("auto", "vulkan", "cpu"))
  stopifnot(is.numeric(crf),    crf    >= 0, crf    <= 63)
  stopifnot(is.numeric(preset), preset >= 0, preset <= 13)
  stopifnot(is.numeric(threads), threads >= 0)

  structure(
    list(crf     = as.integer(crf),
         preset  = as.integer(preset),
         threads = as.integer(threads),
         backend = backend),
    class = "av1r_options"
  )
}

#' @export
print.av1r_options <- function(x, ...) {
  cat(sprintf(
    "AV1R options: crf=%d  preset=%d  threads=%s  backend=%s\n",
    x$crf, x$preset,
    if (x$threads == 0L) "auto" else as.character(x$threads),
    x$backend
  ))
  invisible(x)
}
