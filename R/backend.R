#' Detect best available encoding backend
#'
#' Returns \code{"vulkan"} if a Vulkan-capable GPU with
#' \code{VK_KHR_VIDEO_ENCODE_AV1} support is found, otherwise \code{"cpu"}.
#'
#' @param prefer \code{"auto"} (default), \code{"vulkan"}, or \code{"cpu"}.
#'
#' @return Character string: \code{"vulkan"} or \code{"cpu"}.
#' @export
detect_backend <- function(prefer = "auto") {
  prefer <- match.arg(prefer, c("auto", "vulkan", "cpu"))
  .Call("R_av1r_detect_backend", prefer)
}

#' Check Vulkan AV1 availability
#'
#' @return \code{TRUE} if the package was compiled with Vulkan AV1 encode support.
#' @export
vulkan_available <- function() {
  .Call("R_av1r_vulkan_available")
}

#' List Vulkan-capable GPU devices
#'
#' @return Character vector of device names. Devices supporting
#'   \code{VK_KHR_VIDEO_ENCODE_AV1} are marked with \code{[AV1]}.
#' @export
vulkan_devices <- function() {
  .Call("R_av1r_vulkan_devices")
}
