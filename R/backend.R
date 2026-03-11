#' Detect best available encoding backend
#'
#' Priority order: \code{"vulkan"} (Vulkan AV1 GPU) >
#' \code{"vaapi"} (VAAPI AV1 GPU via ffmpeg, AMD/Intel) >
#' \code{"cpu"} (SVT-AV1 via ffmpeg) >
#' \code{"libaom"} (libaom-av1 via ffmpeg, fallback).
#'
#' @param prefer \code{"auto"} (default), \code{"vulkan"}, \code{"vaapi"},
#'   \code{"cpu"}, or \code{"libaom"}.
#'
#' @return Character string: \code{"vulkan"}, \code{"vaapi"}, \code{"cpu"},
#'   or \code{"libaom"}.
#' @export
detect_backend <- function(prefer = "auto") {
  prefer <- match.arg(prefer, c("auto", "vulkan", "vaapi", "cpu", "libaom"))
  if (prefer == "libaom") return("libaom")
  if (prefer == "cpu")    return("cpu")
  if (prefer == "vaapi")  return(if (.vaapi_av1_available()) "vaapi" else "cpu")
  # prefer == "vulkan" or "auto": try Vulkan first
  vk <- tryCatch(
    .Call("R_av1r_detect_backend", if (prefer == "vulkan") "vulkan" else "auto", PACKAGE = "AV1R"),
    error = function(e) "cpu"
  )
  if (vk == "vulkan") return("vulkan")
  # fallback: try VAAPI
  if (.vaapi_av1_available()) return("vaapi")
  # fallback: try SVT-AV1, then libaom-av1
  if (.has_ffmpeg_encoder("libsvtav1")) return("cpu")
  if (.has_ffmpeg_encoder("libaom-av1")) return("libaom")
  "cpu"
}

# Internal: check if ffmpeg has a given encoder
.has_ffmpeg_encoder <- function(encoder_name) {
  ffmpeg <- Sys.which("ffmpeg")
  if (nchar(ffmpeg) == 0) return(FALSE)
  encoders <- tryCatch(
    system2(ffmpeg, c("-encoders", "-v", "quiet"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  any(grepl(encoder_name, encoders, fixed = TRUE))
}

# Internal: check VAAPI AV1 encode via vainfo
.vaapi_av1_available <- function() {
  vainfo <- Sys.which("vainfo")
  if (nchar(vainfo) == 0) return(FALSE)
  lines <- tryCatch(
    suppressWarnings(system2(vainfo, stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  any(grepl("VAProfileAV1", lines) & grepl("VAEntrypointEncSlice", lines))
}

#' Show AV1R backend status
#'
#' Prints the active encoding backend and available GPU/CPU capabilities.
#'
#' @return Invisibly returns the active backend string.
#' @export
av1r_status <- function() {
  bk <- detect_backend()
  cat("backend:", bk, "\n")
  invisible(bk)
}

#' Check Vulkan AV1 availability
#'
#' @return \code{TRUE} if the package was compiled with Vulkan AV1 encode support.
#' @export
vulkan_available <- function() {
  tryCatch(.Call("R_av1r_vulkan_available", PACKAGE = "AV1R"), error = function(e) FALSE)
}

#' List Vulkan-capable GPU devices
#'
#' @return Character vector of device names. Devices supporting
#'   \code{VK_KHR_VIDEO_ENCODE_AV1} are marked with \code{[AV1]}.
#' @export
vulkan_devices <- function() {
  tryCatch(.Call("R_av1r_vulkan_devices", PACKAGE = "AV1R"), error = function(e) character(0))
}
