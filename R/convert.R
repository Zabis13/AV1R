#' Convert video to AV1
#'
#' Converts biological microscopy video files (MP4/H.264, H.265, AVI/MJPEG)
#' or TIFF stacks to AV1 format. Uses ffmpeg binary for CPU encoding,
#' or Vulkan GPU if available.
#'
#' @param input  Path to input file. Supported: .mp4, .avi, .mkv, .mov,
#'   .tif/.tiff (multi-page), or printf pattern like \code{"frame\%04d.tif"}.
#' @param output Path to output file (.mp4 or .mkv).
#' @param options An \code{av1r_options} list. Defaults to \code{av1r_options()}.
#'
#' @return Invisibly returns the ffmpeg exit code (0 = success).
#'
#' @examples
#' \dontrun{
#' convert_to_av1("recording.mp4", "recording_av1.mp4")
#' convert_to_av1("stack.tif", "stack.mp4", av1r_options(crf = 20))
#' }
#' @export
convert_to_av1 <- function(input, output, options = av1r_options()) {
  if (!file.exists(input)) stop("Input file not found: ", input)
  check_ffmpeg()

  bk <- if (options$backend == "auto") detect_backend() else options$backend

  if (bk == "vulkan") {
    # GPU path: Vulkan AV1 (C++ bindings) - not yet implemented, fallback to CPU
    message("AV1R: Vulkan GPU path not yet implemented, using CPU.")
    bk <- "cpu"
  }

  # CPU path: ffmpeg binary
  .ffmpeg_encode_av1(input, output, options)
}

# Internal: call ffmpeg via system2()
.ffmpeg_encode_av1 <- function(input, output, options) {
  ffmpeg <- Sys.which("ffmpeg")

  is_tiff <- grepl("\\.tiff?$", input, ignore.case = TRUE)

  # For TIFF stacks ffmpeg reads via image2 demuxer
  input_args <- if (is_tiff) {
    c("-framerate", "25", "-i", input)
  } else {
    c("-i", input)
  }

  # Pick AV1 encoder: prefer libsvtav1, fallback to libaom-av1
  encoder <- .pick_av1_encoder()

  encode_args <- c(
    "-c:v", encoder,
    "-crf", as.character(options$crf),
    "-preset", as.character(options$preset)
  )

  if (options$threads > 0L) {
    encode_args <- c(encode_args, "-threads", as.character(options$threads))
  }

  args <- c(
    "-y",
    input_args,
    encode_args,
    "-an",   # no audio (microscopy video is silent)
    output
  )

  message(sprintf(
    "AV1R [cpu]: ffmpeg %s -> %s  [encoder=%s crf=%d preset=%d]",
    basename(input), basename(output), encoder, options$crf, options$preset
  ))

  ret <- system2(ffmpeg, args)

  if (ret != 0L) {
    stop("ffmpeg failed with exit code ", ret,
         "\nCommand: ffmpeg ", paste(args, collapse = " "))
  }

  message("AV1R: done.")
  invisible(ret)
}

# Pick the best available AV1 encoder in the installed ffmpeg
.pick_av1_encoder <- function() {
  ffmpeg <- Sys.which("ffmpeg")
  encoders <- tryCatch(
    system2(ffmpeg, c("-encoders", "-v", "quiet"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (any(grepl("libsvtav1", encoders)))  return("libsvtav1")
  if (any(grepl("libaom-av1", encoders))) return("libaom-av1")
  stop("No AV1 encoder found in ffmpeg (need libsvtav1 or libaom-av1).\n",
       "  Install: sudo apt install ffmpeg  (Ubuntu 22.04+ includes libsvtav1)")
}
