#' Convert video to AV1
#'
#' Converts biological microscopy video files (MP4/H.264, H.265, AVI/MJPEG)
#' or TIFF stacks to AV1 format. Automatically selects the best available
#' backend: Vulkan GPU (\code{VK_KHR_VIDEO_ENCODE_AV1}) when a compatible
#' device is found, otherwise CPU via FFmpeg (\code{libsvtav1} or
#' \code{libaom-av1}).
#'
#' @param input  Path to input file. Supported: .mp4, .avi, .mkv, .mov,
#'   .tif/.tiff (multi-page), or printf pattern like \code{"frame\%04d.tif"}.
#' @param output Path to output file (.mp4 or .mkv).
#' @param options An \code{av1r_options} list. Defaults to \code{av1r_options()}.
#'   Use \code{backend = "cpu"} or \code{backend = "vulkan"} to force a backend.
#'
#' @return Invisibly returns 0L on success. Stops with an error on failure.
#'
#' @examples
#' # List available options
#' str(av1r_options())
#'
#' \dontrun{
#' # Requires FFmpeg installed
#' convert_to_av1("recording.mp4", file.path(tempdir(), "recording_av1.mp4"))
#' }
#' @export
convert_to_av1 <- function(input, output, options = av1r_options()) {
  is_seq <- grepl("%", input, fixed = TRUE)
  if (!is_seq && !file.exists(input)) stop("Input file not found: ", input)
  check_ffmpeg()

  # Multi-page TIFF: extract pages to temp PNG sequence via magick
  # Skip if input is already an image sequence pattern (contains %)
  is_tiff <- !is_seq && grepl("\\.tiff?$", input, ignore.case = TRUE)
  tiff_tmpdir <- NULL
  if (is_tiff) {
    tiff_tmpdir <- tempfile("av1r_tiff_")
    input <- .tiff_to_png_sequence(input, tiff_tmpdir)
    on.exit(unlink(tiff_tmpdir, recursive = TRUE), add = TRUE)
  }

  bk <- if (options$backend == "auto") detect_backend() else options$backend

  if (bk == "libaom") {
    message("AV1R [libaom-av1]: fallback CPU encoder.",
            "\n  Note: 'preset' is ignored for libaom-av1.",
            "\n  For speed control use ffmpeg -cpu-used (0-8) directly.",
            "\n  Consider installing SVT-AV1 for better performance:",
            "\n  Check: ffmpeg -encoders | grep svt")
    .ffmpeg_encode_av1(input, output, options, encoder = "libaom-av1")
    return(invisible(0L))
  }

  if (bk == "vulkan") {
    # GPU path: ffmpeg decode to NV12 pipe -> Vulkan AV1 encode -> IVF -> MP4
    message("AV1R [gpu/vulkan]: Vulkan AV1 encode")
    info <- .ffmpeg_video_info(input)
    .Call("R_av1r_vulkan_encode",
          input, output,
          info$width, info$height, info$fps, options$crf,
          PACKAGE = "AV1R")
    message("AV1R: done.")
    return(invisible(0L))
  }

  if (bk == "vaapi") {
    message("AV1R [gpu/vaapi]: VAAPI AV1 encode")
    ret <- .vaapi_encode_av1(input, output, options)
    message("AV1R: done.")
    return(invisible(ret))
  }

  # CPU path: prefer SVT-AV1, fallback to libaom-av1
  if (.has_ffmpeg_encoder("libsvtav1")) {
    .ffmpeg_encode_av1(input, output, options, encoder = "libsvtav1")
  } else if (.has_ffmpeg_encoder("libaom-av1")) {
    message("AV1R: SVT-AV1 (libsvtav1) not found in ffmpeg, falling back to libaom-av1.",
            "\n  Note: 'preset' is ignored for libaom-av1.",
            "\n  For speed control use ffmpeg -cpu-used (0-8) directly.")
    .ffmpeg_encode_av1(input, output, options, encoder = "libaom-av1")
  } else {
    stop("No AV1 encoder found in ffmpeg (need libsvtav1 or libaom-av1).\n",
         "  Check: ffmpeg -encoders | grep -E 'svt|aom'")
  }
}

# Internal: call ffmpeg via system2()
.ffmpeg_encode_av1 <- function(input, output, options, encoder = "libsvtav1") {
  ffmpeg <- Sys.which("ffmpeg")

  is_tiff <- grepl("\\.tiff?$", input, ignore.case = TRUE)

  # For TIFF stacks ffmpeg reads via image2 demuxer
  # shQuote: system2() pastes args into a shell command, so paths with
  # spaces or special characters must be quoted explicitly.
  input_args <- if (is_tiff) {
    c("-framerate", "25", "-i", shQuote(input))
  } else {
    c("-i", shQuote(input))
  }

  encode_args <- c(
    "-c:v", encoder,
    "-crf", as.character(options$crf)
  )

  # -preset only for SVT-AV1; libaom-av1 uses -cpu-used
  if (encoder == "libsvtav1") {
    encode_args <- c(encode_args, "-preset", as.character(options$preset))
  } else if (encoder == "libaom-av1") {
    encode_args <- c(encode_args, "-cpu-used", "4")
  }

  if (options$threads > 0L) {
    encode_args <- c(encode_args, "-threads", as.character(options$threads))
  }

  args <- c(
    "-y",
    input_args,
    encode_args,
    "-an",   # no audio (microscopy video is silent)
    shQuote(output)
  )

  label <- if (encoder == "libaom-av1") "libaom" else "cpu"
  message(sprintf(
    "AV1R [%s]: ffmpeg %s -> %s  [encoder=%s crf=%d%s]",
    label, basename(input), basename(output), encoder, options$crf,
    if (encoder == "libsvtav1") sprintf(" preset=%d", options$preset) else ""
  ))

  stderr_file <- tempfile("av1r_ffmpeg_stderr_")
  on.exit(unlink(stderr_file), add = TRUE)
  ret <- system2(ffmpeg, args, stderr = stderr_file)

  if (ret != 0L) {
    stderr_out <- tryCatch(
      paste(readLines(stderr_file, warn = FALSE), collapse = "\n"),
      error = function(e) "(could not read stderr)"
    )
    stop("ffmpeg failed with exit code ", ret,
         "\nCommand: ffmpeg ", paste(shQuote(args), collapse = " "),
         "\nffmpeg stderr:\n", stderr_out)
  }

  message("AV1R: done.")
  invisible(ret)
}

# Internal: VAAPI AV1 encode via ffmpeg av1_vaapi
.vaapi_encode_av1 <- function(input, output, options) {
  ffmpeg <- Sys.which("ffmpeg")
  if (nchar(ffmpeg) == 0) stop("ffmpeg not found")

  is_tiff <- grepl("\\.tiff?$", input, ignore.case = TRUE)
  input_args <- if (is_tiff) c("-framerate", "25", "-i", shQuote(input)) else c("-i", shQuote(input))

  audio_args <- if (is_tiff) c("-an") else c("-c:a", "copy")

  # Rate control priority: explicit bitrate > auto-detect from input
  # Note: RADV (Mesa) does not implement CQP for AV1 — only VBR via -b:v works
  if (!is.null(options$bitrate)) {
    target_bps <- as.integer(options$bitrate) * 1000L
    rate_args  <- c("-b:v", as.character(target_bps))
    rate_label <- sprintf("bitrate=%dk (manual)", options$bitrate)
  } else {
    input_bitrate <- .ffmpeg_video_bitrate(input)
    if (!is.na(input_bitrate) && input_bitrate > 0) {
      # AV1 is ~45% more efficient than H.264 -> target 55% of input bitrate
      target_bps <- as.integer(input_bitrate * 0.55)
      rate_args  <- c("-b:v", as.character(target_bps))
      rate_label <- sprintf("bitrate=%dk (auto, 55%% of input)", target_bps %/% 1000L)
    } else {
      # Fallback for TIFF stacks: fixed 4 Mbps
      rate_args  <- c("-b:v", "4000000")
      rate_label <- "bitrate=4000k (default)"
    }
  }

  args <- c(
    "-y",
    "-vaapi_device", "/dev/dri/renderD128",
    input_args,
    "-vf", "format=nv12,hwupload",
    "-c:v", "av1_vaapi",
    rate_args,
    audio_args,
    shQuote(output)
  )

  message(sprintf(
    "AV1R [vaapi]: ffmpeg %s -> %s  [%s]",
    basename(input), basename(output), rate_label
  ))

  stderr_file <- tempfile("av1r_ffmpeg_stderr_")
  on.exit(unlink(stderr_file), add = TRUE)
  ret <- system2(ffmpeg, args, stderr = stderr_file)
  if (ret != 0L) {
    stderr_out <- tryCatch(
      paste(readLines(stderr_file, warn = FALSE), collapse = "\n"),
      error = function(e) "(could not read stderr)"
    )
    stop("ffmpeg vaapi failed with exit code ", ret,
         "\nCommand: ffmpeg ", paste(shQuote(args), collapse = " "),
         "\nffmpeg stderr:\n", stderr_out)
  }
  invisible(ret)
}

# Internal: get video stream bitrate in bps (NA if unavailable)
.ffmpeg_video_bitrate <- function(input) {
  ffprobe <- Sys.which("ffprobe")
  if (nchar(ffprobe) == 0) return(NA_integer_)

  lines <- tryCatch(
    suppressWarnings(
      system2(ffprobe,
              c("-v", "quiet", "-select_streams", "v:0",
                "-show_entries", "stream=bit_rate",
                "-of", "default=noprint_wrappers=1", shQuote(input)),
              stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) character(0)
  )

  val <- sub("bit_rate=", "", grep("^bit_rate=", lines, value = TRUE))
  bps <- suppressWarnings(as.integer(val))
  if (length(bps) == 0 || is.na(bps) || bps <= 0) NA_integer_ else bps
}

# Internal: get video width/height/fps via ffprobe
.ffmpeg_video_info <- function(input) {
  ffprobe <- Sys.which("ffprobe")
  if (nchar(ffprobe) == 0) ffprobe <- Sys.which("ffmpeg")
  if (nchar(ffprobe) == 0) stop("ffprobe/ffmpeg not found")

  lines <- tryCatch(
    suppressWarnings(
      system2(ffprobe,
              c("-v", "quiet", "-select_streams", "v:0",
                "-show_entries", "stream=width,height,r_frame_rate",
                "-of", "default=noprint_wrappers=1", shQuote(input)),
              stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) character(0)
  )

  width  <- as.integer(sub("width=",  "", grep("^width=",  lines, value = TRUE)))
  height <- as.integer(sub("height=", "", grep("^height=", lines, value = TRUE)))
  fps_str <- sub("r_frame_rate=", "", grep("^r_frame_rate=", lines, value = TRUE))

  fps <- if (length(fps_str) > 0 && grepl("/", fps_str)) {
    parts <- strsplit(fps_str, "/")[[1]]
    as.integer(round(as.numeric(parts[1]) / as.numeric(parts[2])))
  } else {
    25L
  }

  if (length(width) == 0 || length(height) == 0)
    stop("Could not read video dimensions from: ", input)

  list(width = width, height = height, fps = fps)
}

# Internal: extract multi-page TIFF to PNG sequence via magick
# Returns ffmpeg-compatible input path (printf pattern)
.tiff_to_png_sequence <- function(tiff_path, tmpdir) {
  if (!requireNamespace("magick", quietly = TRUE))
    stop("Package 'magick' is required for multi-page TIFF input.\n",
         "  Install: install.packages(\"magick\")")

  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

  img <- magick::image_read(tiff_path)
  n <- length(img)
  message(sprintf("AV1R: extracting %d frames from TIFF stack...", n))

  for (i in seq_len(n)) {
    out_file <- file.path(tmpdir, sprintf("frame%06d.png", i))
    magick::image_write(img[i], out_file, format = "png")
  }

  # Return printf pattern for ffmpeg
  file.path(tmpdir, "frame%06d.png")
}

