#!/usr/bin/env Rscript
# AV1R: Benchmark all available backends on the same input file

library(AV1R)

# ── Settings ──────────────────────────────────────────────────────────────
input      <- "/mnt/Data2/DS_projects/AV_test/test.mp4"
output_dir <- "/mnt/Data2/DS_projects/AV_test/"
crf        <- 28L
duration   <- 1  # seconds to encode (NULL = full file)

# ──────────────────────────────────────────────────────────────────────────
if (!file.exists(input)) stop("File not found: ", input)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Trim input to first N seconds if duration is set
if (!is.null(duration)) {
  bench_input <- file.path(tempdir(), paste0("av1r_bench_clip.", tools::file_ext(input)))
  cat(sprintf("Trimming first %d s from: %s\n", duration, input))
  ret <- system2("ffmpeg", c(
    "-y", "-i", shQuote(input),
    "-t", as.character(duration),
    "-c", "copy", shQuote(bench_input)
  ), stdout = FALSE, stderr = FALSE)
  if (ret != 0) stop("ffmpeg trim failed")
} else {
  bench_input <- input
}

cat("Input:", bench_input, "\n")
cat("Size: ", round(file.info(bench_input)$size / 1024^2, 1), "MB\n")
cat("Output dir:", output_dir, "\n\n")

# ── Backends to test ──────────────────────────────────────────────────────
all_backends <- c("vulkan", "vaapi", "cpu", "libaom")

available <- vapply(all_backends, function(bk) {
  tryCatch({
    detected <- detect_backend(prefer = bk)
    detected == bk
  }, error = function(e) FALSE)
}, logical(1))

backends <- names(available[available])
cat("Available backends:", paste(backends, collapse = ", "), "\n")
cat("CRF:", crf, "\n\n")

if (length(backends) == 0) stop("No backends available")

# ── Benchmark ─────────────────────────────────────────────────────────────
base <- sub("\\.[^.]+$", "", basename(input))
results <- data.frame(
  backend   = character(),
  time_sec  = numeric(),
  size_mb   = numeric(),
  ratio     = numeric(),
  stringsAsFactors = FALSE
)

input_mb <- file.info(bench_input)$size / 1024^2

for (bk in backends) {
  out <- file.path(output_dir, paste0(base, "_av1_", bk, ".mp4"))
  cat(sprintf("── %s ", toupper(bk)))
  cat(strrep("-", max(1, 60 - nchar(bk))), "\n")

  elapsed <- tryCatch({
    t0 <- proc.time()["elapsed"]
    convert_to_av1(bench_input, out, options = av1r_options(crf = crf, backend = bk))
    proc.time()["elapsed"] - t0
  }, error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NA_real_
  })

  if (!is.na(elapsed) && file.exists(out)) {
    out_mb <- file.info(out)$size / 1024^2
    results <- rbind(results, data.frame(
      backend  = bk,
      time_sec = round(as.numeric(elapsed), 1),
      size_mb  = round(out_mb, 2),
      ratio    = round(input_mb / out_mb, 2)
    ))
    cat("  Output:", out, "\n")
  } else {
    results <- rbind(results, data.frame(
      backend = bk, time_sec = NA, size_mb = NA, ratio = NA
    ))
  }
  cat("\n")
}

# ── Results ───────────────────────────────────────────────────────────────
n_frames <- as.numeric(system2("ffprobe", c(
  "-v", "quiet", "-count_frames", "-select_streams", "v:0",
  "-show_entries", "stream=nb_read_frames",
  "-of", "default=noprint_wrappers=1:nokey=1",
  shQuote(bench_input)
), stdout = TRUE, stderr = FALSE))

cat("\n")
cat(strrep("=", 62), "\n")
cat(sprintf("  %-10s %10s %10s %10s %10s\n",
            "Backend", "Time (s)", "Size (MB)", "Ratio", "FPS"))
cat(strrep("-", 62), "\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  fps_str <- if (!is.na(r$time_sec) && r$time_sec > 0)
    sprintf("%10.1f", n_frames / r$time_sec) else sprintf("%10s", "-")
  cat(sprintf("  %-10s %10s %10s %10s %s\n",
              r$backend,
              if (is.na(r$time_sec)) "FAILED" else sprintf("%.1f", r$time_sec),
              if (is.na(r$size_mb))  "-"      else sprintf("%.2f", r$size_mb),
              if (is.na(r$ratio))    "-"      else sprintf("%.2fx", r$ratio),
              fps_str))
}
cat(strrep("=", 62), "\n")
