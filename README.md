# AV1R

**AV1 video encoding for biological microscopy data.**

AV1R is an R package for biologists that converts legacy microscopy video formats (H.264/H.265, AVI/MJPEG, TIFF stacks) to the modern AV1 codec with minimal quality loss.

## Why AV1R?

- **Compress large TIFF stacks** from confocal microscopy, time-lapse, and EBImage workflows from hundreds of gigabytes to manageable sizes — without noticeable loss of information.
- **Re-encode MP4 files** exported from CellProfiler, ImageJ/Fiji, and microscope software with ~2x better compression at the same visual quality.
- **Standardise legacy recordings** — convert old AVI (MJPEG) and H.265 files to a single patent-free format suited for long-term archival.

## GPU acceleration

AV1R automatically selects the best available backend:

| Priority | Backend | How |
|----------|---------|-----|
| 1 | **VAAPI** | `av1_vaapi` via FFmpeg — best speed/quality (AMD/Intel) |
| 2 | **Vulkan** | `VK_KHR_VIDEO_ENCODE_AV1` — fastest, CQP only |
| 3 | **CPU (SVT-AV1)** | `libsvtav1` via FFmpeg |

### Tested hardware

| GPU | Driver | Status |
|-----|--------|--------|
| AMD Radeon RX 9070 (RDNA4) | Mesa RADV (GFX1201) | Working |

Vulkan AV1 encode headers are bundled in `src/vk_video/`, so no SDK upgrade is needed at build time. Builds with any Vulkan SDK >= 1.3.275. Runtime support depends on GPU driver.

### Benchmark (30 s clip, 1920x1080, CRF 28, AMD RX 9070)

| Backend | Time (s) | Size (MB) | Compression | SSIM | FPS | vs CPU |
|---------|----------|-----------|-------------|------|-----|--------|
| Vulkan  | 2.9      | 3.31      | 1.9x | 0.9339 | 245.9 | 4.0x |
| VAAPI   | 2.0      | 3.97      | 1.6x | 0.9609 | 363.3 | 5.9x |
| CPU (SVT-AV1) | 11.7 | 8.09  | 0.8x | 0.9645 | 61.6 | 1.0x |

Compression = input clip size / encoded size. SSIM >= 0.97 visually transparent.
vs CPU = CPU time / backend time (higher = faster).

Reproduce: `Rscript inst/examples/benchmark_backends.R`

## Quick Start

```r
library(AV1R)

# Convert microscopy recording
convert_to_av1("recording.mp4", "recording_av1.mp4")

# TIFF stack with custom settings
convert_to_av1("stack.tif", "stack.mp4", av1r_options(crf = 20))

# Batch convert entire experiment folder
convert_folder("experiment/", "compressed/")

# Check what backend will be used
detect_backend()
```

## GPU encoding

```r
# Check GPU availability
vulkan_available()
vulkan_devices()

# Force GPU backend
convert_to_av1("input.mp4", "output.mp4", av1r_options(backend = "vulkan"))

# Force CPU backend
convert_to_av1("input.mp4", "output.mp4", av1r_options(backend = "cpu"))
```

Each backend uses the quality control approach best suited to its rate control:

| Backend | Quality parameter | Adaptation |
|---------|-------------------|------------|
| CPU (SVT-AV1) | CRF directly | SVT-AV1 adapts to content |
| Vulkan | CRF directly (CQP) | Constant quantizer |
| VAAPI | 55% of input bitrate | Adapts via ffprobe |

Frames smaller than the hardware minimum coded extent are automatically
scaled up proportionally (no distortion). Audio from the original file
is preserved automatically.

**Note:** Vulkan uses CQP (constant quantizer) rate control — the only mode
currently working on RADV. CQP does not adapt to scene complexity, so Vulkan
produces slightly lower SSIM (~0.93) compared to CPU/VAAPI (~0.96). This is
a driver limitation; VBR/CRF support will improve quality when RADV implements
it.

## Supported Input Formats

| Format | Extension |
|--------|-----------|
| H.264 / MP4 | `.mp4`, `.mkv`, `.mov` |
| H.265 / HEVC | `.mp4`, `.mkv` |
| AVI / MJPEG | `.avi` |
| TIFF stack | `.tif`, `.tiff` |
| TIFF sequence | `frame%04d.tif` |

## System Requirements

- **FFmpeg >= 4.4** with `libsvtav1` (required for CPU encoding)
- **libvulkan-dev** (optional, for GPU encoding on Linux)

```bash
# Ubuntu 22.04+
sudo apt install ffmpeg libvulkan-dev
```

## TIFF scaling

TIFF stacks use near-lossless CRF 5 by default (`tiff_crf`). Output resolution can be controlled via `tiff_scale`:

```r
# 2x magnification (proportional, no distortion)
convert_to_av1("stack.tif", "stack.mp4", av1r_options(tiff_scale = 2))

# Fit into 1920x1080 bounding box (proportional)
convert_to_av1("stack.tif", "stack.mp4", av1r_options(tiff_scale = c(1920, 1080)))
```

See `inst/examples/tiff_scale.R` for more examples.

## Options

```r
av1r_options(
  crf        = 28,    # quality: 0 (best) - 63 (worst)
  preset     = 8,     # speed: 0 (slow/best) - 13 (fast/worst), CPU only
  threads    = 0,     # 0 = auto, CPU only
  tiff_crf   = 5,     # CRF for TIFF input (near-lossless)
  tiff_scale = NULL,  # NULL, multiplier (e.g. 2), or c(width, height)
  verbose    = TRUE,  # informational messages
  backend    = "auto" # "auto", "cpu", "vaapi", or "vulkan"
)
```

## License

MIT

---

[GitHub](https://github.com/Zabis13/AV1R) · [Issues](https://github.com/Zabis13/AV1R/issues)
