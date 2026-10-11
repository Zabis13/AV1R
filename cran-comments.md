## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: unable to verify current time — network issue in check environment, not a package problem.
* NOTE: non-portable compilation flag `-mno-omit-leaf-frame-pointer` — this flag is set by the
  system R installation, not by the package.

## Test environments

* Local: Linux Mint 22.3, R 4.3.3, x86_64
* GitHub Actions (R-CMD-check):
  * ubuntu-latest: R devel, release, oldrel-1
  * macos-latest, macos-14 (ARM64): R release
  * windows-latest, windows-11-arm: R release

## GPU encoding

The package supports GPU-accelerated AV1 encoding via VAAPI (AMD/Intel) and Vulkan
(VK_KHR_VIDEO_ENCODE_AV1). All GPU code paths are optional and guarded by runtime
detection. On CRAN machines without GPU support, the package falls back to CPU encoding
via FFmpeg. All GPU-specific tests skip unless an actual AV1-capable GPU device is
present (`skip_if_no_vulkan_av1()`, `skip_if_not()`), so they pass cleanly on
machines built with Vulkan headers but without a GPU.

## Downstream dependencies

None.
