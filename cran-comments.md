## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: unable to verify current time — network issue in check environment, not a package problem.
* NOTE: non-portable compilation flag `-mno-omit-leaf-frame-pointer` — this flag is set by the
  system R installation, not by the package.

## Test environments

* Local: Linux Mint 22.3, R 4.3.3, x86_64
* GitHub Actions: ubuntu-latest, R release
* GitHub Actions: macos-latest, R release

## GPU encoding

The package supports GPU-accelerated AV1 encoding via VAAPI (AMD/Intel) and Vulkan
(VK_KHR_VIDEO_ENCODE_AV1). All GPU code paths are optional and guarded by runtime
detection. On CRAN machines without GPU support, the package falls back to CPU encoding
via FFmpeg. All GPU-specific tests use `skip_if_not()` and pass cleanly without GPU.

## Downstream dependencies

None.
