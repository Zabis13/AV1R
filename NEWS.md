# AV1R 0.1.0

## Vulkan AV1 GPU Encoding

* Bundled Khronos `VK_KHR_video_encode_av1` headers (`src/vk_video/`) — package
  no longer requires Vulkan SDK >= 1.3.290 at build time; builds against any
  Vulkan SDK >= 1.3.275 with the bundled polyfill.
* Added `vulkan_video_encode_av1_khr.h` compatibility shim that provides
  all `VkVideoEncodeAV1*KHR` structs, enums and `VK_STRUCTURE_TYPE` constants
  missing from older SDKs.
* `configure` now detects bundled AV1 encode headers and enables GPU path
  automatically.
* Fixed `av1r_encode_vulkan.cpp` to match the final Vulkan 1.3.290 spec:
  `order_hint` field name, `referenceNameSlotIndices` moved to
  `VkVideoEncodeAV1PictureInfoKHR`.

## Initial Release

* CPU encoding via FFmpeg (`libsvtav1` / `libaom-av1`).
* GPU encoding via Vulkan `VK_KHR_VIDEO_ENCODE_AV1` (pipeline ready, pending
  driver support in Mesa RADV).
* Automatic backend selection: GPU (Vulkan) → CPU (FFmpeg).
* Supports H.264, H.265, AVI/MJPEG, TIFF stacks, and TIFF sequences as input.
* `convert_to_av1()` for single-file and batch conversion.
* `detect_backend()`, `vulkan_available()`, `vulkan_devices()` for diagnostics.
