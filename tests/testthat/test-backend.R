test_that("vulkan_available returns logical", {
  result <- vulkan_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("vulkan_devices returns character vector", {
  result <- vulkan_devices()
  expect_type(result, "character")
})

test_that("detect_backend returns valid value", {
  result <- detect_backend()
  expect_true(result %in% c("vulkan", "cpu"))
})

test_that("detect_backend respects cpu preference", {
  result <- detect_backend("cpu")
  expect_equal(result, "cpu")
})
