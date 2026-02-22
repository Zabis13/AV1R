test_that("convert_to_av1 errors on missing input", {
  expect_error(
    convert_to_av1("/nonexistent/file.mp4", tempfile(fileext = ".mp4")),
    "Input file not found"
  )
})

test_that("read_tiff_stack errors on missing file", {
  expect_error(read_tiff_stack("/no/such/file.tif"), "File not found")
})

test_that("read_tiff_stack returns correct structure for existing file", {
  tmp <- tempfile(fileext = ".tif")
  file.create(tmp)
  on.exit(unlink(tmp))

  result <- read_tiff_stack(tmp)

  expect_type(result, "list")
  expect_named(result, c("path", "n_frames", "width", "height", "size_mb"))
  expect_equal(result$path, tmp)
  expect_type(result$size_mb, "double")
})
