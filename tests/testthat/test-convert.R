test_that("convert_to_av1 errors on missing input", {
  expect_error(
    convert_to_av1("/nonexistent/file.mp4", tempfile(fileext = ".mp4")),
    "Input file not found"
  )
})

test_that("read_tiff_stack errors on missing file", {
  expect_error(read_tiff_stack("/no/such/file.tif"), "File not found")
})
