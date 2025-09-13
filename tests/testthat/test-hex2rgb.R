test_that("hex2rgb converts HEX codes correctly", {
  # Test basic color conversion
  test_colors <- c("#FF0000", "#00FF00", "#0000FF")
  result <- hex2rgb(test_colors)

  expect_equal(dim(result), c(3, 3))
  expect_equal(colnames(result), c("red", "green", "blue"))
  expect_equal(result["#FF0000", "red"], 1)
  expect_equal(result["#00FF00", "green"], 1)
  expect_equal(result["#0000FF", "blue"], 1)

  # Test with named colors
  named_colors <- c(red = "#FF0000", green = "#00FF00")
  result <- hex2rgb(named_colors)
  expect_equal(rownames(result), c("red", "green"))

  # Test with mixed named/unnamed colors
  mixed_colors <- c(red = "#FF0000", "#00FF00")
  result <- hex2rgb(mixed_colors)
  expect_equal(rownames(result), c("red", "#00FF00"))

  # Test different maxvalue
  result <- hex2rgb("#FF0000", maxvalue = 255)
  expect_equal(result[1, "red"], 255)

  # Test without hash symbol
  result <- hex2rgb("FF0000")
  expect_equal(result["FF0000", "red"], 1)

  # Test case insensitivity
  expect_equal(
    hex2rgb("#ff0000"),
    hex2rgb("#FF0000")
  )
})
