test_that("rgb2lab converts RGB values correctly", {
  # Test primary colors
  rgb_data <- rbind(
    red   = c(1, 0, 0),
    green = c(0, 1, 0),
    blue  = c(0, 0, 1)
  )
  colnames(rgb_data) <- c("red", "green", "blue")

  result <- rgb2lab(rgb_data)

  # Test dimensions and column names
  expect_equal(dim(result), c(3, 3))
  expect_equal(colnames(result), c("L", "a", "b"))
  expect_equal(rownames(result), c("red", "green", "blue"))

  # Test known LAB values (approximate due to floating point)
  # Test red RGB(1,0,0)
  expect_equal(result["red", "L"], 53.233, tolerance = 0.001)
  expect_equal(result["red", "a"], 80.109, tolerance = 0.001)
  expect_equal(result["red", "b"], 67.220, tolerance = 0.001)

  # Test green RGB(0,1,0)
  expect_equal(result["green", "L"], 87.737, tolerance = 0.001)
  expect_equal(result["green", "a"], -86.185, tolerance = 0.001)
  expect_equal(result["green", "b"], 83.181, tolerance = 0.001)

  # Test blue RGB(0,0,1)
  expect_equal(result["blue", "L"], 32.297, tolerance = 0.001)
  expect_equal(result["blue", "a"], 79.188, tolerance = 0.001)
  expect_equal(result["blue", "b"], -107.86, tolerance = 0.001)

  # Test black and white
  bw_data <- rbind(
    black = c(0, 0, 0),
    white = c(1, 1, 1)
  )
  colnames(bw_data) <- c("red", "green", "blue")

  bw_result <- rgb2lab(bw_data)

  # Black should have L=0, white should have L=100
  expect_equal(bw_result["black", "L"], 0, tolerance = 0.01)
  expect_equal(bw_result["white", "L"], 100, tolerance = 0.01)

  # a and b should be close to 0 for black and white
  expect_equal(bw_result["black", c("a", "b")], c(a = 0, b = 0), tolerance = 0.01)
  expect_equal(bw_result["white", c("a", "b")], c(a = 0, b = 0), tolerance = 0.01)
})
