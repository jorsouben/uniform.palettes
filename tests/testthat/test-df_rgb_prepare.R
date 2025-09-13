test_that("df_rgb_prepare handles data frames correctly", {
  # Test with default column names
  df <- data.frame(
    r = c(255, 128, 0),
    g = c(0, 255, 128),
    b = c(128, 0, 255)
  )

  result <- df_rgb_prepare(df, maxvalue = 255)

  expect_equal(dim(result), c(3, 3))
  expect_equal(colnames(result), c("red", "green", "blue"))
  expect_equal(result[1, ], c(red = 1, green = 0, blue = 0.5019608), tolerance = 0.0001)

  # Test with custom column names
  df2 <- data.frame(
    RED = c(1, 0.5, 0),
    GREEN = c(0, 1, 0.5),
    BLUE = c(0.5, 0, 1)
  )

  result2 <- df_rgb_prepare(
    df2,
    channel_map = c(red = "RED", green = "GREEN", blue = "BLUE")
  )

  expect_equal(dim(result2), c(3, 3))
  expect_equal(colnames(result2), c("red", "green", "blue"))
  expect_equal(result2[1, ], c(red = 1, green = 0, blue = 0.5))

  # Test with matrix input
  mat <- matrix(
    c(
      255, 0, 128,
      0, 255, 0,
      128, 0, 255
    ),
    nrow = 3,
    dimnames = list(NULL, c("r", "g", "b"))
  )

  result3 <- df_rgb_prepare(mat, maxvalue = 255)
  expect_equal(dim(result3), c(3, 3))
  expect_equal(colnames(result3), c("red", "green", "blue"))
})
