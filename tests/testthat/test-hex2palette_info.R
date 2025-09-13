test_that("hex2palette_info generates correct information from hex codes", {
  # Test with example palette
  hex_colors <- c(
    yellow = "#FFCC00",
    green = "#4EC433",
    blue = "#007BC4",
    red = "#C43E4E"
  )

  result <- hex2palette_info(hex_colors)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_equal(
    names(result),
    c(
      "name", "red", "green", "blue", "L", "a", "b", "hex",
      "delta_2000", "cum_delta_2000"
    )
  )

  # Check names and hex values
  expect_equal(result$name, c("yellow", "green", "blue", "red"))
  expect_equal(unname(result$hex), unname(toupper(hex_colors)))

  # First delta should be NA, others should be numeric
  expect_true(is.na(result$delta_2000[1]))
  expect_true(all(!is.na(result$delta_2000[-1])))

  # Cumulative delta should increase
  expect_true(all(diff(result$cum_delta_2000) >= 0))

  # Test with unnamed colors
  unnamed_colors <- c("#FF0000", "#00FF00", "#0000FF")
  result2 <- hex2palette_info(unnamed_colors)
  expect_equal(result2$name, unnamed_colors)
  expect_equal(result2$hex, toupper(unnamed_colors))
})
