test_that("palette_info generates correct information", {
  # Test with simple RGB matrix
  rgb_mat <- rbind(
    red   = c(1, 0, 0),
    green = c(0, 1, 0),
    blue  = c(0, 0, 1)
  )
  colnames(rgb_mat) <- c("red", "green", "blue")

  result <- palette_info(rgb_mat)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(
    names(result),
    c(
      "name", "red", "green", "blue", "L", "a", "b", "hex",
      "delta_2000", "cum_delta_2000"
    )
  )

  # Check color values
  expect_equal(result$name, c("red", "green", "blue"))
  expect_equal(result$hex[1], "#FF0000")

  # Check Lab values (approximate)
  expect_equal(unname(result$L[1]), 53.233, tolerance = 0.001) # Red
  expect_equal(unname(result$a[1]), 80.109, tolerance = 0.001)
  expect_equal(unname(result$b[1]), 67.220, tolerance = 0.001)

  # Check delta calculations
  # First delta should be NA
  expect_true(is.na(result$delta_2000[1]))
  # Other deltas should be numeric
  expect_true(all(!is.na(result$delta_2000[-1])))
  # Deltas should be positive
  expect_true(all(result$delta_2000[-1] > 0))

  # Check cumulative deltas
  expect_equal(result$cum_delta_2000[1], 0)
  expect_true(all(diff(result$cum_delta_2000) > 0))
  expect_equal(
    result$cum_delta_2000[3],
    sum(result$delta_2000[-1], na.rm = TRUE)
  )

  # Test with unnamed RGB matrix
  rgb_mat_unnamed <- rgb_mat
  rownames(rgb_mat_unnamed) <- NULL
  result2 <- palette_info(rgb_mat_unnamed)
  expect_equal(result2$hex, c("#FF0000", "#00FF00", "#0000FF"))
  expect_equal(result2$name, result2$hex)

  # Test with partially named matrix
  rgb_mat_partial <- rgb_mat
  rownames(rgb_mat_partial) <- c("red", "", "blue")
  result3 <- palette_info(rgb_mat_partial)
  expect_equal(result3$name, c("red", "#00FF00", "blue"))
  expect_equal(result3$hex, c("#FF0000", "#00FF00", "#0000FF"))
})
