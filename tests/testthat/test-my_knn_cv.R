test_that("correct output type", {
  expect_is(my_knn_cv(my_penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], my_penguins$species, 5, 5), list)
})
