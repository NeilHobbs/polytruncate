test_that("equation works", {
  expect_equal(calculate_insecticide_selection_differential_truncation(mean.post.selection = 20,
                                                                       trait.mean = 10), 10)
})
