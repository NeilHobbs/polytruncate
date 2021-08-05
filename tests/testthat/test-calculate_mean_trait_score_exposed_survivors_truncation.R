test_that("equation works", {
  expect_equal(calculate_mean_trait_score_exposed_survivors_truncation(trait.mean = 10,
                                                                       standard.deviation = 1,
                                                                       field.survival = 1), 10)

  expect_equal(calculate_mean_trait_score_exposed_survivors_truncation(trait.mean = 10,
                                                                       standard.deviation = 1,
                                                                       field.survival = 0), 10)
})
