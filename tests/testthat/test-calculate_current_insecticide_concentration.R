test_that("no decay", {
  expect_equal(calculate_current_insecticide_concentration(applied.concentration = 100,
                                                           instantaneous.decay.rate = 0,
                                                           generations.since.deployment = 10), 100)
})
