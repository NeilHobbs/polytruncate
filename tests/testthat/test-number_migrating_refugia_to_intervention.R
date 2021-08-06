test_that("dispersa.rate too low", {
  expect_error(number_migrating_refugia_to_intervention(dispersal.rate = -0.1,
                                                        intervention.coverage = 0.5), "dipersal.rate must be between 0 and 1")
})

test_that("dispersa.rate too high", {
  expect_error(number_migrating_refugia_to_intervention(dispersal.rate = 1.1,
                                                        intervention.coverage = 0.5), "dipersal.rate must be between 0 and 1")
})

test_that("dispersa.rate too low", {
  expect_error(number_migrating_refugia_to_intervention(intervention.coverage = -0.1,
                                                        dispersal.rate = 0.5), "intervention.coverage must be between 0 and 1")
})

test_that("dispersa.rate too high", {
  expect_error(number_migrating_refugia_to_intervention(intervention.coverage = 1.1,
                                                        dispersal.rate = 0.5), "intervention.coverage must be between 0 and 1")
})
