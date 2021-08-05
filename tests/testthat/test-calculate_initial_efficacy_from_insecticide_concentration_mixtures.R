test_that("multiplication works", {
  expect_equal(calculate_initial_efficacy_from_insecticide_concentration_mixtures(mixture.dose = 1,
                                                                                  recommended.insecticide.dose = 1), 1)

  expect_equal(calculate_initial_efficacy_from_insecticide_concentration_mixtures(mixture.dose = 0.5,
                                                                                  recommended.insecticide.dose = 1), 0.5)

  expect_equal(calculate_initial_efficacy_from_insecticide_concentration_mixtures(mixture.dose = 50,
                                                                                  recommended.insecticide.dose = 100), 0.5)
})
