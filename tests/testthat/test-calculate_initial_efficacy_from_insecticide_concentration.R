test_that("equation works", {
  expect_equal(calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose = 1,
                                                                         recommended.insecticide.dose = 1), 1)

  expect_equal(calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose = 0.5,
                                                                         recommended.insecticide.dose = 1), 0.5)

  expect_equal(calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose = 100,
                                                                         recommended.insecticide.dose = 50), 2)
})
