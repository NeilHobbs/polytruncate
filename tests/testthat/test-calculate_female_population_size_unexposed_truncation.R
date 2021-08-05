test_that("equation works", {
  expect_equal(calculate_female_population_size_unexposed_truncation(total.female.population.size = 1000,
                                                      female.insecticide.exposure = 0.7), 300)

  expect_equal(calculate_female_population_size_unexposed_truncation(total.female.population.size = 1000,
                                                                     female.insecticide.exposure = 1), 0)

  expect_equal(calculate_female_population_size_unexposed_truncation(total.female.population.size = 1000,
                                                                     female.insecticide.exposure = 0), 1000)
})
