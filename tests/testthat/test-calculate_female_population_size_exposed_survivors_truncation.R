
test_that("equation works", {
  expect_equal(calculate_female_population_size_exposed_survivors_truncation(female.insecticide.exposure = 0,
                                                                             total.female.population.size = 1000,
                                                                             field.survival = 1), 0)

  expect_equal(calculate_female_population_size_exposed_survivors_truncation(female.insecticide.exposure = 1,
                                                                             total.female.population.size = 1000,
                                                                             field.survival = 0), 0)

  expect_equal(calculate_female_population_size_exposed_survivors_truncation(female.insecticide.exposure = 0.5,
                                                                             total.female.population.size = 1000,
                                                                             field.survival = 0.5), 250)

})
