test_that("equation works", {
  expect_equal(calculate_male_population_size_exposed_survivors_truncation(male.insecticide.exposure = 1,
                                                                           female.insecticide.exposure = 1,
                                                                           total.male.population.size = 10000,
                                                                           field.survival = 1), 10000)

  expect_equal(calculate_male_population_size_exposed_survivors_truncation(male.insecticide.exposure = 1,
                                                                           female.insecticide.exposure = 1,
                                                                           total.male.population.size = 10000,
                                                                           field.survival = 1), 10000)


  expect_equal(calculate_male_population_size_exposed_survivors_truncation(male.insecticide.exposure = 0,
                                                                           female.insecticide.exposure = 1,
                                                                           total.male.population.size = 10000,
                                                                           field.survival = 1), 0)


  expect_equal(calculate_male_population_size_exposed_survivors_truncation(male.insecticide.exposure = 1,
                                                                           female.insecticide.exposure = 1,
                                                                           total.male.population.size = 10000,
                                                                           field.survival = 0), 0)
})
