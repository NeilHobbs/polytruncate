test_that("multiplication works", {
  expect_equal(calculate_male_population_size_unexposed_truncation(total.male.population.size = 10000,
                                                                   male.insecticide.exposure = 0,
                                                                   female.insecticide.exposure = 0), 10000)

  expect_equal(calculate_male_population_size_unexposed_truncation(total.male.population.size = 10000,
                                                                   male.insecticide.exposure = 1,
                                                                   female.insecticide.exposure = 1), 0)


  expect_equal(calculate_male_population_size_unexposed_truncation(total.male.population.size = 10000,
                                                                   male.insecticide.exposure = 0.5,
                                                                   female.insecticide.exposure = 1), 5000)
})
