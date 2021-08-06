test_that("error works", {
  expect_error(calculate_population_size_unexposed_truncation(total.population.size = 1000,
                                                              insecticide.exposure = -0.1), "insecticide.exposure must be between 0 and 1")

  expect_error(calculate_population_size_unexposed_truncation(total.population.size = 1000,
                                                              insecticide.exposure = 1.1), "insecticide.exposure must be between 0 and 1")
  })


test_that("equation works", {
  expect_equal(calculate_population_size_unexposed_truncation(total.population.size = 1000,
                                                              insecticide.exposure = 0.5), 500)
})
