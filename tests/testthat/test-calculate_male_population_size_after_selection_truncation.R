test_that("equation works", {
  expect_equal(calculate_male_population_size_after_selection_truncation(male.population.size.exposed.survivors = 1000,
                                                                         male.population.size.unexposed = 600), 1600)
})
