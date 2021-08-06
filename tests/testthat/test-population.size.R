test_that("equation works", {
  expect_equal(calculate_population_size_post_selection_truncation(population.size.exposed.survivors = 1000,
                                                                   population.size.unexposed = 1000), 2000)
})
