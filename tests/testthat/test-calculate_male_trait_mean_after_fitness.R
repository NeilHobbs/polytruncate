test_that("equation works", {
  expect_equal(calculate_male_trait_mean_after_fitness(male.trait.mean = 14,
                                                       male.fitness.cost = 4), 10)
})
