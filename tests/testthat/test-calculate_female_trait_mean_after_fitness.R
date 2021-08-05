test_that("equation works", {
  expect_equal(calculate_female_trait_mean_after_fitness(female.trait.mean = 10,
                                                         female.fitness.cost = 1), 9)

  expect_equal(calculate_female_trait_mean_after_fitness(female.trait.mean = 10,
                                                         female.fitness.cost = -1), 11)

  expect_equal(calculate_female_trait_mean_after_fitness(female.trait.mean = 10,
                                                         female.fitness.cost = 0), 4)
})
