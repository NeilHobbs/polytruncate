test_that("equation works", {
  expect_equal(calculate_male_fitness_selection_differential(male.trait.mean = 10,
                                                             male.trait.mean.after.fitness = 10), 0)

  expect_equal(calculate_male_fitness_selection_differential(male.trait.mean = 10,
                                                             male.trait.mean.after.fitness = 8), -2)
})
