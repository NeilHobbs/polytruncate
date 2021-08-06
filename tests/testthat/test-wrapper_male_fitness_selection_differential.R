test_that("equation works", {
  expect_equal(wrapper_male_fitness_selection_differential(male.trait.mean = 10,
                                                                      male.fitness.cost = 1), -1)

  expect_equal(wrapper_male_fitness_selection_differential(male.trait.mean = 10,
                                                           male.fitness.cost = 0), 0)


  expect_equal(wrapper_male_fitness_selection_differential(male.trait.mean = 10,
                                                           male.fitness.cost = -1), 1)
})
