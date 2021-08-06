test_that("equation works", {
  expect_equal(wrapper_female_fitness_selection_differential(female.trait.mean = 10,
                                                             female.fitness.cost = 0), 0)

  expect_equal(wrapper_female_fitness_selection_differential(female.trait.mean = 10,
                                                             female.fitness.cost = 1), -1)


  expect_equal(wrapper_female_fitness_selection_differential(female.trait.mean = 10,
                                                             female.fitness.cost = 2), -2)
})
