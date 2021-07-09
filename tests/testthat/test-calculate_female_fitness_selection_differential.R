test_that("equation works", {
  expect_equal(calculate_female_fitness_selection_differential(female.trait.mean = 10,
                                                               female.trait.mean.after.fitness = 0), -10)

  expect_equal(calculate_female_fitness_selection_differential(female.trait.mean = 10,
                                                               female.trait.mean.after.fitness = 5), -5)

    expect_equal(calculate_female_fitness_selection_differential(female.trait.mean = 5,
                                                               female.trait.mean.after.fitness = 10), 5)
})
