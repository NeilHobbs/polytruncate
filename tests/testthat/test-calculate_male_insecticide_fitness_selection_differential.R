test_that("equation works", {
  expect_equal(calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = 10,
                                                                         exposure.scaling.factor = 0,
                                                                         male.fitness.selection.differential = 0), 0)

  expect_equal(calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = 10,
                                                                         exposure.scaling.factor = 1,
                                                                         male.fitness.selection.differential = -1), 9)

})
