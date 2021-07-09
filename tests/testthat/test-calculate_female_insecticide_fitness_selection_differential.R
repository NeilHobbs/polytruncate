test_that("equation work works", {
  expect_equal(calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = 10,
                                                                           exposure.scaling.factor = 10,
                                                                           female.fitness.selection.differential = 10), 110)

  expect_equal(calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = -10,
                                                                           exposure.scaling.factor = 10,
                                                                           female.fitness.selection.differential = 10), -90)


  expect_equal(calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = -10,
                                                                           exposure.scaling.factor = 10,
                                                                           female.fitness.selection.differential = -10), -110)
})
