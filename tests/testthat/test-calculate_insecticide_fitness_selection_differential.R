test_that("equation works", {
  expect_equal(calculate_insecticide_fitness_selection_differential(exposure.scaling.factor = 10,
                                                                    insecticide.selection.differential = 1,
                                                                    fitness.selection.differential = -1), 9)
})
