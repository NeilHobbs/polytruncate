test_that("division works", {
  expect_equal(calculate_insecticide_population_suppression(female.population.size.after.selection = 100,
                                                            total.female.population.size = 200), 0.5)
})
