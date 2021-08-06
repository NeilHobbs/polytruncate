test_that("NA becomes zero", {
  expect_equal(intervention_after_migration(intervention.after.selection = 100,
                                            staying.in.intervention = 0 ,
                                            insecticide.population.suppression = 1,
                                            joining.from.refugia = 0,
                                            refugia.after.selection = 100), 0)
})
