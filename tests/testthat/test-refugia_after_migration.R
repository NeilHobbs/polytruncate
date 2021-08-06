test_that("equation works", {
  expect_equal(refugia_after_migration(intervention.after.selection = 100,
                                       joining.from.intervention = 1000,
                                       insecticide.population.suppression = 0,
                                       refugia.after.selection = 100,
                                       staying.in.refugia = 1000), 100)
})

test_that("suppression works", {
  expect_equal(refugia_after_migration(intervention.after.selection = 60,
                                       joining.from.intervention = 1000,
                                       insecticide.population.suppression = 1,
                                       refugia.after.selection = 100,
                                       staying.in.refugia = 1000), 100)
})


test_that("NA becomes zero", {
  expect_equal(refugia_after_migration(intervention.after.selection = 60,
                                       joining.from.intervention = 0,
                                       insecticide.population.suppression = 1,
                                       refugia.after.selection = 100,
                                       staying.in.refugia = 0), 0)
})
