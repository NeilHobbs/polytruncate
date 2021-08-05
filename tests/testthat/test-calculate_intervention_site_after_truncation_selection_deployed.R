test_that("multiplication works", {
  expect_equal(calculate_intervention_site_after_truncation_selection_deployed(intervention.before.selection = 10,
                                                                               response.insecticide.fitness = 6), 16)
})


test_that("does not fall below zero", {
  expect_equal(calculate_intervention_site_after_truncation_selection_deployed(intervention.before.selection = 1,
                                                                               response.insecticide.fitness = -10), 0)
})
