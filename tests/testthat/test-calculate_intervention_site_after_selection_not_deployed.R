test_that("equation works", {
  expect_equal(calculate_intervention_site_after_selection_not_deployed(intervention.before.selection = 10,
                                                                        response.fitness = -4), 6)
})

test_that("does not fall below zero", {
  expect_equal(calculate_intervention_site_after_selection_not_deployed(intervention.before.selection = 1,
                                                                                   response.fitness = -10), 0)
})
