test_that("no selection as all survive", {
  expect_equal(wrapper_intervention_site_after_truncation_selection_deployed(intervention.before.selection = 100,
                                                                             female.fitness.cost = 0,
                                                                             male.fitness.cost = 0,
                                                                             female.insecticide.exposure = 0.5,
                                                                             male.insecticide.exposure = 0.5,
                                                                             standard.deviation = 10,
                                                                             vector.length = 1000,
                                                                             maximum.bioassay.survival.proportion = 1,
                                                                             michaelis.menten.slope = 1,
                                                                             half.population.bioassay.survival.resistance = 900,
                                                                             regression.coefficient = 1,
                                                                             regression.intercept = 1,
                                                                             current.insecticide.efficacy = 1,
                                                                             exposure.scaling.factor = 10,
                                                                             heritability = 1), 100)
})

test_that("no selection as no insecticide efficacy", {
  expect_equal(wrapper_intervention_site_after_truncation_selection_deployed(intervention.before.selection = 100,
                                                                             female.fitness.cost = 0,
                                                                             male.fitness.cost = 0,
                                                                             female.insecticide.exposure = 0.5,
                                                                             male.insecticide.exposure = 0.5,
                                                                             standard.deviation = 10,
                                                                             vector.length = 1000,
                                                                             maximum.bioassay.survival.proportion = 1,
                                                                             michaelis.menten.slope = 1,
                                                                             half.population.bioassay.survival.resistance = 900,
                                                                             regression.coefficient = 1,
                                                                             regression.intercept = 0,
                                                                             current.insecticide.efficacy = 0,
                                                                             exposure.scaling.factor = 10,
                                                                             heritability = 1), 100)
})
