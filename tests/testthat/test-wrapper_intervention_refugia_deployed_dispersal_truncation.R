test_that("no dispersal no refugia change", {
  expect_equal(wrapper_intervention_refugia_deployed_dispersal_truncation(insecticide.population.suppression = 0,
                                                                          intervention.before.selection = 100,
                                                                          female.fitness.cost = 0,
                                                                          male.fitness.cost = 0,
                                                                          female.insecticide.exposure = 1,
                                                                          male.insecticide.exposure = 1,
                                                                          standard.deviation = 10,
                                                                          vector.length = 1000,
                                                                          maximum.bioassay.survival.proportion = 1,
                                                                          michaelis.menten.slope = 1,
                                                                          half.population.bioassay.survival.resistance = 900,
                                                                          regression.coefficient = 1,
                                                                          regression.intercept = 0,
                                                                          current.insecticide.efficacy = 1,
                                                                          exposure.scaling.factor = 1,
                                                                          heritability = 1,
                                                                          refugia.before.selection = 10,
                                                                          dispersal.rate = 0,
                                                                          intervention.coverage = 0.5)[[2]], 10)
})

test_that("complete suppression, intervention resistance is refugia resistance", {

  expect_equal(wrapper_intervention_refugia_deployed_dispersal_truncation(insecticide.population.suppression = 1,
                                                                          intervention.before.selection = 100,
                                                                          female.fitness.cost = 0,
                                                                          male.fitness.cost = 0,
                                                                          female.insecticide.exposure = 1,
                                                                          male.insecticide.exposure = 1,
                                                                          standard.deviation = 10,
                                                                          vector.length = 1000,
                                                                          maximum.bioassay.survival.proportion = 1,
                                                                          michaelis.menten.slope = 1,
                                                                          half.population.bioassay.survival.resistance = 900,
                                                                          regression.coefficient = 1,
                                                                          regression.intercept = 0,
                                                                          current.insecticide.efficacy = 1,
                                                                          exposure.scaling.factor = 1,
                                                                          heritability = 1,
                                                                          refugia.before.selection = 10,
                                                                          dispersal.rate = 0.1,
                                                                          intervention.coverage = 0.5)[[1]], 10)
})
