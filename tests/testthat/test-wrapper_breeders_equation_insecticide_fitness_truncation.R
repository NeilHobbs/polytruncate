test_that("not heritable", {
  expect_equal(wrapper_breeders_equation_insecticide_fitness_truncation(vector.length = 1000,
                                                                        standard.deviation = 10,
                                                                        male.insecticide.exposure = 1,
                                                                        female.insecticide.exposure = 1,
                                                                        trait.mean = 60,
                                                                        maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1,
                                                                        half.population.bioassay.survival.resistance = 900,
                                                                        regression.coefficient = 0.48,
                                                                        regression.intercept = 0.15,
                                                                        current.insecticide.efficacy = 1,
                                                                        exposure.scaling.factor = 10,
                                                                        heritability = 0,
                                                                        male.fitness.cost = 10,
                                                                        female.fitness.cost = 100), 0)
})

test_that("NA becomes zero", {

  expect_equal(wrapper_breeders_equation_insecticide_fitness_truncation(vector.length = 1000,
                                                           standard.deviation = 10,
                                                           male.insecticide.exposure = 1,
                                                           female.insecticide.exposure = 1,
                                                           trait.mean = 0,
                                                           maximum.bioassay.survival.proportion = 1,
                                                           michaelis.menten.slope = 1,
                                                           half.population.bioassay.survival.resistance = 900,
                                                           regression.coefficient = 1,
                                                           regression.intercept = 0,
                                                           current.insecticide.efficacy = 1,
                                                           exposure.scaling.factor = 10,
                                                           heritability = 0,
                                                           male.fitness.cost = 0,
                                                           female.fitness.cost = 0), 0 )
})
