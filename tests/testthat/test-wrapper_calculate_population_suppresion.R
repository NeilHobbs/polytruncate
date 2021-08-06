sim.array = create_starting_array(n.insecticides = 1,
                                  maximum.generations = 2)

sim.array["intervention", 1, 1] = 100



test_that("no population suppression", {
  expect_equal(wrapper_calculate_population_suppresion(current.insecticide.efficacy = 1,
                                                       currently.deployed.insecticide = 1,
                                                       vector.length = 1000,
                                                       current.generation = 2,
                                                       standard.deviation = 10,
                                                       female.insecticide.exposure = 0,
                                                       maximum.bioassay.survival.proportion = 1,
                                                       michaelis.menten.slope = 1,
                                                       half.population.bioassay.survival.resistance = 900,
                                                       regression.coefficient = 1,
                                                       regression.intercept = 1,
                                                       sim.array = sim.array,
                                                       population.suppression = FALSE), 0)
})

test_that("complete suppression", {
  expect_equal(wrapper_calculate_population_suppresion(current.insecticide.efficacy = 1,
                                                       currently.deployed.insecticide = 1,
                                                       vector.length = 1000,
                                                       current.generation = 2,
                                                       standard.deviation = 10,
                                                       female.insecticide.exposure = 1,
                                                       maximum.bioassay.survival.proportion = 1,
                                                       michaelis.menten.slope = 1,
                                                       half.population.bioassay.survival.resistance = 900,
                                                       regression.coefficient = 0,
                                                       regression.intercept = 0,
                                                       sim.array = sim.array,
                                                       population.suppression = TRUE), 0)
})

test_that("no suppression", {
  expect_equal(wrapper_calculate_population_suppresion(current.insecticide.efficacy = 1,
                                                       currently.deployed.insecticide = 1,
                                                       vector.length = 1000,
                                                       current.generation = 2,
                                                       standard.deviation = 10,
                                                       female.insecticide.exposure = 1,
                                                       maximum.bioassay.survival.proportion = 1,
                                                       michaelis.menten.slope = 1,
                                                       half.population.bioassay.survival.resistance = 900,
                                                       regression.coefficient = 1,
                                                       regression.intercept = 1,
                                                       sim.array = sim.array,
                                                       population.suppression = TRUE), 1)
})





test_that("only unexposed survive", {
  expect_equal(wrapper_calculate_population_suppresion(current.insecticide.efficacy = 1,
                                                       currently.deployed.insecticide = 1,
                                                       vector.length = 1000,
                                                       current.generation = 2,
                                                       standard.deviation = 10,
                                                       female.insecticide.exposure = 0.5,
                                                       maximum.bioassay.survival.proportion = 1,
                                                       michaelis.menten.slope = 1,
                                                       half.population.bioassay.survival.resistance = 900,
                                                       regression.coefficient = 0,
                                                       regression.intercept = 0,
                                                       sim.array = sim.array,
                                                       population.suppression = TRUE), 0.5)
})

