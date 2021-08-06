sim.array = create_starting_array(n.insecticides = 2,
                                  maximum.generations = 2)


sim.array["intervention", 1, 1] = 12
sim.array["intervention", 2, 1] = 9

test_that("cannot be returned", {
  expect_equal(can_insecticide_be_returned_to_arsenal(insecticide = 1,
                                                      current.generation = 1,
                                                      return.threshold = 10,
                                                      simulation.array = sim.array), FALSE)
})

test_that("can be returned", {
  expect_equal(can_insecticide_be_returned_to_arsenal(insecticide = 2,
                                                      current.generation = 1,
                                                      return.threshold = 10,
                                                      simulation.array = sim.array), TRUE)
})
