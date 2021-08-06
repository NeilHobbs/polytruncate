sim.array = create_starting_array(n.insecticides = 3,
                                  maximum.generations = 2)

sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                           starting.refugia.resistance.score = c(1, 3, 7),
                                           starting.intervention.resistance.score = c(2,5,9),
                                           number.of.insecticides = 3)


test_that("gives correct starting resistance", {
  expect_equal(sim.array["intervention", 1, 1], 2)
  expect_equal(sim.array["refugia", 1, 1], 1)
  expect_equal(sim.array["intervention", 2, 1], 5)
  expect_equal(sim.array["refugia", 2, 1], 3)
  expect_equal(sim.array["intervention", 3, 1], 9)
  expect_equal(sim.array["refugia", 3, 1], 7)
})

test_that("remain NA", {
  expect_equal(is.na(sim.array["intervention", 1, 2]), TRUE)
})
