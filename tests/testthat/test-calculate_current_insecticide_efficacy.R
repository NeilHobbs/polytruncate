test_that("no decay", {
  expect_equal(calculate_current_insecticide_efficacy(generations.since.deployment = 5,
                                                      threshold.generations = 10,
                                                      initial.insecticide.efficacy = 1,
                                                      base.efficacy.decay.rate = 0,
                                                      rapid.decay.rate = 0), 1)
})



test_that("decay rate changes so no decay", {
  expect_equal(calculate_current_insecticide_efficacy(generations.since.deployment = 5,
                                                      threshold.generations = 5,
                                                      initial.insecticide.efficacy = 1,
                                                      base.efficacy.decay.rate = 0.1,
                                                      rapid.decay.rate = 0),
               calculate_current_insecticide_efficacy(generations.since.deployment = 6,
                                                      threshold.generations = 5,
                                                      initial.insecticide.efficacy = 1,
                                                      base.efficacy.decay.rate = 0.1,
                                                      rapid.decay.rate = 0))
})
