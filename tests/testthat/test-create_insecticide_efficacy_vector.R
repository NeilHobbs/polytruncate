test_that("vector is correct length", {
  expect_equal(length(create_insecticide_efficacy_vector(applied.insecticide.dose = 1,
                                                  recommended.insecticide.dose = 1,
                                                  threshold.generations = 5,
                                                  base.efficacy.decay.rate = 0.4,
                                                  rapid.decay.rate = 0.8,
                                                  deployment.frequency = 10)), 10)
})


test_that("is deployed at correct dose", {
  expect_equal(create_insecticide_efficacy_vector(applied.insecticide.dose = 1,
                                                         recommended.insecticide.dose = 1,
                                                         threshold.generations = 5,
                                                         base.efficacy.decay.rate = 0.4,
                                                         rapid.decay.rate = 0.8,
                                                         deployment.frequency = 1), 1)
})
