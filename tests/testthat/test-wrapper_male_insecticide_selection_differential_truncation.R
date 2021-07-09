test_that("no insecticide no selection", {
  expect_equal(wrapper_male_insecticide_selection_differential_truncation(vector.length = 10000,
                                                                          standard.deviation = 20,
                                                                          male.insecticide.exposure = 0.5,
                                                                          female.insecticide.exposure = 0.5,
                                                                          male.trait.mean = 10,
                                                                          maximum.bioassay.survival.proportion = 1,
                                                                          michaelis.menten.slope = 1,
                                                                          half.population.bioassay.survival.resistance = 900,
                                                                          regression.coefficient = 0.48,
                                                                          regression.intercept = 0.15,
                                                                          current.insecticide.efficacy = 0), 0)
})
