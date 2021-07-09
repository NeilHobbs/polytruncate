test_that("no insecticide efficacy, no selection differential", {
  expect_equal(wrapper_female_insecticide_selection_differential_truncation(vector.length= 100000,
                                                                            standard.deviation = 20,
                                                                            female.insecticide.exposure = 0.5,
                                                                            female.trait.mean = 10,
                                                                            maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1,
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            regression.coefficient = 1,
                                                                            regression.intercept = 1,
                                                                            current.insecticide.efficacy = 0), 0)
})
