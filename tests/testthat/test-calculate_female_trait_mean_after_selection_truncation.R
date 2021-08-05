test_that("equation works", {
  expect_equal(calculate_female_trait_mean_after_selection_truncation(female.population.size.exposed.survivors = 1000,
                                                                      female.trait.mean.exposed.survivors =100,
                                                                      female.population.size.unexposed = 500,
                                                                      female.trait.mean = 100,
                                                                      female.population.size.after.selection = 1500), 100)

  expect_equal(calculate_female_trait_mean_after_selection_truncation(female.population.size.exposed.survivors = 1000,
                                                                      female.trait.mean.exposed.survivors =100,
                                                                      female.population.size.unexposed = 1000,
                                                                      female.trait.mean = 50,
                                                                      female.population.size.after.selection = 2000), 75)

  expect_equal(calculate_female_trait_mean_after_selection_truncation(female.population.size.exposed.survivors = 0,
                                                                      female.trait.mean.exposed.survivors =100,
                                                                      female.population.size.unexposed = 500,
                                                                      female.trait.mean = 100,
                                                                      female.population.size.after.selection = 500), 100)
})
