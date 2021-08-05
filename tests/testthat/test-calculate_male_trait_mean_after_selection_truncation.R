test_that("equation works", {
  expect_equal(calculate_male_trait_mean_after_selection_truncation(male.population.size.exposed.survivors = 1000,
                                                                    male.trait.mean.exposed.survivors = 100,
                                                                    male.population.size.unexposed = 1000,
                                                                    male.trait.mean = 100,
                                                                    male.population.size.after.selection = 2000), 100)

  expect_equal(calculate_male_trait_mean_after_selection_truncation(male.population.size.exposed.survivors = 1000,
                                                                    male.trait.mean.exposed.survivors = 200,
                                                                    male.population.size.unexposed = 1000,
                                                                    male.trait.mean = 100,
                                                                    male.population.size.after.selection = 2000), 150)



})
