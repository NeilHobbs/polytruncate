test_that("equation works", {
  expect_equal(calculate_trait_mean_post_selection_truncation(population.size.exposed.survivors = 1000,
                                                              mean.score.exposed.survivors = 100,
                                                              population.size.unexposed = 1000,
                                                              trait.mean = 100,
                                                              population.size.post.selection = 2000), 100)

  expect_equal(calculate_trait_mean_post_selection_truncation(population.size.exposed.survivors = 0,
                                                              mean.score.exposed.survivors = 0,
                                                              population.size.unexposed = 1000,
                                                              trait.mean = 10,
                                                              population.size.post.selection = 1000), 10)

  expect_equal(calculate_trait_mean_post_selection_truncation(population.size.exposed.survivors = 1000,
                                                              mean.score.exposed.survivors = 200,
                                                              population.size.unexposed = 1000,
                                                              trait.mean = 100,
                                                              population.size.post.selection = 2000), 150)
})


test_that("cannot fall below 0 works", {
  expect_equal(calculate_trait_mean_post_selection_truncation(population.size.exposed.survivors = 1000,
                                                              mean.score.exposed.survivors = -100,
                                                              population.size.unexposed = 1000,
                                                              trait.mean = -100,
                                                              population.size.post.selection = 2000), 0)
  })
