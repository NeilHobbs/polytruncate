test_that("equation works", {
  expect_equal(calculate_male_trait_mean_exposed_survivors_truncation(male.exposed.selection.differential = 10,
                                                                                 male.trait.mean = 10), 20)
})
