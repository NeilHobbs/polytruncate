test_that("function works", {
  expect_equal(calculate_female_trait_mean_exposed_survivors_truncation(female.exposed.selection.differential = 10,
                                                                        female.trait.mean = 6), 16)
})
