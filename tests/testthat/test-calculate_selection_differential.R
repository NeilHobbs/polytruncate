test_that("equation works", {
  expect_equal(calculate_selection_differential(trait.mean = 10,
                                                           trait.value.parents = 10), 0)

  expect_equal(calculate_selection_differential(trait.mean = 8,
                                                trait.value.parents = 10), 2)


  expect_equal(calculate_selection_differential(trait.mean = 12,
                                                trait.value.parents = 10), -2)
})
