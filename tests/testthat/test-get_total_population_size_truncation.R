test_that("function works", {
  expect_equal(get_total_population_size_truncation(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), sum(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
})
