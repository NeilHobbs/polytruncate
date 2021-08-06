test_that("equation works", {
  expect_equal(calculate_total_male_population_size_truncation(total.population.size = 10000), 5000)
})
