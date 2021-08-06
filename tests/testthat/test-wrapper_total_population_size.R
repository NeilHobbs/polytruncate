test_that("calculates pop size", {
  expect_equal(wrapper_total_population_size(standard.deviation = 1,
                                             vector.length = 1000)/10,
               wrapper_total_population_size(standard.deviation = 10,
                                             vector.length = 1000))
})
