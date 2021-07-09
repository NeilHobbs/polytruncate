test_that("opposite positions equal", {
  expect_equal(calculate_density_of_trait_values(vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 5)[1],
               calculate_density_of_trait_values(vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 5)[1000])
})



test_that("middle values equal", {
  expect_equal(calculate_density_of_trait_values(vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 5)[500],
               calculate_density_of_trait_values(vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 5)[501])
})
