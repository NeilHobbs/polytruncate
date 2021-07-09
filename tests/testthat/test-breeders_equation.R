test_that("equation works", {
  expect_equal(breeders_equation(selection.differential = 10,
                                 heritability = 0.3), 3)

  expect_equal(breeders_equation(selection.differential = -10,
                                 heritability = 0.3), -3)

  expect_equal(breeders_equation(selection.differential = 0,
                                 heritability = 0.3), 0)

  expect_equal(breeders_equation(selection.differential = 10,
                                 heritability = 0), 0)
})


test_that("error message", {
  expect_error(breeders_equation(selection.differential = 10,
                                 heritability = 1.1), "heritability must be between 0 and 1")

  expect_error(breeders_equation(selection.differential = 10,
                                 heritability = -0.1), "heritability must be between 0 and 1")

})
