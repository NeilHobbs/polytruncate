test_that("equation works", {
  expect_equal(breeders_equation_male_female(heritability = 1,
                                             female.selection.differential = 10,
                                             male.selection.differential = 20), 15)

  expect_equal(breeders_equation_male_female(heritability = 0.5,
                                             female.selection.differential = 10,
                                             male.selection.differential = 20), 7.5)


  expect_equal(breeders_equation_male_female(heritability = 0,
                                             female.selection.differential = 10,
                                             male.selection.differential = 20), 0)


  expect_equal(breeders_equation_male_female(heritability = 1,
                                             female.selection.differential = 0,
                                             male.selection.differential = 0), 0)
})

test_that("heritability error works", {
  expect_error(breeders_equation_male_female(heritability = -0.1,
                                             female.selection.differential = 10,
                                             male.selection.differential = 20), "heritability must be between 0 and 1")

  expect_error(breeders_equation_male_female(heritability = 1.5,
                                             female.selection.differential = 10,
                                             male.selection.differential = 20), "heritability must be between 0 and 1")

})
