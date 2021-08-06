test_that("mean resitance cannot fall below 0", {
  expect_equal(wrapper_refugia_breeders_equation(refugia.before.selection = 1,
                                                 heritability = 1,
                                                 female.fitness.cost = 10,
                                                 male.fitness.cost = 10), 0)
})
