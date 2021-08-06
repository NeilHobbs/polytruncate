temp.matrix.0 = make_cross_selection_matrix(number.of.insecticides = 3,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0)

test_that("no cross selection", {
  expect_equal(c(temp.matrix.0), rep(0, 9))
})


temp.matrix.1 = make_cross_selection_matrix(number.of.insecticides = 3,
                                            min.cross.selection = 1,
                                            max.cross.selection = 1)

test_that("no cross selection", {
  expect_equal(c(temp.matrix.1), c(0, 1, 1, 1, 0, 1, 1, 1, 0))
})
