temp.df = create_insecticide_parameters_dataframe(number.of.insecticides = 4,
                                        applied.insecticide.dose = c(1, 3, 4, 8),
                                        recommended.insecticide.dose = 1,
                                        threshold.generation = 1,
                                        base.efficacy.decay.rate = 4,
                                        rapid.decay.rate = 5)

test_that("correct number of rows", {
  expect_equal(nrow(temp.df), 4)
})


test_that("correct number of columns", {
  expect_equal(ncol(temp.df), 6)
})


test_that("unique doses", {
  expect_equal(temp.df$applied.insecticide.doses, c(1, 3, 4, 8))
})


test_that("unique insecticides", {
  expect_equal(temp.df$insecticides, c(1, 2, 3, 4))
})


test_that("threshold generation the same for all", {
  expect_equal(temp.df$threshold.generations, c(1, 1, 1, 1))
})
