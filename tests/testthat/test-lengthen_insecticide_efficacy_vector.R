
vector.1 = seq(1, 10)
vector.2 = seq(11, 20)

test_that("multiplication works", {
  expect_equal(lengthen_insecticide_efficacy_vector(previous.efficacy.vector = vector.1,
                                                    new.efficacy.vector = vector.2), seq(1, 20))
})
