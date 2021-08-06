test_that("no response as not heritable", {
  expect_equal(wrapper_breeders_equation_male_female_fitness(heritability = 0,
                                                             trait.mean = 10,
                                                             female.fitness.cost = 8,
                                                             male.fitness.cost = 4), 0)
})

test_that("no response as no fitness cost", {
  expect_equal(wrapper_breeders_equation_male_female_fitness(heritability = 0,
                                                             trait.mean = 10,
                                                             female.fitness.cost = 0,
                                                             male.fitness.cost = 0), 0)
})
