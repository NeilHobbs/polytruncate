test_that("equation works", {
  expect_equal(breeders_equation_male_female_fitness(male.fitness.selection.differential = -20,
                                                     female.fitness.selection.differential = -10,
                                                     heritability = 1), -15)

  expect_equal(breeders_equation_male_female_fitness(male.fitness.selection.differential = -20,
                                                     female.fitness.selection.differential = -10,
                                                     heritability = 0.5), -7.5)


  expect_equal(breeders_equation_male_female_fitness(male.fitness.selection.differential = -20,
                                                     female.fitness.selection.differential = - 20,
                                                     heritability = 0), 0)

  expect_equal(breeders_equation_male_female_fitness(male.fitness.selection.differential = -20,
                                                     female.fitness.selection.differential = 20,
                                                     heritability= 1), 0)
})

test_that("heritability errors works", {
  expect_error(breeders_equation_male_female_fitness(male.fitness.selection.differential = -20,
                                                     female.fitness.selection.differential = -10,
                                                     heritability = 1.1), "heritability must be between 0 and 1")

  expect_error(breeders_equation_male_female_fitness(male.fitness.selection.differential = -20,
                                                     female.fitness.selection.differential = -10,
                                                     heritability = -0.5), "heritability must be between 0 and 1")

})
