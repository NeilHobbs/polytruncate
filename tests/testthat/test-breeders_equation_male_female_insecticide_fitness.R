test_that("equation works", {
  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 1,
                                                                 male.insecticide.fitness.selection.differential = 20,
                                                                 female.insecticide.fitness.selection.differential = 10), 15)

  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 0.5,
                                                                 male.insecticide.fitness.selection.differential = 20,
                                                                 female.insecticide.fitness.selection.differential = 10), 7.5)


  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 0,
                                                                 male.insecticide.fitness.selection.differential = 20,
                                                                 female.insecticide.fitness.selection.differential = 10), 0)


  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 1,
                                                                 male.insecticide.fitness.selection.differential = 20,
                                                                 female.insecticide.fitness.selection.differential = -20), 0)
})

test_that("heritability errors", {
  expect_error(breeders_equation_male_female_insecticide_fitness(heritability = -1,
                                                                 male.insecticide.fitness.selection.differential = 20,
                                                                 female.insecticide.fitness.selection.differential = 10), "heritability must be between 0 and 1")

  expect_error(breeders_equation_male_female_insecticide_fitness(heritability = 1.5,
                                                                 male.insecticide.fitness.selection.differential = 20,
                                                                 female.insecticide.fitness.selection.differential = 10), "heritability must be between 0 and 1")

})
