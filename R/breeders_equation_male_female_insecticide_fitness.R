#'@title Calculate the response to selection from insecticide selection and fitness costs from males and females.
#'
#'@param heritability = The heritability of a polygenic trait.
#'@param male.insecticide.fitness.selection.differential = The selection differential for male mosquitoes following insecticide selection and fitness costs.
#'@param female.insecticide.fitness.selection.differential = The selection differential for female mosquitoes following insecticide selection and fitness costs.


breeders_equation_male_female_insecticide_fitness = function(heritability,
                                                             male.insecticide.fitness.selection.differential,
                                                             female.insecticide.fitness.selection.differential){


  response.insecticide.fitness = ((heritability / 2)*female.insecticide.fitness.selection.differential)+
                                 ((heritability / 2)*male.insecticide.fitness.selection.differential)


  return(response.insecticide.fitness)
}
