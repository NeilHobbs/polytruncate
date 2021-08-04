#'@title Wrapper function to calculate the response when there is only fitness costs
#'
#'@param heritability = The heritability of a polygenic trait.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.
#'@param male.fitness.cost = The fixed fitness costs associated with polygenic resistance for males

wrapper_breeders_equation_male_female_fitness = function(heritability,
                                                         trait.mean,
                                                         female.fitness.cost,
                                                         male.fitness.cost){

  #Selection Differential for Females: Fitness Costs only
  female.fitness.selection.differential  = wrapper_female_fitness_selection_differential(female.trait.mean = trait.mean,
                                                   female.fitness.cost = female.fitness.cost)
  #Selection Differential for Males: Fitness Costs only
  male.fitness.selection.differential = wrapper_male_fitness_selection_differential(male.trait.mean = trait.mean,
                                                 male.fitness.cost = male.fitness.cost)

  #finally do the Breeder's Equation
  response.fitness =  breeders_equation_male_female_fitness(male.fitness.selection.differential = male.fitness.selection.differential,
                                        female.fitness.selection.differential = female.fitness.selection.differential,
                                        heritability)

  return(response.fitness)
}
