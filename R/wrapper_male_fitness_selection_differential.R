#'@title A wrapper function to calculate the fitness selection differential for male mosquitoes
#'
#'@param male.trait.mean = The mean Polygenic Resistance Score of male mosquitoes before selection has occurred.
#'@param male.fitness.cost = The fixed fitness costs associated with polygenic resistance for males

wrapper_male_fitness_selection_differential = function(male.trait.mean,
                                                       male.fitness.cost){



  male.trait.mean.after.fitness = calculate_male_trait_mean_after_fitness(male.trait.mean = male.trait.mean,
                                                                          male.fitness.cost = male.fitness.cost)

  male.fitness.selection.differential = calculate_male_fitness_selection_differential(male.trait.mean = male.trait.mean,
                                                                                      male.trait.mean.after.fitness = male.trait.mean.after.fitness)

  return(male.fitness.selection.differential)
}
