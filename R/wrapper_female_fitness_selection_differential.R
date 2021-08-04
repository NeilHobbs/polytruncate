#'@title A wrapper function to calculate the fitness selection differential for female mosquitoes
#'
#'@param female.trait.mean = The mean Polygenic Resistance Score of female mosquitoes before selection has occurred.
#'@param female.fitness.cost = The fixed fitness costs associated with polygenic resistance for females

wrapper_female_fitness_selection_differential = function(female.trait.mean,
                                                         female.fitness.cost){



  female.trait.mean.after.fitness = calculate_female_trait_mean_after_fitness(female.trait.mean = female.trait.mean,
                                                                              female.fitness.cost = female.fitness.cost)

  female.fitness.selection.differential = calculate_female_fitness_selection_differential(female.trait.mean = female.trait.mean,
                                                                                          female.trait.mean.after.fitness = female.trait.mean.after.fitness)

  return(female.fitness.selection.differential)
}
