#'@title Calculate the selection differential for male mosquitoes from fitness costs
#'
#'@param male.trait.mean = The mean Polygenic Resistance Score to the insecticide for male mosquitoes before selection has occurred.
#'@param male.trait.mean.after.fitness = The mean male Polygenic Resistance Score of the population after fitness costs have been applied.

calculate_male_fitness_selection_differential = function(male.trait.mean,
                                                          male.trait.mean.after.fitness){


  male.fitness.selection.differential = male.trait.mean.after.fitness - male.trait.mean

  return(male.fitness.selection.differential)
}
