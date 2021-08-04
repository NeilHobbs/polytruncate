#'@title Calculate the selection differential for female mosquitoes from fitness costs
#'
#'@param female.trait.mean = The mean Polygenic Resistance Score to the insecticide for female mosquitoes before selection has occurred.
#'@param female.trait.mean.after.fitness = The Mean Female Polygenic Resistance Score of the population after fitness costs have been applied.

calculate_female_fitness_selection_differential = function(female.trait.mean,
                                                           female.trait.mean.after.fitness){


  female.fitness.selection.differential = female.trait.mean.after.fitness - female.trait.mean

  return(female.fitness.selection.differential)
}
