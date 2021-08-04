#'@title Calculate The Mean male Polygenic Resistance Score of the population after fitness costs have been applied.
#'
#'@param male.trait.mean = The mean Polygenic Resistance Score to the insecticide for male mosquitoes before selection has occurred.
#'@param male.fitness.cost = The fixed fitness cost associated with polygenic resistance for males.


calculate_male_trait_mean_after_fitness = function(male.trait.mean,
                                                   male.fitness.cost){

  male.trait.mean.after.fitness = male.trait.mean - male.fitness.cost

  return(male.trait.mean.after.fitness)
}
