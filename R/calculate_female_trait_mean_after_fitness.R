#'@title Calculate The Mean Female Polygenic Resistance Score of the population after fitness costs have been applied.
#'
#'@param female.trait.mean = The mean Polygenic Resistance Score to the insecticide for female mosquitoes before selection has occurred.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.


calculate_female_trait_mean_after_fitness = function(female.trait.mean,
                                                     female.fitness.cost){

  female.trait.mean.after.fitness = female.trait.mean - female.fitness.cost

  return(female.trait.mean.after.fitness)
}
