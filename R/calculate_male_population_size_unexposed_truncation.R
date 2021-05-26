#'@title Calculate the male population unexposed to the insecticide
#'
#'@param total.male.population.size = The total relative population size of male mosquitoes in the intervention site before insecticide selection.
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide as a proportion of the exposure of female mosquitoes
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.


calculate_male_population_size_unexposed_truncation = function(total.male.population.size,
                                                               male.insecticide.exposure,
                                                               female.insecticide.exposure){


  male.population.size.unexposed = total.male.population.size * (1 - (male.insecticide.exposure * female.insecticide.exposure))


  return(male.population.size.unexposed)
}
