#'@title Calculate the relative population size of the unexposed female mosquitoes in the intervention site.
#'
#'@param total.female.population.size = The total relative population size of female mosquitoes in the intervention site before insecticide selection.
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.


calculate_female_population_size_unexposed_truncation = function(total.female.population.size,
                                                                 female.insecticide.exposure){

  female.population.size.unexposed = total.female.population.size * (1 - female.insecticide.exposure)

  return(female.population.size.unexposed)
}
