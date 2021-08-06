#'@title Get the total relative population size of female mosquitoes in the intervention site before insecticide selection.
#'
#'@param total.population.size = The relative population size of the intervention site prior to selection.

get_female_total_population_size = function(total.population.size){

  total.female.population.size = total.population.size/2

  return(total.female.population.size)

}
