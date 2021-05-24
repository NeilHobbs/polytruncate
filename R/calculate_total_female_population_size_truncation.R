#'@title Calculate the total relative population size of female mosquitoes in the intervention site before insecticide selection
#'
#'@param total.population.size = The relative population size of the intervention site prior to selection.

 calculate_total_female_population_size_truncation = function(total.population.size){

    total.female.population.size  =  total.population.size/2

    return(total.female.population.size)
  }
