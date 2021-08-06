#'@title Calculate the total relative population size of male mosquitoes in the intervention site before insecticide selection.
#'
#'@param total.population.size = The relative population size of the intervention site prior to selection.


calculate_total_male_population_size_truncation = function(total.population.size){

  total.male.population.size = total.population.size/2 #50:50 male female ratio at emergence


  return(total.male.population.size)
}
