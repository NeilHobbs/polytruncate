#'@title Calculate the male population size after insecticide selection assuming truncation selection.
#'
#'@param male.population.size.exposed.survivors = The relative population size of male mosquitoes that have encountered the insecticide and survived.
#'@param male.population.size.unexposed = The relative population size of the unexposed male mosquitoes in the intervention site.


calculate_male_population_size_after_selection_truncation = function(male.population.size.exposed.survivors,
                                                                     male.population.size.unexposed){

  male.population.size.after.selection = male.population.size.unexposed + male.population.size.exposed.survivors

  return(male.population.size.after.selection)
}
