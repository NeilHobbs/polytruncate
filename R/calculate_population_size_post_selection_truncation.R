#'@title Calculate the relative population size of all the mosquitoes in the intervention site after selection has occurred.
#'
#'@param population.size.exposed.survivors = The relative population size of the mosquitoes in the intervention site who encountered the insecticide and survived.
#'@param population.size.unexposed = The relative population size of the mosquitoes in the intervention site who did not encounter the insecticide.

calculate_population_size_post_selection_truncation = function(population.size.exposed.survivors,
                                                    population.size.unexposed){

  population.size.post.selection = population.size.exposed.survivors + population.size.unexposed

  return(population.size.post.selection)
}
