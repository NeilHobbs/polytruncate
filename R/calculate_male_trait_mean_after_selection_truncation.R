#'@title Calculate the male trait mean after insecticide selection assumming truncation selection.
#'
#'@param male.population.size.exposed.survivors = The relative population size of male mosquitoes that have encountered the insecticide and survived.
#'@param male.trait.mean.exposed.survivors = The relative population size of male mosquitoes that have encountered the insecticide and survived.
#'@param male.population.size.unexposed = The relative population size of the unexposed male mosquitoes in the intervention site.
#'@param male.trait.mean = The mean Polygenic Resistance Score to insecticide i of male mosquitoes before selection has occurred.
#'@param male.population.size.after.selection = The relative population size of all the male mosquitoes in the intervention site after insecticide selection.


calculate_male_trait_mean_after_selection_truncation = function(male.population.size.exposed.survivors,
                                                                male.trait.mean.exposed.survivors,
                                                                male.population.size.unexposed,
                                                                male.trait.mean,
                                                                male.population.size.after.selection){


male.trait.mean.after.selection = ((male.population.size.exposed.survivors * male.trait.mean.exposed.survivors)+
     (male.population.size.unexposed*male.trait.mean))/male.population.size.after.selection

  return(male.trait.mean.after.selection)
}
