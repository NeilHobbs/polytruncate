#'@title Calculate The mean Polygenic Resistance Score to insecticide i of the female mosquitoes after insecticide selection.
#'
#'@param female.population.size.exposed.survivors = The mean Polygenic Resistance Score of the female mosquitoes exposed to and surviving the insecticide encounter.
#'@param female.trait.mean.exposed.survivors = The relative population size of female mosquitoes that have encountered the insecticide and survived.
#'@param female.population.size.unexposed = The relative population size of the unexposed female mosquitoes in the intervention site
#'@param female.trait.mean = The mean Polygenic Resistance Score to insecticide i of female mosquitoes before selection has occurred.
#'@param female.population.size.after.selection = The relative population size of all the female mosquitoes in the intervention site after insecticide selection.


calculate_female_trait_mean_after_selection_truncation = function(female.population.size.exposed.survivors,
                                                                  female.trait.mean.exposed.survivors,
                                                                  female.population.size.unexposed,
                                                                  female.trait.mean,
                                                                  female.population.size.after.selection){



  female.trait.mean.after.selection = ((female.population.size.exposed.survivors*female.trait.mean.exposed.survivors)+
     (female.population.size.unexposed*female.trait.mean))/female.population.size.after.selection


  return(female.trait.mean.after.selection)

}
