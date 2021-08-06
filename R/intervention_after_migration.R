#'@title Calculate the polygenic resistance score in the intervention site after mosquito dispersal
#'
#'@param intervention.after.selection = The polygenic resistance score to the insecticide in the intervention site after insecticide selection
#'@param staying.in.intervention = The relative number of mosquitoes staying in the intervention site
#'@param insecticide.population.suppression = The degree of population suppression as a result of insecticide deployment
#'@param joining.from.refugia = The relative number of mosquitoes joining from the refugia
#'@param refugia.after.selection = The polygenic resistance score to the insecticide in the refugia after selection.

intervention_after_migration = function(intervention.after.selection,
                                        staying.in.intervention,
                                        insecticide.population.suppression,
                                        joining.from.refugia,
                                        refugia.after.selection){


  numerator = (intervention.after.selection*staying.in.intervention*(1-insecticide.population.suppression)) + (joining.from.refugia*refugia.after.selection)
  denominator =  joining.from.refugia + (staying.in.intervention*(1-insecticide.population.suppression))

intervention.after.migration = numerator/denominator

intervention.after.migration = ifelse(is.na(intervention.after.migration),
                                      yes = 0,
                                      no = intervention.after.migration)

return(intervention.after.migration)
}
