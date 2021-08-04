#'@title Calculate the polygenic resistance score in the refugia after mosquito dispersal
#'
#'@param intervention.after.selection = The polygenic resistance score to the insecticide in the intervention site after insecticide selection
#'@param staying.in.intervention = The relative number of mosquitoes staying in the intervention site
#'@param insecticide.population.suppression = The degree of population suppression as a result of insecticide deployment
#'@param joining.from.refugia = The relative number of mosquitoes joining from the refugia
#'@param refugia.after.selection = The polygenic resistance score to the insecticide in the refugia after selection.

refugia_after_migration = function(intervention.after.selection,
                                   joining.from.intervetion,
                                   insecticide.population.suppression,
                                   refugia.after.selection,
                                   staying.in.refugia){


  numerator = (intervention.after.selection*joining.from.intervetion*(1-insecticide.population.suppression)) + (refugia.after.selection * staying.in.refugia)
  #denominator = (joining.from.intervetion * (1-insecticide.population.suppression))+staying.in.refugia


  refugia.after.migration = numerator#/denominator

  #Prevent NA occurring
  refugia.after.migration = ifelse(is.na(refugia.after.migration),
                                        yes = 0,
                                        no = refugia.after.migration)

  return(refugia.after.migration)
}
