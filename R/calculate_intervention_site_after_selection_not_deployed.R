#'@title Calculate the mean Polygenic Resistance Score of the mosquito population in the intervention site after  fitness cost based selection.
#'
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention before selection has occurred that generation.
#'@param response.fitness = The response to selection from fitness costs only, such as would occur when the insecticide is either not deployed in the intervention site or the selection that occurs in the refugia.


calculate_intervention_site_after_selection_not_deployed = function(intervention.before.selection,
                                                                    response.fitness){


  intervention.after.selection = intervention.before.selection + response.fitness

  intervention.after.selection = ifelse(intervention.after.selection < 0,
                                        yes = 0,
                                        no =intervention.after.selection)

  return(intervention.after.selection)
}
