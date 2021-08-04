#'@title Calculate the mean Polygenic Resistance Score of the mosquito population in the intervention site after insecticide and fitness cost based selection.
#'
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention before selection has occurred that generation.
#'@param response.insecticide.fitness = The response to selection from insecticide selection and fitness costs.


calculate_intervention_site_after_truncation_selection_deployed = function(intervention.before.selection,
                                                                response.insecticide.fitness){


  intervention.after.selection = intervention.before.selection + response.insecticide.fitness

  intervention.after.selection = ifelse(intervention.after.selection < 0,
                                        yes = 0,
                                        no= intervention.after.selection)

  return(intervention.after.selection)
}
