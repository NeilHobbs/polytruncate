#'@title Calculate the relative number of mosquitoes dispersing from the intervention site to the refugia.
#'
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.

number_migrating_intervention_to_refugia = function(dispersal.rate,
                                                    intervention.coverage){

  if(dispersal.rate > 1 | dispersal.rate < 0){stop("dipersal.rate must be between 0 and 1")}
  if(intervention.coverage > 1 | intervention.coverage < 0){stop("intervention.coverage must be between 0 and 1")}

  migration.intervention.to.refugia = dispersal.rate * intervention.coverage

return(migration.intervention.to.refugia)

  }
