#'@title Calculate the relative number of mosquitoes dispersing from the refugia to the intervention site.
#'
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.

number_migrating_refugia_to_intervention = function(dispersal.rate,
                                                    intervention.coverage){

  if(dispersal.rate > 1 | dispersal.rate < 0){stop("dipersal.rate must be between 0 and 1")}
  if(intervention.coverage > 1 | intervention.coverage < 0){stop("intervention.coverage must be between 0 and 1")}

    migration.refugia.to.intervention =  dispersal.rate * (1 - intervention.coverage)

  return(migration.refugia.to.intervention)
}
