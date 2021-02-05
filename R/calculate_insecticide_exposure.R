#' @title Calculate the exposure to the insecticide in the intervention site.
#'
#' @param female.insecticide.exposure = The proportion of female mosquitoes exposed to the insecticide
#' @param male.insecticide.exposure = The proportion of male mosquitoes exposed to the insecticide as a proportion of the female
#'
#'@return The overall proportion of mosquitoes exposed.

#This is equation 2B in base_modelv2
calculate_insecticide_exposure = function(female.insecticide.exposure,
                                          male.insecticide.exposure){


  overall.exposure = (female.insecticide.exposure + (female.insecticide.exposure*male.insecticide.exposure))/2

  return(overall.exposure)
}


