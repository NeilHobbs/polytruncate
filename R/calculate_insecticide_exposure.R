#' @title Calculate the exposure to the insecticide in the intervention site.
#'
#' @param female.insecticide.exposure
#' @param male.insecticide.exposure
#'

#This is equation 2B in base_modelv2
calculate_insecticide_exposure = function(female.insecticide.exposure,
                                          male.insecticide.exposure){


  overall.exposure = (female.insecticide.exposure + (female.insecticide.exposure*male.insecticide.exposure))/2

  return(overall.exposure)
}


