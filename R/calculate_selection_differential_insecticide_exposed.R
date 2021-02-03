#' @title Calculate the selection differential between those exposed and not exposed to the insecticide.
#'
#' @param sd.population.resistance = The standard deviation of the resistance intensity in the population.
#' @param field.survival = The survival probability of individuals encountering the insecticide.


#This is equation 2d(iii) in base_model v2
calculate_selection_differential_insecticide_exposed = function(sd.population.resistance,
                                                                field.survival){

   selection.differential.insecticide.exposed =((dnorm(pnorm(1-field.survival)))/field.survival)*sd.population.resistance

  return(selection.differential.insecticide.exposed)
}
