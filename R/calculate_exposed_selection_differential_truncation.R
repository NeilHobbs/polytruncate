#'@title Calculate the selection differential between the exposed and unexposed individuals
#'
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param field.survival = The survival probability in the field to the insecticide.



calculate_exposed_selection_differential_truncation = function(standard.deviation,
                                                               field.survival){



 exposed.selection.differential =  standard.deviation * (dnorm(qnorm(1-field.survival))/field.survival)

 #If NA then set selection differential to zero.
 exposed.selection.differential = ifelse(is.na(exposed.selection.differential),
                                         yes = 0,
                                         no = exposed.selection.differential)
 return(exposed.selection.differential)

}

