#'@title Calculate the selection differential between female mosquitoes that were exposed and unexposed to the insecticide assuming truncation selection.
#'
#'@param field.survival = The survival probability in the field to the insecticide.
#'@param standard.deviation = The standard deviation of the trait mean in the population.



calculate_female_exposed_selection_differential_truncation = function(field.survival,
                                                                      standard.deviation){


  female.exposed.selection.differential  = standard.deviation * (dnorm(qnorm(1-field.survival))/field.survival)


  #if NA (therefore all die) set to zero
  female.exposed.selection.differential = ifelse(is.na(female.exposed.selection.differential),
                                                 yes = 0,
                                                 no = female.exposed.selection.differential)

  return(female.exposed.selection.differential)

}
