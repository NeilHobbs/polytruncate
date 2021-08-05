#'@title Calculate the selection differential between male mosquitoes that were exposed and unexposed to the insecticide assuming truncation selection.
#'
#'@param field.survival = The survival probability in the field to the insecticide.
#'@param standard.deviation = The standard deviation of the trait mean in the population.



calculate_male_exposed_selection_differential_truncation = function(field.survival,
                                                                    standard.deviation){


  male.exposed.selection.differential  = standard.deviation * (dnorm(qnorm(1-field.survival))/field.survival)


  #if absolutely all the males die there would be no selection differential too.
  male.exposed.selection.differential = ifelse(is.na(male.exposed.selection.differential),
                                               yes = 0,
                                               no = male.exposed.selection.differential)

  return(male.exposed.selection.differential)

}
