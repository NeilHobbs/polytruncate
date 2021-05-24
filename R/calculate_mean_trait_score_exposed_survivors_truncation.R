#'@title Calculate the mean trait score of the exposed survivors assuming truncation selection
#'
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation =The standard deviation of the trait mean in the population.
#'@param field.survival = The survival probability in the field to insecticide



calculate_mean_trait_score_exposed_survivors_truncation = function(trait.mean,
                                                                   standard.deviation,
                                                                   field.survival){

  exposed.selection.differential = calculate_exposed_selection_differential_truncation(standard.deviation = standard.deviation,
                                                                                       field.survival = field.survival)


  mean.trait.score.exposed.survivors = exposed.selection.differential + trait.mean

  return(mean.trait.score.exposed.survivors)
}
