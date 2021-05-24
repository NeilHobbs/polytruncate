#'@title Calculate the mean Polygenic Resistance Score of the all the mosquitoes after insecticide
#'selection has occurred.
#'
#'@param population.size.exposed.survivors = The relative population size of the mosquitoes in the intervention site who encountered the insecticide and survived.
#'@param mean.score.exposed.survivors = The mean Polygenic Resistance Score of the mosquitoes who encounter the insecticide and survive.
#'@param population.size.unexposed = The relative population size of the mosquitoes in the intervention site who did not encounter the insecticide.
#'@param trait.mean = The mean value of a polygenic trait before selection.
#'@param population.size.post.selection = The relative population size of all the mosquitoes in the intervention site after selection has occurred.


calculate_trait_mean_post_selection_truncation = function(population.size.exposed.survivors,
                                               mean.score.exposed.survivors,
                                               population.size.unexposed,
                                               trait.mean,
                                               population.size.post.selection){


  mean.score.post.selection = ((population.size.exposed.survivors*mean.score.exposed.survivors) +
                              (population.size.unexposed * trait.mean))/population.size.post.selection


  #Prevent the trait.mean falling below zero.
  mean.score.post.selection = ifelse(mean.score.post.selection < 0, yes = 0,
                                     no = mean.score.post.selection)

  return(mean.score.post.selection)
}
