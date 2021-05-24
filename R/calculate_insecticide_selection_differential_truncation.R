#'@title Calculate the selection differential as a result of insecticide selection.
#'
#'@param mean.post.selection = The mean Polygenic Resistance Score of the all the mosquitoes after insecticide selection has occurred.
#'@param trait.mean = The mean value of a polygenic trait.

calculate_insecticide_selection_differential_truncation = function(mean.post.selection,
                                                                   trait.mean){

  insecticide.selection.differential = mean.post.selection - trait.mean

    return(insecticide.selection.differential)
}
