#'@title Calculate the selection differential
#'
#'@param trait.mean = The mean value of a polygenic trait before selection.
#'@param trait.value.parents = The mean polygenic trait value in the parents of the next generation.

calculate_selection_differential = function(trait.mean,
                                            trait.value.parents){

  selection.differential = trait.value.parents - trait.mean

  return(selection.differential)
}
