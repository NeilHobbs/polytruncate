#'@title Perform the basic Breeder's Equation
#'
#'@param selection.differential = The change in the value of a polygenic trait within a generation.
#'@param heritability = The heritability of a polygenic trait.

breeders_equation = function(selection.differential,
                             heritability){

  if(0 > heritability |heritability > 1){stop("heritability must be between 0 and 1")}

  response = selection.differential * heritability

  return(response)
}
