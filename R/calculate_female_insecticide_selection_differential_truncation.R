#'@title Calculate the insecticide selection differential for female mosquitoes assuming truncation selection.
#'
#'@param female.trait.mean.after.selection = The mean Polygenic Resistance Score to insecticide i of the female mosquitoes after insecticide selection.
#'@param female.trait.mean = The mean Polygenic Resistance Score to insecticide i of female mosquitoes before selection has occurred.

calculate_female_insecticide_selection_differential_truncation = function(female.trait.mean.after.selection,
                                                                          female.trait.mean){

  female.insecticide.selection.differential = female.trait.mean.after.selection - female.trait.mean

  return(female.insecticide.selection.differential)
}
