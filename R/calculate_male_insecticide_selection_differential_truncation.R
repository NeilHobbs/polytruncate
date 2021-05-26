#'@title Calculate the male insecticide selection differential assuming truncation selection
#'
#'@param male.trait.mean.after.selection = The mean Polygenic Resistance Score to insecticide i of male mosquitoes after insecticide selection.
#'@param male.trait.mean =  The mean Polygenic Resistance Score to insecticide i of male mosquitoes before selection has occurred.

calculate_male_insecticide_selection_differential_truncation = function(male.trait.mean.after.selection,
                                                                        male.trait.mean){

  male.insecticide.selection.differential = male.trait.mean.after.selection - male.trait.mean

  return(male.insecticide.selection.differential)

}
