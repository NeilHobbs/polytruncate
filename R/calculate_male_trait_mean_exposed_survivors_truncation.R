#'@title The mean Polygenic Resistance Score of the male mosquitoes exposed to and surviving the insecticide encounter.
#'
#'@param male.exposed.selection.differential = The selection differential between male mosquitoes that were exposed and unexposed to the insecticide assuming truncation selection
#'@param male.trait.mean = The mean Polygenic Resistance Score to insecticide i of male mosquitoes before selection has occurred.

calculate_male_trait_mean_exposed_survivors_truncation = function(male.exposed.selection.differential,
                                                                    male.trait.mean){

  male.trait.mean.exposed.survivors = male.exposed.selection.differential + male.trait.mean

  return(male.trait.mean.exposed.survivors)
}
