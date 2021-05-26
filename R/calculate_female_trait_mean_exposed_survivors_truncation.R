#'@title The mean Polygenic Resistance Score of the female mosquitoes exposed to and surviving the insecticide encounter.
#'
#'@param female.exposed.selection.differential = The selection differential between female mosquitoes that were exposed and unexposed to the insecticide assuming truncation selection
#'@param female.trait.mean = The mean Polygenic Resistance Score to insecticide i of female mosquitoes before selection has occurred.

calculate_female_trait_mean_exposed_survivors_truncation = function(female.exposed.selection.differential,
                                                                    female.trait.mean){

  female.trait.mean.exposed.survivors = female.exposed.selection.differential + female.trait.mean

return(female.trait.mean.exposed.survivors)
}
