#'@title Convert bioassay survival to the expected field survival
#'
#'@param bioassay.survival = The probability of a mosquito with a Polygenic Resistance Unit Score of z surviving in a WHO cylinder bioassay to the corresponding insecticide.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The insecticide efficacy of the insecticide at time since deployment defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.


convert_bioassay_survival_to_field_survival = function(bioassay.survival,
                                                       regression.coefficient,
                                                       regression.intercept,
                                                       current.insecticide.efficacy){


  field.survival = ((bioassay.survival * regression.coefficient) + regression.intercept)^current.insecticide.efficacy

  return(field.survival)
}
