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

  #field.survival must be between 0 and 1

  field.survival = ifelse(field.survival < 0,
                          yes = 0,
                          no = field.survival)

  field.survival = ifelse(field.survival > 1,
                          yes = 1,
                          no = field.survival)

  return(field.survival)
}
