#'@title Plot the expected selection differential for female mosquitoes.
#'


example_plot_female_insecticide_selection_differential_truncation = function(applied.dosage,
                                                                             recommended.dosage,
                                                                             precision,
                                                                             vector.length,
                                                                             standard.deviation,
                                                                             female.trait.mean,
                                                                             female.insecticide.exposure,
                                                                             half.population.bioassay.survival.resistance,
                                                                             regression.coefficient,
                                                                             regression.intercept){

  maximum.insecticide.efficacy = applied.dosage/recommended.dosage

  #create vector of insecticide efficacies:
  insecticide.efficacy.vector = seq(from = 0, to = maximum.insecticide.efficacy, by = precision)

  #create empty vector to hold selection differentials:
  selection.differentials = c()

  for(i in 1:length(insecticide.efficacy.vector)){

    selection.differentials[i] = wrapper_female_insecticide_selection_differential_truncation(vector.length = vector.length,
                                                                                              standard.deviation = standard.deviation,
                                                                                              female.insecticide.exposure = female.insecticide.exposure,
                                                                                              female.trait.mean = female.trait.mean,
                                                                                              maximum.bioassay.survival.proportion = 1,
                                                                                              michaelis.menten.slope = 1,
                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                              regression.coefficient = regression.coefficient,
                                                                                              regression.intercept = regression.intercept,
                                                                                              current.insecticide.efficacy = insecticide.efficacy.vector[i])}



  example.df = data.frame(selection.differentials, insecticide.efficacy.vector)

  example.plot =  ggplot(example.df, aes(x=insecticide.efficacy.vector, y=selection.differentials))+
    geom_point()+
    xlab("Insecicide Efficacy")+
    ylab("Female Insecticide Selection Differential")+
    theme_classic()

  return(example.plot)
}
#
# example_plot_female_insecticide_selection_differential_truncation(applied.dosage = 1.5,
#                                                                   recommended.dosage = 1,
#                                                                   precision = 0.001,
#                                                                   vector.length = 10000,
#                                                                   standard.deviation = 30,
#                                                                   female.trait.mean = 0,
#                                                                   female.insecticide.exposure = 0.1,
#                                                                   half.population.bioassay.survival.resistance = 900,
#                                                                   regression.coefficient = 0.48,
#                                                                   regression.intercept = 0.15)


