#Param descriptions

#' @param current.resistance.intensity = The current resistance intensity in the location where the insecticide selection is occurring.
#' @param sd.population.resistance = The standard deviation of the resistance intensity values in the mosquito population
#' @param half.population.bioassay.survival.resistance = The resistance intensity that would give 50% survival in the bioassay.
#' @param conversion.factor = The regression coefficient to convert bioassay survival to field survival
#' @param intercept = The intercept from the linear regression to convert bioassay survival to field survival
#' @param female.insecticide.exposure = The proportion of female mosquitoes exposed to the insecticide
#' @param male.insecticide.exposure = The proportion of male mosquitoes exposed to the insecticide as a proportion of the female exposure.
#' @param current.insecticide.efficacy = The current efficacy of the insecticide in deployment.

#' @param desired.resistance The value you want a survival value to correspond to
#' @param desired.survival.proportion The survival proportion you want your desired.resistance to have. Values must be between 0 and 1
#' @param maximum.bioassay.survival.proportion Must be set at 1
#' @param michaelis.menten.slope Must be set at 1
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param estimate.accuracy Number of replications of the rnorm function. Recommended value = 1000 for a reliable mean value.
#' @param minimum.resistance.value Recommend setting to 0. Must be lower than half survival resistance.
#' @param maximum.resistance.value Depends on your scale. Recommend setting arbitrarily high (10000). Must be higher than half resistance survival
#'
#' @param applied.dose = The efficacy of the applied insecticide dose.
#' @param recommended.dose = The recommended efficacy of insecticide dose. Should normally be set at 1.
#' @param relative.fitness = The relative fitness of the most resistant individuals in the population compared to the least resistant individuals in the population.
#'
#' @param initial.applied.efficacy = The efficacy of the initial application of the insecticide dose.
#' @param generations.since.deployment = The number of generations the insecticide dose has been in deployment
#' @param basal.decay.rate = The base decay rate of the insecticide
#' @param rapid.decay.rate = The rate of insecticide decay when the cut off threshold has been exceeded
#' @param cut.off.generations = The number of generations until the decay rate changes.
#'
#' @param bioassay.survival = The survival that occured in the bioassay, as a proportion(values must be between 0 and 1). Where 1=all survived, 0=all died
#'
#' #' @param ... = named dimensions to create in array e.g. (name1=c('a','b','c), name2=c('x','y'))
