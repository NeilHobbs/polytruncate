#'@title A function that obtains the population suppression caused by the insecticide mixture in the intervention site.

#'@param vector.length = The length of the vector used
#'@param current.generation = The generation the simulation is up to
#'@param standard.deviation = The standard deviation of the polygneic resistance score
#'@param maximum.bioassay.survival.proportion = maximum survival proportion in the bioassay, should be set at 1
#'@param michaelis.menten.slope = slope of the michaelis menten equation, should be set at 1
#'@param half.population.bioassay.survival.resistance = the polygenic resistance score which gives 50% bioassay survival
#'@param regression.coefficient = regression coefficient between field and bioassay survival, obtained from a linear model
#'@param regression.intercept = regression intercept between field and bioassay survival, obtained from a linear model
#'@param sim.array = the array holding the simulation
#'@param population.supression = Is this special case being included, TRUE or FALSE
#'@param deployed.mixture = the dataframe containing the deployed mixture information

wrapper_calculate_population_suppresion_mixtures = function(vector.length,
                                                            current.generation,
                                                            standard.deviation,
                                                            female.insecticide.exposure,
                                                            maximum.bioassay.survival.proportion,
                                                            michaelis.menten.slope,
                                                            half.population.bioassay.survival.resistance,
                                                            regression.coefficient,
                                                            regression.intercept,
                                                            sim.array,
                                                            population.suppression,
                                                            deployed.mixture){

  if(population.suppression == TRUE){

    trait.mean.deployed.1 = sim.array['intervention', deployed.mixture$mixture.part.1[current.generation], current.generation-1]
    trait.mean.deployed.2 = sim.array['intervention', deployed.mixture$mixture.part.2[current.generation], current.generation-1]

    field.survival.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                     trait.mean = trait.mean.deployed.1,
                                                                                                                                     michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                   regression.coefficient = regression.coefficient,
                                                                   regression.intercept = regression.intercept,
                                                                   current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[current.generation])

    field.survival.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                     trait.mean = trait.mean.deployed.2,
                                                                                                                                     michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                   regression.coefficient = regression.coefficient,
                                                                   regression.intercept = regression.intercept,
                                                                   current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[current.generation])



    relative.contributions.before.selection = calculate_density_of_trait_values(vector.length = vector.length,
                                                                                trait.mean = 0, #value does not matter
                                                                                standard.deviation = standard.deviation)

    total.female.population.size = get_female_total_population_size(
      total.population.size = get_total_population_size(
        relative.contributions.before.selection = relative.contributions.before.selection))

    ##Get the female population size that is unexposed to the insecticide:

    female.population.size.unexposed = calculate_female_population_size_unexposed(total.female.population.size = total.female.population.size,
                                                                                  female.insecticide.exposure = female.insecticide.exposure)

    female.population.size.exposed = total.female.population.size - female.population.size.unexposed

    female.population.size.exposed.survivors = female.population.size.exposed * field.survival.1 * field.survival.2



    #Then obtain the final relative population size in the intervention site (for females only):
    #This is obtained by unexposed + exposed.survivors
    female.population.size.after.selection = female.population.size.exposed.survivors + female.population.size.unexposed


    #Finally find the proportion of the population in the intervention site that survives.
    insecticide.population.suppression = 1 - (female.population.size.after.selection / total.female.population.size)

  }

  if(population.suppression == FALSE){insecticide.population.suppression = 0}

  return(insecticide.population.suppression)

}



