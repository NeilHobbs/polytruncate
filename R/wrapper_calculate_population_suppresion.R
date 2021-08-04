#A function that obtains the population suppression caused by the insecticide in the intervention site.


wrapper_calculate_population_suppresion = function(current.insecticide.efficacy,
                                                   currently.deployed.insecticide,
                                                   vector.length,
                                                   current.generation,
                                                   standard.deviation,
                                                   female.insecticide.exposure,
                                                   maximum.bioassay.survival.proportion,
                                                   michaelis.menten.slope,
                                                   half.population.bioassay.survival.resistance,
                                                   regression.coefficient,
                                                   regression.intercept,
                                                   sim.array,
                                                   population.suppression){

  if(population.suppression == TRUE){

  trait.mean.deployed = sim.array['intervention', currently.deployed.insecticide, current.generation-1]


  create_normal_distribution(vector.length = vector.length,
                             trait.mean = trait.mean.deployed,
                             standard.deviation = standard.deviation)

relative.contributions.before.selection = calculate_density_of_trait_values(vector.length = vector.length,
                                                                            trait.mean = trait.mean.deployed,
                                                                            standard.deviation = standard.deviation)

total.female.population.size = get_female_total_population_size(
  total.population.size = get_total_population_size(
    relative.contributions.before.selection = relative.contributions.before.selection))

##Get the female population size that is unexposed to the insecticide:

female.population.size.unexposed = calculate_female_population_size_unexposed(total.female.population.size = total.female.population.size,
                                                                              female.insecticide.exposure = female.insecticide.exposure)

##Get the female population size are the exposed survivors:
relative.female.contributions.after.selection = calculate_female_density_after_selection(
  female.insecticide.exposure = female.insecticide.exposure,
  vector.length = vector.length,
  trait.mean = trait.mean.deployed,
  standard.deviation = standard.deviation,
  maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
  michaelis.menten.slope = michaelis.menten.slope,
  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
  regression.coefficient = regression.coefficient,
  regression.intercept = regression.intercept,
  current.insecticide.efficacy = current.insecticide.efficacy
)

female.population.size.exposed.survivors = calculate_female_population_size_exposed_survivors(
  relative.female.contributions.after.selection = relative.female.contributions.after.selection)


#Then obtain the final relative population size in the intervention site (for females only):
  #This is obtained by unexposed + exposed.survivors
female.population.size.after.selection = calculate_female_population_size_after_selection(female.population.size.unexposed = female.population.size.unexposed,
                                                                                          female.population.size.exposed.survivors = female.population.size.exposed.survivors)



#Finally find the proportion of the population in the intervention site that survives.
proportion.surviving =  calculate_insecticide_population_suppression(female.population.size.after.selection = female.population.size.after.selection,
                                                                                  total.female.population.size = total.female.population.size)

#insecticide.population.suppression = 1 - proportion.surviving
insecticide.population.suppression =  proportion.surviving
}

if(population.suppression == FALSE){insecticide.population.suppression = 0}

return(insecticide.population.suppression)

}



