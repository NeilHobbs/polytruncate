

run_simulation_truncation = function(number.of.insecticides = 2,
                                 exposure.scaling.factor = 10,
                                 female.fitness.cost,
                                 male.fitness.cost,
                                 female.insecticide.exposure,
                                 male.insecticide.exposure,
                                 heritability,
                                 dispersal.rate,
                                 intervention.coverage,
                                 standard.deviation,
                                 vector.length,
                                 maximum.bioassay.survival.proportion,
                                 michaelis.menten.slope,
                                 regression.coefficient,
                                 regression.intercept,
                                 maximum.generations = 500,
                                 irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                 half.population.bioassay.survival.resistance = 900,
                                 withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                 return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                 deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                 maximum.resistance.value = 25000,
                                 starting.refugia.resistance.score = 0,
                                 starting.intervention.resistance.score = 0,
                                 applied.insecticide.dose,
                                 recommended.insecticide.dose,
                                 threshold.generations,
                                 base.efficacy.decay.rate,
                                 rapid.decay.rate,
                                 population.suppression,
                                 min.cross.selection,
                                 max.cross.selection
){

  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "intervention"), which hold resistance scores
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)

  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = starting.refugia.resistance.score,
                                             starting.intervention.resistance.score = starting.intervention.resistance.score,
                                             number.of.insecticides = number.of.insecticides)


  cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
                                                       min.cross.selection = min.cross.selection,
                                                       max.cross.selection = max.cross.selection)

  #Make a vector of the available insecticides@
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment.
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.

  deployed.insecticide = rep(1, times = deployment.frequency)#Always start with insecticide 1.
  #This is fine as all insecticides have equivalent properties.

  #Set the withdrawal and return bioassay survival thresholds.
  calc.withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                            bioassay.survival = withdrawal.threshold.value,
                                                                            estimate.precision = 0.001,
                                                                            minimum.resistance.value = 0,
                                                                            maximum.resistance.value = maximum.resistance.value)

  calc.return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                        bioassay.survival = return.threshold.value,
                                                                        estimate.precision = 0.001,
                                                                        minimum.resistance.value = 0,
                                                                        maximum.resistance.value = maximum.resistance.value)

  #Make a dataframe of the insecticide parameters:
  insecticide.parameters.df = create_insecticide_parameters_dataframe(number.of.insecticides = number.of.insecticides,
                                                                      applied.insecticide.dose = applied.insecticide.dose,
                                                                      recommended.insecticide.dose = recommended.insecticide.dose,
                                                                      threshold.generation = threshold.generations,
                                                                      base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                      rapid.decay.rate = rapid.decay.rate)


  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                   recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                   threshold.generations = insecticide.parameters.df[1,4],
                                                                   base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                   rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                   deployment.frequency = deployment.frequency)


  insecticide.info = list(available.vector, withdrawn.vector, deployed.insecticide)



  #Also worth considering turning the for generation and for insecticide loops into functions,
  #as the code is other wise very large and chunky and therefore complicated to edit and adapt.
  #start at generation 2, as generation 1 has intensities set at 0.
  for(generation in 2:maximum.generations){

    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.insecticide[generation])){break}else{

      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
        ##                                                   #ask whether insecticide is the same as deployed insecticide


        if(insecticide == deployed.insecticide[generation]){

          #The inclusion of the population suppression as a result of insecticidal deployment is causing some weird problems - appears to be restting to 0 after each new deployment.

          tracked.resistance = wrapper_intervention_refugia_deployed_dispersal_truncation(insecticide.population.suppression = wrapper_calculate_population_suppresion(current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                            currently.deployed.insecticide = deployed.insecticide[generation],
                                                                                                                                                            vector.length = vector.length,
                                                                                                                                                            current.generation = generation,
                                                                                                                                                            standard.deviation = standard.deviation,
                                                                                                                                                            female.insecticide.exposure = female.insecticide.exposure,
                                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                            regression.coefficient = regression.coefficient,
                                                                                                                                                            regression.intercept = regression.intercept,
                                                                                                                                                            sim.array = sim.array,
                                                                                                                                                            population.suppression = population.suppression),
                                                                               intervention.before.selection = sim.array['intervention', insecticide, generation-1],
                                                                               female.fitness.cost = female.fitness.cost,
                                                                               male.fitness.cost = male.fitness.cost,
                                                                               female.insecticide.exposure = female.insecticide.exposure,
                                                                               male.insecticide.exposure = male.insecticide.exposure,
                                                                               standard.deviation = standard.deviation,
                                                                               vector.length = vector.length,
                                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                               michaelis.menten.slope = michaelis.menten.slope,
                                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                               regression.coefficient = regression.coefficient,
                                                                               regression.intercept = regression.intercept,
                                                                               current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               heritability = heritability,
                                                                               refugia.before.selection = sim.array['refugia', insecticide, generation-1],
                                                                               dispersal.rate = dispersal.rate,
                                                                               intervention.coverage = intervention.coverage)

          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

        }

        if(insecticide != deployed.insecticide[generation]){

          tracked.resistance = wrapper_intervention_refugia_not_deployed_dispersal_truncation(insecticide.population.suppression = wrapper_calculate_population_suppresion(current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                                currently.deployed.insecticide = deployed.insecticide[generation],
                                                                                                                                                                vector.length = vector.length,
                                                                                                                                                                current.generation = generation,
                                                                                                                                                                standard.deviation = standard.deviation,
                                                                                                                                                                female.insecticide.exposure = female.insecticide.exposure,
                                                                                                                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                regression.coefficient = regression.coefficient,
                                                                                                                                                                regression.intercept = regression.intercept,
                                                                                                                                                                sim.array = sim.array,
                                                                                                                                                                population.suppression = population.suppression),
                                                                                   intervention.before.selection = sim.array['intervention', insecticide, generation-1],
                                                                                   female.fitness.cost = female.fitness.cost,
                                                                                   male.fitness.cost = male.fitness.cost,
                                                                                   heritability = heritability,
                                                                                   refugia.before.selection = sim.array['refugia', insecticide, generation-1],
                                                                                   dispersal.rate = dispersal.rate,
                                                                                   intervention.coverage = intervention.coverage,
                                                                                   cross.selection.matrix = cross.selection.matrix,
                                                                                   currently.deployed.insecticide = deployed.insecticide[generation],
                                                                                   currently.tracked.insecticide = insecticide,
                                                                                   female.insecticide.exposure = female.insecticide.exposure,
                                                                                   male.insecticide.exposure = male.insecticide.exposure,
                                                                                   standard.deviation = standard.deviation,
                                                                                   vector.length = vector.length,
                                                                                   maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                   michaelis.menten.slope = michaelis.menten.slope,
                                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                   regression.coefficient = regression.coefficient,
                                                                                   regression.intercept = regression.intercept,
                                                                                   current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                   exposure.scaling.factor = exposure.scaling.factor,
                                                                                   intervention.before.selection.other = sim.array['intervention', deployed.insecticide[generation], generation-1])

          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]


          #end insecticide not deployed
        }
        #NEED TO FIGURE OUT A WAY TO MAKE ALL THIS LESS MESSY AND MORE READABLE!!!!!!!!!!!
      }#end of for insecticide loop

      if(generation < maximum.generations){
        update.insecticide.info = if(generation %% deployment.frequency == 0){
          if(irm.strategy == "rotation"){
            irm_strategy_rotation(
              number.of.insecticides = number.of.insecticides,
              current.generation = generation,
              withdrawal.threshold = calc.withdrawal.threshold,
              return.threshold = calc.return.threshold,
              simulation.array = sim.array,
              available.vector = available.vector,
              withdrawn.vector = withdrawn.vector,
              current.insecticide = deployed.insecticide[generation],
              deployment.frequency = deployment.frequency,
              deployment.vector = deployed.insecticide)} else{
                if(irm.strategy == "sequence"){
                  irm_strategy_sequence(
                    number.of.insecticides = number.of.insecticides,
                    current.generation = generation,
                    withdrawal.threshold = calc.withdrawal.threshold,
                    return.threshold = calc.return.threshold,
                    simulation.array = sim.array,
                    available.vector = available.vector,
                    withdrawn.vector = withdrawn.vector,
                    current.insecticide = deployed.insecticide[generation],
                    deployment.frequency = deployment.frequency,
                    deployment.vector = deployed.insecticide)

                }
              }

          #update.insectide.info[[1]] is the vector of the available insecticides
          #update.insecticide.info[[2]] is the vector of the withdrawn insecticides
          #update.insecticide.info[[3]] is the vector of the whole deployment =c(previous.deployment, new.deployment)
        }
        if(generation %% deployment.frequency == 0){available.vector = update.insecticide.info[[1]]}
        if(generation %% deployment.frequency == 0){withdrawn.vector = update.insecticide.info[[2]]}
        if(generation %% deployment.frequency == 0){deployed.insecticide = update.insecticide.info[[3]]}
        if(generation %% deployment.frequency == 0){currently.deployed.insecticide = deployed.insecticide[generation+1]}

        if(generation %% deployment.frequency == 0){insecticide.efficacy.vector = c(insecticide.efficacy.vector,
                                                                                    create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[currently.deployed.insecticide, 2],
                                                                                                                       recommended.insecticide.dose = insecticide.parameters.df[currently.deployed.insecticide, 3],
                                                                                                                       threshold.generations = insecticide.parameters.df[currently.deployed.insecticide, 4],
                                                                                                                       base.efficacy.decay.rate = insecticide.parameters.df[currently.deployed.insecticide, 5],
                                                                                                                       rapid.decay.rate = insecticide.parameters.df[currently.deployed.insecticide, 6],
                                                                                                                       deployment.frequency = deployment.frequency))}


        #A break point to stop simuation if there is no insecticide deployed
        #if(is.na(deployed.insecticide[generation])){break}

      }
    }
  }#end of for(generation) loop

  #ensure the simulation array is return after running
  #need to develop an quick and easy way to turn array into dataframes for plotting purposes
  return(list(sim.array, deployed.insecticide, insecticide.efficacy.vector))
}






