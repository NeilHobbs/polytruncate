#' @title Run the insecticide resistance management simulation for sequences and rotations.
#'
#' @description This is the main wrapper function that implements the running of the insecticide resistance management
#' simulations. Currently the simulations allows for the comparison of sequence and rotation strategies. At the moment,
#' each insecticide acts completely independently; such that there is no cross resistance and cross selection.
#' For the "sequence" irm.strategy a single insecticide is continually deployed until it reaches the threshold for withdrawal,
#' at this point the next insecticide is deployed (at the next deployment opportunity). For the "rotation" irm.strategy, the
#' insecticide is changed at each deployment interval. It is therefore not recommend to compare rotations and sequences when
#' the number.of.insecticides = 1; as this will mean that the rotation strategy lasts a single deployment duration.
#'
run_simulation_intervention= function(number.of.insecticides = 2,
                                      insecticide.scaling.factor = 10,
                                      sd.population.resistance = 5,
                                      relative.fitness = 0.9,
                                      heritability = 0.05,
                                      male.insecticide.exposure = 1,
                                      female.insecticide.exposure = 0.4,
                                      starting.intervention.intensity = 0,
                                      starting.refugia.intensity = 0,
                                      intervention.coverage = 0.1,
                                      dispersal.rate = 0.1,
                                      maximum.generations = 500,
                                      intercept = 0.15,
                                      conversion.factor = 0.48,
                                      irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                      half.population.bioassay.survival.resistance = 900,
                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                      deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                      maximum.resistance.value = 25000){

  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "intervention"), which hold resistance intensities.
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  #dim 4: which insecticide is currently deployed. Need to decide if it easier/better to have the deployed insecticide in the
  #array; or just as a separate vector as is currently used. I think as a separate vector as it is currently working through this method.
  #refugia is the refugia. intervention is the place where insecticides are the intervention site where insecticides are deployed.

  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)


  #Maybe create a separate function: set_starting_conditions() for the following chunk of code. In doing so;
  #be able to set each insecticide having a unique starting intensity. And would set the insecticide.info
  #and calculating the withdrawal and return thresholds.

if(length(starting.refugia.intensity) == 1){
  sim.array['refugia', , 1] = starting.refugia.intensity

}  else(

  #Set starting resistance intensities (fills in only the first row/generation). The other generations are set to NAs.
  for(i in 1:number.of.insecticides){
    sim.array['refugia', i , 1] = starting.refugia.intensity[i]
  })

if(length(starting.intervention.intensity) == 1){
  sim.array['intervention', , 1] = starting.intervention.intensity
}else(

  #intervention site starting resistance intensity (where the insecticide can be deployed)
  for(i in 1:number.of.insecticides){
    sim.array['intervention', i , 1] = starting.intervention.intensity[i]
  })

  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment.
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.

  deployed.insecticide = rep(1, times = deployment.frequency)#Always start with insecticide 1.
  #This is fine as all insecticides have equivalent properties.


  insecticide.info = list(available.vector, withdrawn.vector, deployed.insecticide)
  #Set the withdrawal and return thresholds: requires inputting the desired proportion of survival as input parameters. These will require
  #the user to input the half.population.bioassay.survival.resistance; the required thresholds; and maximum.resistance.value [this will be incase,
  #a user decides to use a high Z50 value]. But we should recommend the Z50 to be 900.


  calc.withdrawal.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                              bioassay.survival = withdrawal.threshold.value,
                                                              estimate.precision = 0.001,
                                                              sd.population.resistance = sd.population.resistance,
                                                              number.bioassays = 10000,#set high to calculate true mean
                                                              minimum.resistance.value = 0,
                                                              maximum.resistance.value = maximum.resistance.value)

  calc.return.threshold = bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                                          michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          bioassay.survival = return.threshold.value,
                                                          estimate.precision = 0.001,
                                                          sd.population.resistance = 0,
                                                          number.bioassays =  10000, #set high to calculate true mean
                                                          minimum.resistance.value = 0,
                                                          maximum.resistance.value = maximum.resistance.value)

#This is just a temporary fix to check main code works
  tracked.insecticide.efficacy = 1
  #Also worth considering turning the for generation and for insecticide loops into functions,
  #as the code is other wise very large and chunky and therefore complicated to edit and adapt.
  #start at generation 2, as generation 1 has intensities set at 0.
  for(generation in 2:maximum.generations){

    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.insecticide[generation])){break}else{

      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
                    ##                                                   #ask whether insecticide is the same as deployed insecticide
        sim.array['intervention', insecticide, generation] = if(insecticide == deployed.insecticide[generation]){#Insecticide is deployed in intervention site
          #calculate population mean from previous population mean when insecticide present
          perform_intervention_tracking_insecticide_present(dispersal.rate = dispersal.rate,
                                                            intervention.coverage = intervention.coverage,
                                                            female.insecticide.exposure = female.insecticide.exposure,
                                                            male.insecticide.exposure = male.insecticide.exposure,
                                                            current.intervention.intensity = sim.array['intervention', insecticide, generation-1],
                                                            current.refugia.intensity =  sim.array['refugia', insecticide, generation-1],
                                                            sd.population.resistance = sd.population.resistance,
                                                            conversion.factor = conversion.factor,
                                                            intercept = intercept,
                                                            current.insecticide.efficacy = tracked.insecticide.efficacy,#NEED TO FIGURE OUT HOW TO BRING THIS IN TO THE SIMULATIONS
                                                            insecticide.scaling.factor = insecticide.scaling.factor,
                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                            relative.fitness = relative.fitness,
                                                            heritability = heritability)}
        else( #insecticide is not deployed
          #calculate population mean when insecticide not deployed from previous population mean
          perform_intervention_tracking_insecticide_absent(dispersal.rate = dispersal.rate,
                                                           intervention.coverage = intervention.coverage,
                                                           female.insecticide.exposure = female.insecticide.exposure,
                                                           male.insecticide.exposure = male.insecticide.exposure,
                                                           current.intervention.intensity =  sim.array['intervention', insecticide, generation-1],
                                                           current.refugia.intensity = sim.array['refugia', insecticide, generation-1],
                                                           sd.population.resistance = sd.population.resistance,
                                                           conversion.factor = conversion.factor,
                                                           intercept = intercept,
                                                           current.insecticide.efficacy = tracked.insecticide.efficacy,#NEED TO FIGURE OUT HOW TO BRING THIS IN TO THE SIMULATIONS
                                                           insecticide.scaling.factor = insecticide.scaling.factor,
                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                           relative.fitness = relative.fitness,
                                                           heritability = heritability))#end insecticide not deployed

      #Do refugia second, updating each generation for each insecticide
        #calculate the population mean from the previous population mean
        sim.array['refugia', insecticide, generation] = if(insecticide == deployed.insecticide[generation]){#Insecticide is deployed in intervention site
          perform_refugia_tracking_present(dispersal.rate = dispersal.rate,
                                           intervention.coverage = intervention.coverage,
                                           female.insecticide.exposure = female.insecticide.exposure,
                                           male.insecticide.exposure = male.insecticide.exposure,
                                           current.intervention.intensity =  sim.array['intervention', insecticide, generation-1],
                                           current.refugia.intensity = sim.array['refugia', insecticide, generation-1],
                                           sd.population.resistance = sd.population.resistance,
                                           conversion.factor = conversion.factor,
                                           intercept = intercept,
                                           relative.fitness = relative.fitness,
                                           heritability = heritability,
                                           current.insecticide.efficacy = tracked.insecticide.efficacy,
                                           insecticide.scaling.factor = insecticide.scaling.factor,
                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)

          } #end of insecticide deployed
        else( #insecticide is not deployed
          perform_refugia_tracking_absent(dispersal.rate = dispersal.rate,
                                          intervention.coverage = intervention.coverage,
                                          female.insecticide.exposure = female.insecticide.exposure,
                                          male.insecticide.exposure = male.insecticide.exposure,
                                          current.intervention.intensity = sim.array['intervention', insecticide, generation-1],
                                          current.refugia.intensity = sim.array[['refugia', insecticide, generation-1]],
                                          sd.population.resistance = sd.population.resistance,
                                          conversion.factor = conversion.factor,
                                          intercept = intercept,
                                          current.insecticide.efficacy = tracked.insecticide.efficacy,
                                          insecticide.scaling.factor = insecticide.scaling.factor,
                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                          heritability = heritability,
                                          relative.fitness = relative.fitness)
        )#end insecticide not deployed

        #end of refugia

      }}#end of forinsecticide loop

    #returns the mean population insecticide resistance each generation.


    #Which irm.strategy is being used: sequence or rotation

    #May be worth making the following chunk of code into its own function as it is a bit chunky
    #at the moment.
    #Update insecticide each time the deployment.frequency is reached:
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
    }
    #A break point to stop simuation if there is no insecticide deployed
    #if(is.na(deployed.insecticide[generation])){break}

  }#end of for(generation) loop

  #ensure the simulation array is return after running
  #need to develop an quick and easy way to turn array into dataframes for plotting purposes
  return(list(sim.array, deployed.insecticide))
}


# test_simulation = run_simulation_intervention(number.of.insecticides = 2,
#                        exposure.scaling.factor = 10,
#                        nsim = 1000,
#                         minimum.insecticide.resistance.heritability = 0.3,
#                                maximum.insecticide.resistance.heritability = 0.30,
#                                minimum.male.insecticide.exposure = 1,
#                                maximum.male.insecticide.exposure = 1,
#                                minimum.female.insecticide.exposure = 0.9,
#                                maximum.female.insecticide.exposure = 0.9,
#                                resistance.cost = 0,
#                                starting.intervention.intensity = 0,
#                                starting.refugia.intensity = 0,
#                                min.intervention.coverage = 0.1,
#                                max.intervention.coverage = 0.9,
#                                min.dispersal.rate = 0.1,
#                                max.dispersal.rate = 0.1,
#                                maximum.generations = 500,
#                                irm.strategy = sequence, #will be sequence or rotation (plus mixture later on),
#                                half.population.bioassay.survival.resistance = 900,
#                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                maximum.resistance.value = 25000 #have arbitrarily high just in case
#
# )
# #
# # test_simulation[[1]]
# # test_simulation[[2]]
# # print(test_simulation)






