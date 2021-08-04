#' @title Convert the array from the simulation into a dataframe
#'
#' @description
#' Input the saved simulation array into this function to return a dataframe holding the information of regarding the
#' simulation. The dataframe has 5 columns. insecticide.tracked is the insecticide to which the resistance.score corresponds to
#' at the site (either refugia or intervention), at the timepoint time.in.generations. When the corresponding insecticide.deployed is the
#' insecticide that is deployed in the intervention site at at that generation.

#' @param simulation.array = the array which holds the simulation data called from run_simulation_intervention.
#' @param maximum.generations = The number of generations the simulation was asked to run for (maximum.generations)
#' @param number.of.insecticides = the number of insectcides that are tracked in the simulation
#'
#' @return final.df A dataframe that has 5 columns, insecticide.tracked, resistance.score, site,
#' time.in.generations and insecticide.deployed.




get_simulation_dataframe = function(simulation.array, maximum.generations, number.of.insecticides){

  data.list = list()


  sim.duration = length(simulation.array[[2]])

  for(insecticide in 1:number.of.insecticides){

    if(sim.duration >= maximum.generations){
      insecticide.tracked = as.character(rep(insecticide, times = (2 * maximum.generations))) # 2* as refugia and intervention

      generation.sequence = seq(1, maximum.generations, by = 1)
      time.in.generations = rep(generation.sequence, times = 2) # 2* as refugia and intervention

      resistance.score.refugia = simulation.array[[1]]["refugia", insecticide, ]
      resistance.score.refugia = head(resistance.score.refugia, n=maximum.generations)
      resistance.score.intervention = simulation.array[[1]]["intervention", insecticide, ]
      resistance.score.intervention = head(resistance.score.intervention, n=maximum.generations)
      resistance.score = c(resistance.score.refugia, resistance.score.intervention)

      site.refugia = rep("refugia", times = maximum.generations)
      site.intervention = rep("intervention", times = maximum.generations)
      site = c(site.refugia, site.intervention)

      deployed = simulation.array[[2]]
      deployed_temp = head(deployed, n = maximum.generations)
      insecticide.deployed = as.character(rep(deployed_temp, times = 2)) #2 times as refugia and intervention

      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.score,
                                           site,
                                           time.in.generations,
                                           insecticide.deployed)
    } else{          #Does sim.duration-1 as the final deployed.insecticide is NA which is when the simulation stops
      insecticide.tracked = as.character(rep(insecticide, times = (2 * (sim.duration-1)))) # 2* as refugia and intervention
      time.in.generations = rep(seq(1, (sim.duration-1), by = 1), times = 2) # 2* as refugia and intervention

      resistance.score = c(head(simulation.array[[1]]["refugia", insecticide, ], n=(sim.duration-1)), #first is refugia
                               head(simulation.array[[1]]["intervention", insecticide, ], n=(sim.duration-1))) #second is intervention

      site = c(rep("refugia", times =  (sim.duration-1)), #first is refugia
               rep("intervention", times = (sim.duration-1))) #second is intervention

      insecticide.deployed = as.character(rep(head(simulation.array[[2]], n=(sim.duration-1)), times = 2)) #2 times as refugia and intervention

      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.score,
                                           site,
                                           time.in.generations,
                                           insecticide.deployed)
    }
  }

  final_df = do.call(rbind, data.list)
  return(final_df)
}




#This function returns a dataframe that has 5 columns. And the number of rows is n*2 (refugia and intervention)
# times the duration. Where n is the number of insecticides included in the simulation.
