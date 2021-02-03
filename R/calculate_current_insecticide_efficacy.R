#' @title Calculate the current efficacy of the insecticide in deployment.
#' 
#' @description 
#' 
#' @param initial.applied.efficacy
#' @param generations.since.deployment
#' @param basal.decay.rate
#' @param rapid.decay.rate
#' @param cut.off.generations

calculate_current_insecticide_efficacy = function(initial.applied.efficacy,
                                                  generations.since.deployment,
                                                  basal.decay.rate,
                                                  rapid.decay.rate,
                                                  cut.off.generations){
  
  
  generations.after.cutoff = generations.since.deployment - cut.off.generations
  
    #Equation from Yakob, Dunning, Yan 2010.
        #Modified to allow for decay to speed up after a set time frame.
  #perhaps worth having slow decay for first X generations then rapid after that.
  #Rationale: Public Health Insecticides are designed to maintain a high efficacy then rapidly decay.
  
  if(generations.since.deployment <= cut.off.generations){
  current.efficacy = initial.applied.efficacy * exp(-((generations.since.deployment^2) * basal.decay.rate))
  }else{
    pre.cut.off = initial.applied.efficacy * exp(-((generations.since.deployment^2) * basal.decay.rate))
                                                                              
    current.efficacy = pre.cut.off * exp(-((generations.after.cutoff^2) * rapid.decay.rate))                                          
    
  }
  #Then rapid decay thereafter:
  
  
  return(current.efficacy)
  
}

##Test it works
# time.vec = seq(0, 9, by = 1)
# 
# efficacy = c()
# for(i in 1:length(time.vec))(
#   
#   efficacy[i] = calculate_current_insecticide_efficacy(initial.applied.efficacy = 1,
#                                                generations.since.deployment = time.vec[i],
#                                                basal.decay.rate = 0.01,
#                                                rapid.decay.rate = 0.2,
#                                                cut.off.generations = 5)
#   
# )
# 
# plot(x=time.vec, y = efficacy)
# 
# min(efficacy)


