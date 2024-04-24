library(devtools)
load_all()
library(patchwork)

impact_of_decay_simulations = function(dispersal,
                                       coverage,
                                       fitness.cost,
                                       exposure,
                                       heritability,
                                       init.i,
                                       init.j){

base.decay.rate.i = rep(rep(rep(c(0.005, 0.015, 0.025), 3), 3), 3)
base.decay.rate.j = rep(rep(rep(c(0.005, 0.015, 0.025), each = 3), 3), 3)
cross.resistance = rep(rep(c(-0.3, 0, 0.3), each = 9), 3)
dose = rep(c(0.5, 0.75, 1), each = 27)

param.df = data.frame(cross.resistance, base.decay.rate.i, base.decay.rate.j, dose)

mixture.list = list()

for(v in 1:81){

  temp = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                        exposure.scaling.factor = 1,
                                                                                        female.fitness.cost = fitness.cost,
                                                                                        male.fitness.cost = fitness.cost,
                                                                                        female.insecticide.exposure = exposure,
                                                                                        male.insecticide.exposure = 1,
                                                                                        heritability = heritability,
                                                                                        dispersal.rate = dispersal,
                                                                                        intervention.coverage = coverage,
                                                                                        standard.deviation = 20,
                                                                                        vector.length = 1000,
                                                                                        maximum.bioassay.survival.proportion = 1,
                                                                                        michaelis.menten.slope = 1,
                                                                                        regression.coefficient = 0.48,
                                                                                        regression.intercept = 0.15,
                                                                                        maximum.generations = 500,
                                                                                        irm.strategy = "sequence",
                                                                                        half.population.bioassay.survival.resistance = 900,
                                                                                        withdrawal.threshold.value = 0.1,
                                                                                        return.threshold.value = 0.08,
                                                                                        deployment.frequency = 30, #minimum deployment frequency
                                                                                        maximum.resistance.value = 25000,
                                                                                        starting.refugia.resistance.score = c(init.i, init.j),
                                                                                        starting.intervention.resistance.score = c(init.i, init.j),
                                                                                        applied.insecticide.dose = dose[v],
                                                                                        recommended.insecticide.dose = 1,
                                                                                        threshold.generations = 15,#no decay, so this value does not matter
                                                                                        base.efficacy.decay.rate = c(base.decay.rate.i[v],
                                                                                                                     base.decay.rate.j[v]),
                                                                                        rapid.decay.rate = 0.08,
                                                                                        population.suppression = FALSE,
                                                                                        min.cross.selection = cross.resistance[v],
                                                                                        max.cross.selection = cross.resistance[v],
                                                                                        deployment.type = "mixtures",
                                                                                        mixture.strategy = "mix.sequential.discrete"),
                                           maximum.generations = 500, number.of.insecticides = 2)

  temp = subset(temp, site == "intervention")

  temp$dose = dose[v]
  temp$base.decay.i = base.decay.rate.i[v]
  temp$base.decay.j = base.decay.rate.j[v]
  temp$cross.resistance = cross.resistance[v]
  temp$sim.replicate = v


  mixture.list[[v]] = temp

  print(v)
}


mixture.df = do.call("rbind", mixture.list)

mixture.df.1 = mixture.df|>
  dplyr::group_by(sim.replicate)|>
  dplyr::summarise(max.gen = max(time.in.generations))


mixture.df.1 = data.frame(mixture.df.1,
                          base.decay.rate.i,
                          base.decay.rate.j,
                          cross.resistance,
                          dose)

#monotherapy comparisons

base.decay.rate.i = rep(rep(c(0.005, 0.015, 0.025), 3), 3)
base.decay.rate.j = rep(rep(c(0.005, 0.015, 0.025), each = 3), 3)
cross.resistance = rep(c(-0.3, 0, 0.3), each = 9)
dose = rep(1, 27)

param.df = data.frame(cross.resistance, base.decay.rate.i, base.decay.rate.j, dose)

monotherapy.list = list()

for(v in 1:27){

  temp = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                        exposure.scaling.factor = 1,
                                                                                        female.fitness.cost = fitness.cost,
                                                                                        male.fitness.cost = fitness.cost,
                                                                                        female.insecticide.exposure = exposure,
                                                                                        male.insecticide.exposure = 1,
                                                                                        heritability = heritability,
                                                                                        dispersal.rate = dispersal,
                                                                                        intervention.coverage = coverage,
                                                                                        standard.deviation = 20,
                                                                                        vector.length = 1000,
                                                                                        maximum.bioassay.survival.proportion = 1,
                                                                                        michaelis.menten.slope = 1,
                                                                                        regression.coefficient = 0.48,
                                                                                        regression.intercept = 0.15,
                                                                                        maximum.generations = 500,
                                                                                        irm.strategy = "sequence", #best performer
                                                                                        half.population.bioassay.survival.resistance = 900,
                                                                                        withdrawal.threshold.value = 0.1,
                                                                                        return.threshold.value = 0.08,
                                                                                        deployment.frequency = 30, #minimum deployment frequency
                                                                                        maximum.resistance.value = 25000,
                                                                                        starting.refugia.resistance.score = c(init.i, init.j),
                                                                                        starting.intervention.resistance.score = c(init.i, init.j),
                                                                                        applied.insecticide.dose = 1,
                                                                                        recommended.insecticide.dose = 1,
                                                                                        threshold.generations = 15,#no decay, so this value does not matter
                                                                                        base.efficacy.decay.rate = c(base.decay.rate.i[v]),
                                                                                        rapid.decay.rate = 0.08,
                                                                                        population.suppression = FALSE,
                                                                                        min.cross.selection = cross.resistance[v],
                                                                                        max.cross.selection = cross.resistance[v],
                                                                                        deployment.type = "singles",
                                                                                        mixture.strategy = "mix.sequential.discrete"),
                                           maximum.generations = 500, number.of.insecticides = 2)

  temp = subset(temp, site == "intervention")

  temp$dose = dose[v]
  temp$base.decay.i = base.decay.rate.i[v]
  temp$base.decay.j = base.decay.rate.j[v]
  temp$cross.resistance = cross.resistance[v]
  temp$sim.replicate = v

  monotherapy.list[[v]] = temp

  print(v)
}

monotherapy.df = do.call("rbind", monotherapy.list)

monotherapy.df.1 = monotherapy.df|>
  dplyr::group_by(sim.replicate)|>
  dplyr::summarise(max.gen = max(time.in.generations))


monotherapy.df.2 = rbind(monotherapy.df.1, monotherapy.df.1, monotherapy.df.1)

mixture.df.1$monotherapy.duration = monotherapy.df.2$max.gen
mixture.df.1$start.i= init.i
mixture.df.1$start.j = init.j
mixture.df.1$dispersal = dispersal
mixture.df.1$coverage = coverage

if(length(heritability) == 1){
 mixture.df.1$heritability = heritability
}

if(length(heritability) == 2){

  mixture.df.1$heritability.i = heritability[1]
  mixture.df.1$heritability.j = heritability[2]

}

mixture.df.1$fitness.cost = fitness.cost
mixture.df.1$exposure = exposure


return(mixture.df.1)

}

no_decay_comparisons = function(dispersal,
                                coverage,
                                fitness.cost,
                                exposure,
                                heritability,
                                init.i,
                                init.j){

  dose = rep(c(0.5, 0.75, 1), times = 3)
  cross.resistance = rep(c(-0.3, 0, 0.3), each = 3)

  mixture.list = list()

    for(v in 1:9){

      temp = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                            exposure.scaling.factor = 1,
                                                                                            female.fitness.cost = fitness.cost,
                                                                                            male.fitness.cost = fitness.cost,
                                                                                            female.insecticide.exposure = exposure,
                                                                                            male.insecticide.exposure = 1,
                                                                                            heritability = heritability,
                                                                                            dispersal.rate = dispersal,
                                                                                            intervention.coverage = coverage,
                                                                                            standard.deviation = 20,
                                                                                            vector.length = 1000,
                                                                                            maximum.bioassay.survival.proportion = 1,
                                                                                            michaelis.menten.slope = 1,
                                                                                            regression.coefficient = 0.48,
                                                                                            regression.intercept = 0.15,
                                                                                            maximum.generations = 500,
                                                                                            irm.strategy = "sequence",
                                                                                            half.population.bioassay.survival.resistance = 900,
                                                                                            withdrawal.threshold.value = 0.1,
                                                                                            return.threshold.value = 0.08,
                                                                                            deployment.frequency = 30, #minimum deployment frequency
                                                                                            maximum.resistance.value = 25000,
                                                                                            starting.refugia.resistance.score = c(init.i, init.j),
                                                                                            starting.intervention.resistance.score = c(init.i, init.j),
                                                                                            applied.insecticide.dose = dose[v],
                                                                                            recommended.insecticide.dose = 1,
                                                                                            threshold.generations = 20,#no decay, so this value does not matter
                                                                                            base.efficacy.decay.rate = 0,
                                                                                            rapid.decay.rate = 0,
                                                                                            population.suppression = FALSE,
                                                                                            min.cross.selection = cross.resistance[v],
                                                                                            max.cross.selection = cross.resistance[v],
                                                                                            deployment.type = "mixtures",
                                                                                            mixture.strategy = "mix.sequential.discrete"),
                                               maximum.generations = 500, number.of.insecticides = 2)

      temp = subset(temp, site == "intervention")

      temp$dose = dose[v]
      temp$base.decay.i = 0
      temp$base.decay.j = 0
      temp$cross.resistance = cross.resistance[v]
      temp$sim.replicate = v


      mixture.list[[v]] = temp

      print(v)
    }


    mixture.df = do.call("rbind", mixture.list)

    mixture.df.1 = mixture.df|>
      dplyr::group_by(sim.replicate)|>
      dplyr::summarise(max.gen = max(time.in.generations))


    mixture.df.1 = data.frame(mixture.df.1,
                              base.decay.rate.i = rep(0, 9),
                              base.decay.rate.j = rep(0, 9),
                              cross.resistance,
                              dose)

    #monotherapy comparisons


    cross.resistance = c(-0.3, 0, 0.3)

    monotherapy.list = list()

    for(v in 1:3){

      temp = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
                                                                                   exposure.scaling.factor = 1,
                                                                                   female.fitness.cost = fitness.cost,
                                                                                   male.fitness.cost = fitness.cost,
                                                                                   female.insecticide.exposure = exposure,
                                                                                   male.insecticide.exposure = 1,
                                                                                   heritability = heritability,
                                                                                   dispersal.rate = dispersal,
                                                                                   intervention.coverage = coverage,
                                                                                   standard.deviation = 20,
                                                                                   vector.length = 1000,
                                                                                   maximum.bioassay.survival.proportion = 1,
                                                                                   michaelis.menten.slope = 1,
                                                                                   regression.coefficient = 0.48,
                                                                                   regression.intercept = 0.15,
                                                                                   maximum.generations = 500,
                                                                                   irm.strategy = "sequence", #best performer
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   withdrawal.threshold.value = 0.1,
                                                                                   return.threshold.value = 0.08,
                                                                                   deployment.frequency = 30, #minimum deployment frequency
                                                                                   maximum.resistance.value = 25000,
                                                                                   starting.refugia.resistance.score = c(init.i, init.j),
                                                                                   starting.intervention.resistance.score = c(init.i, init.j),
                                                                                   applied.insecticide.dose = 1,
                                                                                   recommended.insecticide.dose = 1,
                                                                                   threshold.generations = 20,#no decay, so this value does not matter
                                                                                   base.efficacy.decay.rate = 0,
                                                                                   rapid.decay.rate = 0,
                                                                                   population.suppression = FALSE,
                                                                                   min.cross.selection = cross.resistance[v],
                                                                                   max.cross.selection = cross.resistance[v],
                                                                                   deployment.type = "singles",
                                                                                   mixture.strategy = "mix.sequential.discrete"),
                                      maximum.generations = 500, number.of.insecticides = 2)

      temp = subset(temp, site == "intervention")

      temp$dose = dose[v]
      temp$base.decay.i = 0
      temp$base.decay.j = 0
      temp$cross.resistance = cross.resistance[v]
      temp$sim.replicate = v

      monotherapy.list[[v]] = temp

      print(v)
    }

    monotherapy.df = do.call("rbind", monotherapy.list)

    monotherapy.df.1 = monotherapy.df|>
      dplyr::group_by(sim.replicate)|>
      dplyr::summarise(max.gen = max(time.in.generations))


    monotherapy.df.2 = rbind(monotherapy.df.1, monotherapy.df.1, monotherapy.df.1)

    mixture.df.1$monotherapy.duration = monotherapy.df.2$max.gen
    mixture.df.1$start.i= init.i
    mixture.df.1$start.j = init.j
    mixture.df.1$dispersal = dispersal
    mixture.df.1$coverage = coverage

    if(length(heritability) == 1){
      mixture.df.1$heritability = heritability
    }

    if(length(heritability) == 2){

      mixture.df.1$heritability.i = heritability[1]
      mixture.df.1$heritability.j = heritability[2]

    }

    mixture.df.1$fitness.cost = fitness.cost
    mixture.df.1$exposure = exposure


    return(mixture.df.1)

}


scenario.1 = impact_of_decay_simulations(dispersal = 0.2,
                            coverage = 0.7,
                            fitness.cost = 0,
                            exposure = 0.7,
                            heritability = 0.2,
                            init.i = 0,
                            init.j = 0)

scenario.1.no.decay = no_decay_comparisons(dispersal = 0.2,
                                           coverage = 0.7,
                                           fitness.cost = 0,
                                           exposure = 0.7,
                                           heritability = 0.2,
                                           init.i = 0,
                                           init.j = 0)

scenario.2 = impact_of_decay_simulations(dispersal = 0.2,
                                         coverage = 0.7,
                                         fitness.cost = 0,
                                         exposure = 0.7,
                                         heritability = 0.2,
                                         init.i = 25,
                                         init.j = 0)

scenario.2.no.decay = no_decay_comparisons(dispersal = 0.2,
                                           coverage = 0.7,
                                           fitness.cost = 0,
                                           exposure = 0.7,
                                           heritability = 0.2,
                                           init.i = 25,
                                           init.j = 0)

scenario.3 = impact_of_decay_simulations(dispersal = 0.2,
                                         coverage = 0.7,
                                         fitness.cost = 0,
                                         exposure = 0.7,
                                         heritability = c(0.15, 0.25),
                                         init.i = 0,
                                         init.j = 0)

scenario.3.no.decay = no_decay_comparisons(dispersal = 0.2,
                                         coverage = 0.7,
                                         fitness.cost = 0,
                                         exposure = 0.7,
                                         heritability = c(0.15, 0.25),
                                         init.i = 0,
                                         init.j = 0)



impact_decay_plot = function(decay.df,
                             no.decay.df,
                             graph.title){

max.value =  max(decay.df$max.gen-decay.df$monotherapy.duration,
                   no.decay.df$max.gen-no.decay.df$monotherapy.duration)/10

min.value = min(decay.df$max.gen-decay.df$monotherapy.duration,
                  no.decay.df$max.gen-no.decay.df$monotherapy.duration)/10

decay.plot = ggplot(decay.df,
                      aes(colour= as.factor(dose),
                      y=(max.gen - monotherapy.duration)/10,
                      x = cross.resistance))+
  geom_point(size = 4)+
  geom_line()+
    scale_colour_manual(values = c("darkred", #0.5
                                   "orange", #0.75
                                   "darkblue" #1
  ))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Base decay rate insecticide i",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = seq(min.value,
                                  max.value,
                                  3),
                     limits = c(min.value, max.value))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Base decay rate insecticide j",
                                         breaks = NULL,
                                         labels = NULL),
                     breaks = c(-0.3, 0, 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black")+
  facet_grid(base.decay.rate.i ~ base.decay.rate.j)+
  xlab("Cross Resistance")+
  ylab("Difference in Strategy Lifespan (years)")+
  ggtitle(graph.title)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size = 18))

no.decay.plot = ggplot(no.decay.df,
                       aes(colour= as.factor(dose),
                           y=(max.gen - monotherapy.duration)/10,
                           x = cross.resistance))+
  geom_point(size = 4)+
  geom_line()+
  scale_colour_manual(values = c("darkred", #0.5
                                 "orange", #0.75
                                 "darkblue" #1
  ))+
  scale_y_continuous(breaks = seq(min.value,
                                  max.value,
                                  3),
                     limits = c(min.value, max.value))+
  scale_x_continuous(breaks = c(-0.3, 0, 0.3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black")+
  facet_grid(base.decay.rate.i ~ base.decay.rate.j)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank())

the.legend = cowplot::get_legend(ggplot(no.decay.df,
                               aes(colour= as.factor(dose),
                                   y=(max.gen - monotherapy.duration)/10,
                                   x = cross.resistance))+
                          geom_point(size = 4)+
                          geom_line()+
                          scale_colour_manual(values = c("darkred", #0.5
                                                         "orange", #0.75
                                                         "darkblue" #1
                          ))+
                          scale_y_continuous(breaks = seq(min.value,
                                                          max.value,
                                                          3),
                                             limits = c(min.value, max.value))+
                          scale_x_continuous(breaks = c(-0.3, 0, 0.3))+
                          geom_hline(yintercept = 0, linetype = "dashed", colour = "black")+
                          facet_grid(base.decay.rate.i ~ base.decay.rate.j)+
                          theme_bw()+
                          guides(colour=guide_legend(title=paste0("Initial Mixture\nPartner Efficacy")))+
                          theme(legend.position = "right",
                                axis.title = element_blank()))



the.layout = "
AAA#
AAAB
AAAC
"

end.plot = decay.plot + no.decay.plot + the.legend + plot_layout(design = the.layout)


return(end.plot)
}

impact_decay_plot(scenario.1,
                  scenario.1.no.decay,
                  graph.title = "Scenario 1: The Impact of Insecticide Decay")

impact_decay_plot(scenario.2,
                  scenario.2.no.decay,
                  graph.title = "Scenario 2: Mismatched Initial Resistance")

impact_decay_plot(scenario.3,
                  scenario.3.no.decay,
                  graph.title = "Scenario 3: Mismatched Heritability")

