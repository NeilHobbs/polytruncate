#Set 25 Truncation Selection no fitness costs or dispersal
#Identify what would be the suitable standard deviation and exposure scaling factor for calibration.


##Model Calibration for Truncation Selection

#Still need to have it such that the "average" insecticide lasts 10 years with continual use.
#Issue is now that the response at each generation time point is dependent on both the current
#mean value of the polygenic resistance score and the standard deviation of the Normal distribution.
library(devtools)
load_all()

parameter.space = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.3.csv")
standard.deviation = rep(c(rep(20, 5000), rep(30, 5000), rep(40, 5000), rep(50, 5000), rep(60, 5000), rep(70, 5000), rep(80, 5000)), 2)

parameter.space.df = rbind(parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space, parameter.space)

exposure.scaling.factor = c(rep(1, 35000), rep(10, 35000))

parameter.space.df$standard.deviation = standard.deviation
parameter.space.df$exposure.scaling.factor = exposure.scaling.factor


sequence.list = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 1,
                                                                        exposure.scaling.factor = parameter.space.df$exposure.scaling.factor[v],
                                                                        female.fitness.cost = 0,
                                                                        male.fitness.cost = 0,
                                                                        female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                        male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                        heritability = parameter.space.df$Heritability[v],
                                                                        dispersal.rate = 0,
                                                                        intervention.coverage = 1,
                                                                        standard.deviation = parameter.space.df$standard.deviation[v],
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
                                                                        deployment.frequency = 2, #minimum deployment frequency
                                                                        maximum.resistance.value = 25000,
                                                                        starting.refugia.resistance.score = 0,
                                                                        starting.intervention.resistance.score = 0,
                                                                        applied.insecticide.dose = 1,
                                                                        recommended.insecticide.dose = 1,
                                                                        threshold.generations = 5,#no decay, so this value does not matter
                                                                        base.efficacy.decay.rate = 0,
                                                                        rapid.decay.rate = 0,
                                                                        population.suppression = FALSE,
                                                                        min.cross.selection = 0,
                                                                        max.cross.selection = 0),
                               maximum.generations = 500, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)



  sequence.list[[v]] = simulation.duration

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, sequence.list)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//sequence.set.25.csv")



ggplot(df, aes(x=sim.duration))+
  geom_histogram()+
  facet_wrap(exposure.scaling.factor~standard.deviation)

df.1 = df%>%
  dplyr::filter(exposure.scaling.factor == 1)

df.3 = df%>%
  dplyr::filter(simulation.duration < 500)

ggplot(df.3, aes(x=sim.duration/10))+
  geom_histogram(binwidth = 1,
                 fill = "#99d8c9")+
  geom_vline(xintercept = 8,
             colour = "red",
             alpha = 0.7,
             linetype = "dashed")+
  geom_vline(xintercept = 12,
             colour = "red",
             alpha = 0.7,
             linetype = "dashed")+
  geom_rect(data = subset(df.3, exposure.scaling.factor %in% 1 & standard.deviation %in% 20),
            fill = NA, colour = "#c994c7", xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,
            size = 5) +
  xlab("Operational Lifespan (years)")+
  ylab("Count")+
  facet_grid(exposure.scaling.factor~standard.deviation)+
  theme_classic()



df.2 = df.1%>%
  dplyr::filter(standard.deviation == 20|
                  standard.deviation == 30|
                  standard.deviation == 40)%>%
  dplyr::filter(sim.duration < 500)

ggplot(df.2, aes(x=sim.duration))+
  geom_histogram(binwidth = 10)+
  geom_vline(xintercept = 80,
             colour = "red")+
  geom_vline(xintercept = 120,
             colour = "red")+
  facet_wrap(~standard.deviation)






