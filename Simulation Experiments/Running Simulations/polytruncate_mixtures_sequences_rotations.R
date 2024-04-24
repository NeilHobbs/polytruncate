# parameter.space.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")
#
# parameter.space.df = rbind(parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
#                            parameter.space.df, parameter.space.df, parameter.space.df)
#
# parameter.space.df$cross.selection = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000),
#                                       rep(0, 5000), rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000))
#
#
# sequence.duration = c()
# sequence.peak = c()
# rotation.duration = c()
# rotation.peak = c()
# mixture.duration = c()
# mixture.peak = c()
#
#
# for(v in 1:nrow(parameter.space.df)){
#
#   SEQ = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
#                                                                             exposure.scaling.factor = 1,
#                                                                             female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
#                                                                             male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
#                                                                             female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                             male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                             heritability = parameter.space.df$Heritability[v],
#                                                                             dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                             intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                             standard.deviation = 20,
#                                                                             vector.length = 250,
#                                                                             maximum.bioassay.survival.proportion = 1,
#                                                                             michaelis.menten.slope = 1,
#                                                                             regression.coefficient = 0.48,
#                                                                             regression.intercept = 0.15,
#                                                                             maximum.generations = 500,
#                                                                             irm.strategy = "sequence",
#                                                                             half.population.bioassay.survival.resistance = 900,
#                                                                             withdrawal.threshold.value = 0.1,
#                                                                             return.threshold.value = 0.08,
#                                                                             deployment.frequency = 10, #minimum deployment frequency
#                                                                             maximum.resistance.value = 25000,
#                                                                             starting.refugia.resistance.score = 0,
#                                                                             starting.intervention.resistance.score = 0,
#                                                                             applied.insecticide.dose = 1,
#                                                                             recommended.insecticide.dose = 1,
#                                                                             threshold.generations = 5,#no decay, so this value does not matter
#                                                                             base.efficacy.decay.rate = 0,
#                                                                             rapid.decay.rate = 0,
#                                                                             population.suppression = FALSE,
#                                                                             min.cross.selection = parameter.space.df$cross.selection[v],
#                                                                             max.cross.selection = parameter.space.df$cross.selection[v],
#                                                                             deployment.type = "singles",
#                                                                             mixture.strategy = NA),
#                                maximum.generations = 500, number.of.insecticides = 2)
#
#
#   ROT = get_simulation_dataframe(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
#                                                                             exposure.scaling.factor = 1,
#                                                                             female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
#                                                                             male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
#                                                                             female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                             male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                             heritability = parameter.space.df$Heritability[v],
#                                                                             dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                             intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                             standard.deviation = 20,
#                                                                             vector.length = 250,
#                                                                             maximum.bioassay.survival.proportion = 1,
#                                                                             michaelis.menten.slope = 1,
#                                                                             regression.coefficient = 0.48,
#                                                                             regression.intercept = 0.15,
#                                                                             maximum.generations = 500,
#                                                                             irm.strategy = "rotation",
#                                                                             half.population.bioassay.survival.resistance = 900,
#                                                                             withdrawal.threshold.value = 0.1,
#                                                                             return.threshold.value = 0.08,
#                                                                             deployment.frequency = 10, #minimum deployment frequency
#                                                                             maximum.resistance.value = 25000,
#                                                                             starting.refugia.resistance.score = 0,
#                                                                             starting.intervention.resistance.score = 0,
#                                                                             applied.insecticide.dose = 1,
#                                                                             recommended.insecticide.dose = 1,
#                                                                             threshold.generations = 5,#no decay, so this value does not matter
#                                                                             base.efficacy.decay.rate = 0,
#                                                                             rapid.decay.rate = 0,
#                                                                             population.suppression = FALSE,
#                                                                             min.cross.selection = parameter.space.df$cross.selection[v],
#                                                                             max.cross.selection = parameter.space.df$cross.selection[v],
#                                                                             deployment.type = "singles",
#                                                                             mixture.strategy = NA),
#                                maximum.generations = 500, number.of.insecticides = 2)
#
#
#   MIX = get_simulation_dataframe_mixtures(simulation.array = run_simulation_truncation(number.of.insecticides = 2,
#                                                                                      exposure.scaling.factor = 1,
#                                                                                      female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
#                                                                                      male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
#                                                                                      female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                      male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                      heritability = parameter.space.df$Heritability[v],
#                                                                                      dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                      intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                      standard.deviation = 20,
#                                                                                      vector.length = 250,
#                                                                                      maximum.bioassay.survival.proportion = 1,
#                                                                                      michaelis.menten.slope = 1,
#                                                                                      regression.coefficient = 0.48,
#                                                                                      regression.intercept = 0.15,
#                                                                                      maximum.generations = 500,
#                                                                                      irm.strategy = "sequence",
#                                                                                      half.population.bioassay.survival.resistance = 900,
#                                                                                      withdrawal.threshold.value = 0.1,
#                                                                                      return.threshold.value = 0.08,
#                                                                                      deployment.frequency = 10, #minimum deployment frequency
#                                                                                      maximum.resistance.value = 25000,
#                                                                                      starting.refugia.resistance.score = 0,
#                                                                                      starting.intervention.resistance.score = 0,
#                                                                                      applied.insecticide.dose = 1,
#                                                                                      recommended.insecticide.dose = 1,
#                                                                                      threshold.generations = 5,#no decay, so this value does not matter
#                                                                                      base.efficacy.decay.rate = 0,
#                                                                                      rapid.decay.rate = 0,
#                                                                                      population.suppression = FALSE,
#                                                                                      min.cross.selection = parameter.space.df$cross.selection[v],
#                                                                                      max.cross.selection = parameter.space.df$cross.selection[v],
#                                                                                      deployment.type = "mixtures",
#                                                                                      mixture.strategy = "mix.sequential.discrete"),
#                                         maximum.generations = 500, number.of.insecticides = 2)
#
#
#
#
#   sequence.duration[v] = max(SEQ$time.in.generations)
#   sequence.peak[v] = max(SEQ$resistance.score)
#   rotation.duration[v] = max(ROT$time.in.generations)
#   rotation.peak[v] = max(ROT$resistance.score)
#   mixture.duration[v] = max(MIX$time.in.generations)
#   mixture.peak[v] = max(MIX$resistance.intensity)
#
# print(v)
# }
#
# final.df = data.frame(parameter.space.df, sequence.duration, sequence.peak,
#                       rotation.duration, rotation.peak, mixture.duration,
#                       mixture.peak)
#
#
# write.csv(final.df, ".//polytruncate.sequence.rotation.mixture.csv")

final.df = read.csv("polytruncate.sequence.rotation.mixture.csv")

#remove simulations that never took off
df.1 = final.df|>
  dplyr::filter(sequence.peak > 1)|>
  dplyr::filter(mixture.peak > 1)|>
  dplyr::filter(rotation.peak > 1)

df.1$seqrot = ifelse(df.1$sequence.duration > df.1$rotation.duration,
                     yes = "Sequence",
                     no = ifelse(df.1$sequence.duration < df.1$rotation.duration,
                                 yes = "Rotation",
                                 no = "Draw"))

df.1$mixrot = ifelse(df.1$mixture.duration > df.1$rotation.duration,
                     yes = "Mixture",
                     no = ifelse(df.1$mixture.duration < df.1$rotation.duration,
                                 yes = "Rotation",
                                 no = "Draw"))

df.1$mixseq = ifelse(df.1$mixture.duration > df.1$sequence.duration,
                     yes = "Mixture",
                     no = ifelse(df.1$mixture.duration < df.1$sequence.duration,
                                 yes = "Sequence",
                                 no = "Draw"))

library(ggplot2)
library(patchwork)
#Sequences vs Rotations

seq.rot.plot = ggplot(subset(df.1, seqrot != "Draw"), aes(x=sequence.duration - rotation.duration,
                                                          fill = seqrot))+
  geom_histogram(binwidth = 20)+
  scale_fill_manual(values = c("red", "blue", "grey"))+
  ggtitle("Sequences vs Rotations")+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration (Generations)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =10,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

#Mixtures vs Rotations
rot.mix.plot = ggplot(subset(df.1, mixrot != "Draw"), aes(x=mixture.duration - rotation.duration,
                                                          fill = mixrot))+
  scale_fill_manual(values = c("purple", "red", "grey"))+
  geom_histogram(binwidth = 20)+

  ggtitle("Mixtures vs Rotations")+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration (Generations)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =10,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

#Mixtures vs Sequences
seq.mix.plot = ggplot(subset(df.1, mixseq != "Draw"), aes(x=mixture.duration - sequence.duration,
                                                          fill = mixseq))+
  geom_histogram(binwidth = 20)+
  scale_fill_manual(values = c("purple", "blue", "grey"))+
  ggtitle("Mixtures vs Sequences")+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration (Generations)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =10,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

seq.rot.plot + rot.mix.plot + seq.mix.plot + plot_annotation(title = "Truncation Selection - polytruncate",
                                                             theme = theme(plot.title = element_text(size = 16)))


ggsave(
  filename = "chapter3_figure9.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 600,
  height = 400,
  units = "px",
  dpi = 200)

