library(devtools)
load_all()
fd.mix = get_simulation_dataframe_mixtures(run_simulation_truncation(number.of.insecticides = 2,
                                                            exposure.scaling.factor = 1,
                                                            female.fitness.cost = 0,
                                                            male.fitness.cost = 0,
                                                            female.insecticide.exposure = 0.7,
                                                            male.insecticide.exposure = 0.7,
                                                            heritability = 0.3,
                                                            dispersal.rate = 0.2,
                                                            intervention.coverage = 0.8,
                                                            standard.deviation = 20,
                                                            z.sd.coefficient = 0.4,
                                                            z.sd.intercept = 18,
                                                            sd.scaled = FALSE, #FALSE or FALSE. False is the default
                                                            vector.length= 250,
                                                            maximum.bioassay.survival.proportion = 1,
                                                            michaelis.menten.slope = 1,
                                                            regression.coefficient = 0.48,
                                                            regression.intercept = 0.15,
                                                            maximum.generations = 500,
                                                            irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                            half.population.bioassay.survival.resistance = 900,
                                                            withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                            return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                            deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                            maximum.resistance.value = 900000,
                                                            starting.refugia.resistance.score = 0,
                                                            starting.intervention.resistance.score = 0,
                                                            applied.insecticide.dose = 1,
                                                            recommended.insecticide.dose= 1,
                                                            threshold.generations = 10,
                                                            base.efficacy.decay.rate = 0,
                                                            rapid.decay.rate = 0,
                                                            population.suppression = FALSE,
                                                            min.cross.selection = -0.3,
                                                            max.cross.selection = -0.3,
                                                            deployment.type = "mixtures",
                                                            mixture.strategy = "mix.sequential.discrete"),
                                  number.of.insecticides = 2,
                                  maximum.generations = 200)

hd.mix = get_simulation_dataframe_mixtures(run_simulation_truncation(number.of.insecticides = 2,
                           exposure.scaling.factor = 1,
                           female.fitness.cost = 0,
                           male.fitness.cost = 0,
                           female.insecticide.exposure = 0.7,
                           male.insecticide.exposure = 0.7,
                           heritability = 0.3,
                           dispersal.rate = 0.2,
                           intervention.coverage = 0.8,
                           standard.deviation = 20,
                           z.sd.coefficient = 0.4,
                           z.sd.intercept = 18,
                           sd.scaled = FALSE, #FALSE or FALSE. False is the default
                           vector.length= 250,
                           maximum.bioassay.survival.proportion = 1,
                           michaelis.menten.slope = 1,
                           regression.coefficient = 0.48,
                           regression.intercept = 0.15,
                           maximum.generations = 500,
                           irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                           half.population.bioassay.survival.resistance = 900,
                           withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                           return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                           deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                           maximum.resistance.value = 900000,
                           starting.refugia.resistance.score = 0,
                           starting.intervention.resistance.score = 0,
                           applied.insecticide.dose = 0.5,
                           recommended.insecticide.dose= 1,
                           threshold.generations = 10,
                           base.efficacy.decay.rate = 0,
                           rapid.decay.rate = 0,
                           population.suppression = FALSE,
                           min.cross.selection = -0.3,
                           max.cross.selection = -0.3,
                           deployment.type = "mixtures",
                           mixture.strategy = "mix.sequential.discrete"),
  number.of.insecticides = 2,
  maximum.generations = 200)

rotation = get_simulation_dataframe(run_simulation_truncation(number.of.insecticides = 2,
                                                                       exposure.scaling.factor = 1,
                                                                       female.fitness.cost = 0,
                                                                       male.fitness.cost = 0,
                                                                       female.insecticide.exposure = 0.7,
                                                                       male.insecticide.exposure = 0.7,
                                                                       heritability = 0.3,
                                                                       dispersal.rate = 0.2,
                                                                       intervention.coverage = 0.8,
                                                                       standard.deviation = 20,
                                                                       z.sd.coefficient = 0.4,
                                                                       z.sd.intercept = 18,
                                                                       sd.scaled = FALSE, #FALSE or FALSE. False is the default
                                                                       vector.length= 250,
                                                                       maximum.bioassay.survival.proportion = 1,
                                                                       michaelis.menten.slope = 1,
                                                                       regression.coefficient = 0.48,
                                                                       regression.intercept = 0.15,
                                                                       maximum.generations = 500,
                                                                       irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                                       half.population.bioassay.survival.resistance = 900,
                                                                       withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                       deployment.frequency = 2, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                       maximum.resistance.value = 900000,
                                                                       starting.refugia.resistance.score = 0,
                                                                       starting.intervention.resistance.score = 0,
                                                                       applied.insecticide.dose = 0.5,
                                                                       recommended.insecticide.dose= 1,
                                                                       threshold.generations = 10,
                                                                       base.efficacy.decay.rate = 0,
                                                                       rapid.decay.rate = 0,
                                                                       population.suppression = FALSE,
                                                                       min.cross.selection = -0.3,
                                                                       max.cross.selection = -0.3,
                                                                       deployment.type = "singles",
                                                                       mixture.strategy = "mix.sequential.discrete"),
                                             number.of.insecticides = 2,
                                             maximum.generations = 200)


fd.mix = subset(fd.mix, site == "intervention")
fd.mix$fd.mix.bioassay = convert_resistance_score_to_bioassay_survival(trait.mean = fd.mix$resistance.intensity)

fd.mix.1 = subset(fd.mix, insecticide.tracked == 1)
fd.mix.2 = subset(fd.mix, insecticide.tracked == 2)

total.fd = fd.mix.1$fd.mix.bioassay + fd.mix.2$fd.mix.bioassay

hd.mix = subset(hd.mix, site == "intervention")
hd.mix$hd.mix.bioassay = convert_resistance_score_to_bioassay_survival(trait.mean = hd.mix$resistance.intensity)
hd.mix.1 = subset(hd.mix, insecticide.tracked == 1)
hd.mix.2 = subset(hd.mix, insecticide.tracked == 2)
total.hd = hd.mix.1$hd.mix.bioassay + hd.mix.2$hd.mix.bioassay


rotation = subset(rotation, site == "intervention")
rotation$rotation.bioassay = convert_resistance_score_to_bioassay_survival(trait.mean = rotation$resistance.score)
rotation.1 = subset(rotation, insecticide.tracked == 1)
rotation.2 = subset(rotation, insecticide.tracked == 2)
rotation.total = rotation.1$rotation.bioassay +rotation.2$rotation.bioassay

df = data.frame(total.fd, total.hd, rotation.total,
                time = seq(1, 200, 1))


A = ggplot(df, aes(y=total.hd*100,
               x=time))+
  geom_line(colour = "blue", size = 3)+
  geom_line(aes(x=time,
                 y=total.fd*100,
             colour = "red", size = 3))+
  geom_line(aes(x=time,
                 y=rotation.total*100),
             colour = "green", size =3)+
  xlab("Time in Generations")+
  ylab("Total Bioassay Survival (%)")+
  ylim(0, 200)+
  theme_classic()+
  theme(legend.position = "none")


B = ggplot(rotation, aes(x=time.in.generations,
                     y=rotation.bioassay*100,
                     colour = as.character(insecticide.tracked)))+
  geom_line(size = 2)+
  scale_color_manual(values = c("darkgreen", "green"))+
  geom_line(data = hd.mix.1, aes(x=time.in.generations,
                                 y=hd.mix.bioassay*100),
            colour = "darkblue", size = 4)+
  geom_line(data = hd.mix.2, aes(x=time.in.generations,
                                 y=hd.mix.bioassay*100),
            colour = "skyblue", size = 2)+
  geom_line(data = fd.mix.1, aes(x=time.in.generations,
                                 y=fd.mix.bioassay*100),
            colour = "darkred", size = 4)+
  geom_line(data = fd.mix.2, aes(x=time.in.generations,
                                 y=fd.mix.bioassay*100),
            colour = "coral", size = 2)+
  ylim(0, 200)+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  theme_classic()+
  theme(legend.position = "none")

library(patchwork)
A+B




