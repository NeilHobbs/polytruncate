####Insecticide Decay Profile Graphs:::
library(devtools)
load_all()
library(patchwork)


decay.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = 1,
                                   recommended.insecticide.dose = 1,
                                   threshold.generations = 15,
                                   base.efficacy.decay.rate = 0.015,
                                   rapid.decay.rate = 0.08,
                                   deployment.frequency = 30)

decay.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = 1,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.025,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

decay.3 = create_insecticide_efficacy_vector(applied.insecticide.dose = 1,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.005,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

generations = seq(0, 29, 1)


decay.df = data.frame(decay.1, decay.2, decay.3, generations)

decay.plot.1 = ggplot(decay.df, aes(x=generations, y = decay.1))+
  geom_vline(xintercept = 15, linetype = "dashed",
               colour = "black", linewidth = 1.4)+
  geom_line(colour = "#1b9e77")+
  geom_point(colour = "#1b9e77",
             size = 2)+
  geom_line(aes(y = decay.2,
                colour = "#d95f02"))+
  geom_point(aes(y = decay.2),
             colour = "#d95f02",
             size = 2)+
  geom_line(aes(y = decay.3),
            colour = "#7570b3")+
  geom_point(aes(y = decay.3),
             colour = "#7570b3",
             size = 2)+
  geom_label(aes(label = paste0("Slow\nDecay"),
                 x = 10, y = 0.5),
             fill = "#3690c0",
             colour = "black",
             size = 4)+
  geom_label(aes(label = paste0("Rapid\nDecay"),
                 x = 25.7, y = 0.8),
             fill = "#99000d",
             colour = "black",
             size = 4)+
  geom_text(aes(label = paste0("Threshold Decay\nGeneration"),
                x = 15, y= 0.27,
                angle = 270))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = c(0, 5, 10, 15, 20, 25))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01),
                     breaks = seq(0, 1, 0.05))+
  ylab(paste0("Efficacy against susceptible mosquitoes"))+
  xlab(paste0("Time since insecticide deployment\nin mosquito generations"))+
  ggtitle("A")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 15))


##Also add in for mixtures with reduced dose:::
##75%
decay.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = 0.75,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.015,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

decay.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = 0.75,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.025,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

decay.3 = create_insecticide_efficacy_vector(applied.insecticide.dose = 0.75,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.005,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

generations = seq(0, 29, 1)


decay.df = data.frame(decay.1, decay.2, decay.3, generations)

decay.plot.2 = ggplot(decay.df, aes(x=generations, y = decay.1))+
  geom_vline(xintercept = 15, linetype = "dashed",
             colour = "black", linewidth = 1.4)+
  geom_line(colour = "#1b9e77")+
  geom_point(colour = "#1b9e77",
             size = 2)+
  geom_line(aes(y = decay.2,
                colour = "#d95f02"))+
  geom_point(aes(y = decay.2),
             colour = "#d95f02",
             size = 2)+
  geom_line(aes(y = decay.3),
            colour = "#7570b3")+
  geom_point(aes(y = decay.3),
             colour = "#7570b3",
             size = 2)+
  geom_label(aes(label = paste0("Slow\nDecay"),
                 x = 10, y = 0.3),
             fill = "#3690c0",
             colour = "black",
             size = 4)+
  geom_label(aes(label = paste0("Rapid\nDecay"),
                 x = 25.7, y = 0.6),
             fill = "#99000d",
             colour = "black",
             size = 4)+
  geom_text(aes(label = paste0("Threshold Decay\nGeneration"),
                x = 15, y= 0.2,
                angle = 270))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = c(0, 5, 10, 15, 20, 25))+
  scale_y_continuous(expand = c(0, 0),  limits = c(0, 1.01),
                     breaks = seq(0, 1, 0.05))+
  ylab(paste0("Efficacy against susceptible mosquitoes"))+
  xlab(paste0("Time since insecticide deployment\nin mosquito generations"))+
  ggtitle("B")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 15))

#50%
decay.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = 0.5,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.015,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

decay.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = 0.5,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.025,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

decay.3 = create_insecticide_efficacy_vector(applied.insecticide.dose = 0.5,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 15,
                                             base.efficacy.decay.rate = 0.005,
                                             rapid.decay.rate = 0.08,
                                             deployment.frequency = 30)

generations = seq(0, 29, 1)


decay.df = data.frame(decay.1, decay.2, decay.3, generations)

decay.plot.3 = ggplot(decay.df, aes(x=generations, y = decay.1))+
  geom_vline(xintercept = 15, linetype = "dashed",
             colour = "black", linewidth = 1.4)+
  geom_line(colour = "#1b9e77")+
  geom_point(colour = "#1b9e77",
             size = 2)+
  geom_line(aes(y = decay.2,
                colour = "#d95f02"))+
  geom_point(aes(y = decay.2),
             colour = "#d95f02",
             size = 2)+
  geom_line(aes(y = decay.3),
            colour = "#7570b3")+
  geom_point(aes(y = decay.3),
             colour = "#7570b3",
             size = 2)+
  geom_label(aes(label = paste0("Slow\nDecay"),
                 x = 10, y = 0.7),
             fill = "#3690c0",
             colour = "black",
             size = 4)+
  geom_label(aes(label = paste0("Rapid\nDecay"),
                 x = 25.7, y = 0.7),
             fill = "#99000d",
             colour = "black",
             size = 4)+
  geom_text(aes(label = paste0("Threshold Decay\nGeneration"),
                x = 15, y= 0.75,
                angle = 270))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = c(0, 5, 10, 15, 20, 25))+
  scale_y_continuous(expand = c(0, 0),  limits = c(0, 1.01),
                     breaks = seq(0, 1, 0.05))+
  ylab(paste0("Efficacy against susceptible mosquitoes"))+
  xlab(paste0("Time since insecticide deployment\nin mosquito generations"))+
  ggtitle("C")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 15))


decay.plot.1 + decay.plot.2 + decay.plot.3




















