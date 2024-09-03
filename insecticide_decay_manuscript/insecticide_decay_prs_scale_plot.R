library(devtools)
load_all()

PRS = seq(0, 3650, 1)

bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                              trait.mean = PRS,
                                              half.population.bioassay.survival.resistance = 900,
                                              michaelis.menten.slope = 1)

field.survival1.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survival,
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy = 1.2)


field.survival1.0 = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survival,
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                current.insecticide.efficacy = 1)


field.survival0.8 = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survival,
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                current.insecticide.efficacy = 0.8)



field.survival0.5 = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survival,
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                current.insecticide.efficacy = 0.5)



field.survival0.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survival,
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                current.insecticide.efficacy = 0.1)


the.df = data.frame(PRS, bioassay.survival,
                    field.survival1.2,
                    field.survival1.0,
                    field.survival0.8,
                    field.survival0.5,
                    field.survival0.1)

p1.a = ggplot(the.df, aes(x=PRS, y = bioassay.survival))+

  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 3650),
                     breaks = c(0, 47, 100, 225, 900, 3600))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1),
                     breaks = c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1),
                     labels = paste0(c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1)*100))+
  geom_segment(aes(x = 0, y = 0.05, xend = 47, yend = 0.05), colour = "grey", size = 2)+
  geom_segment(aes(x = 47, y = 0, xend = 47, yend = 0.05), colour = "grey", size = 2)+
  geom_segment(aes(x = 0, y = 0.1, xend = 100, yend = 0.1), colour = "grey", size = 2)+
  geom_segment(aes(x = 100, y = 0, xend = 100, yend = 0.1), colour = "grey", size = 2)+
  geom_segment(aes(x = 0, y = 0.2, xend = 225, yend = 0.2), colour = "grey", size = 2)+
  geom_segment(aes(x = 225, y = 0, xend = 225, yend = 0.2), colour = "grey", size = 2)+
  geom_segment(aes(x = 0, y = 0.5, xend = 900, yend = 0.5), colour = "grey", size = 2)+
  geom_segment(aes(x = 900, y = 0, xend = 900, yend = 0.5), colour = "grey", size = 2)+
  geom_segment(aes(x = 0, y = 0.8, xend = 3600, yend = 0.8), colour = "grey", size = 2)+
  geom_segment(aes(x = 3600, y = 0, xend = 3600, yend = 0.8), colour = "grey", size = 2)+
  geom_line(linewidth = 3)+
  geom_line(aes(x=PRS, y = field.survival1.2), linewidth = 2, colour = "#225ea8", linetype = "dashed")+
  geom_line(aes(x=PRS, y = field.survival1.0), linewidth = 2, colour = "#1d91c0", linetype = "dashed")+
  geom_line(aes(x=PRS, y = field.survival0.8), linewidth = 2, colour = "#41b6c4", linetype = "dashed")+
  geom_line(aes(x=PRS, y = field.survival0.5), linewidth = 2, colour = "#7fcdbb", linetype = "dashed")+
  geom_line(aes(x=PRS, y = field.survival0.1), linewidth = 2, colour = "#c7e9b4", linetype = "dashed")+

  geom_text(aes(x=2000, y = 0.14, label = "Insecticide Efficacy = 1.2"), linewidth = 2, colour = "#225ea8", linetype = "dashed", size = 8)+
  geom_text(aes(x=2000, y = 0.17, label = "Insecticide Efficacy = 1.0"), linewidth = 2, colour = "#1d91c0", linetype = "dashed", size = 8)+
  geom_text(aes(x=2000, y = 0.20, label = "Insecticide Efficacy = 0.8"), linewidth = 2, colour = "#41b6c4", linetype = "dashed", size = 8)+
  geom_text(aes(x=2000, y = 0.23, label = "Insecticide Efficacy = 0.5"), linewidth = 2, colour = "#7fcdbb", linetype = "dashed", size = 8)+
  geom_text(aes(x=2000, y = 0.26, label = "Insecticide Efficacy = 0.1"), linewidth = 2, colour = "#c7e9b4", linetype = "dashed", size = 8)+


  xlab("Polygenic Resistance Score")+
  ylab("Survival (%)")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, colour = "black", angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"))


p1.b = ggplot(the.df, aes(x=bioassay.survival * 100,
                   y = field.survival0.1))+
  geom_line(aes(y = field.survival1.2* 100), linewidth = 2, colour = "#225ea8")+
  geom_line(aes(y = field.survival1.0* 100), linewidth = 2, colour = "#1d91c0")+
  geom_line(aes(y = field.survival0.8* 100), linewidth = 2, colour = "#41b6c4")+
  geom_line(aes(y = field.survival0.5* 100), linewidth = 2, colour = "#7fcdbb")+
  geom_line(aes(y = field.survival0.1* 100), linewidth = 2, colour = "#c7e9b4")+
  xlab(paste("Measured Bioassay Survival (%)\nof the Population"))+
  ylab("Expected Survival\nin the Field (%)")+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 80),
                     breaks = seq(0, 100, 20))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 100),
                     breaks = c(0, 0.1, 0.2, 0.5, 0.8, 1)*100)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"))



p1.a + p1.b






ggsave(plot = last_plot(),
       filename = "insecticide_decay_prs_scale.jpeg",
       dpi = 300,
       height = 600,
       width = 1200,
       scale = 5,
       units = "px")








