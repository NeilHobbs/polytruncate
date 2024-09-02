library(devtools)
load_all()
library(patchwork)


trait.values = create_normal_distribution(vector.length = 1000,
                                          trait.mean = 0,
                                          standard.deviation = 20)

values.frequency = calculate_density_of_trait_values(vector.length = 1000,
                                                     trait.mean = 0,
                                                     standard.deviation = 20)


unexposed.individuals = 0.3 * values.frequency
exposed.individuals = 0.7 * values.frequency


exposed.survivors.high.efficacy = c(rep(0, 850), values.frequency[851:1000]* 0.7)

end.survivors.high.efficacy = c(rep(0, 850), values.frequency[851:1000]* 0.7) + unexposed.individuals


df = data.frame(exposed.survivors.high.efficacy,
                end.survivors.high.efficacy,
                unexposed.individuals,
                exposed.individuals,
                trait.values)


# Colour Scheme:
# Initial emerge:: green  = #addd8e
# Avoids Selection:: Blue = #6baed6
# Killed by i: red = #fb6a4a
# Survives i : purple = #807dba

before.selection = ggplot(df, aes(x=trait.values,
                                  y=values.frequency))+
  geom_area(fill = "#addd8e")+
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 1,
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Initial Population")+
  xlab("Polygenic Resistance Score")+
  theme_classic()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text=element_blank(),
        axis.ticks=element_blank())

unexposed.plot = ggplot(df, aes(x=trait.values,
                                y=unexposed.individuals))+
  geom_area(fill = "#6baed6")+
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 1,
             colour = "black")+
  ylim(0, 0.02)+
  ggtitle("Avoids Insecticides")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())


high.efficacy.plot = ggplot(df, aes(x=trait.values,
                                    y=exposed.survivors.high.efficacy))+
  geom_area(aes(x=trait.values, y=exposed.individuals),
            fill = "#fb6a4a")+
  geom_area(fill = "#807dba")+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed", linewidth = 1)+
  geom_vline(xintercept = sum(trait.values * exposed.survivors.high.efficacy)/sum(exposed.survivors.high.efficacy),
             colour = "purple",
             linetype = "dashed", linewidth = 1)+
  geom_vline(xintercept =trait.values[850],
             colour = "#feb24c",
             linetype = "dashed", linewidth = 1)+
  ggtitle("Exposed to Insecticide")+
  ylim(0, 0.02)+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())


high.efficacy.end.plot = ggplot(df, aes(x=trait.values,
                                        y=end.survivors.high.efficacy))+
  geom_area(fill = "#807dba")+
  geom_area(aes(x=trait.values, y = unexposed.individuals),
            fill = "#6baed6")+
  ylim(0, 0.02)+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed", linewidth = 1)+
  geom_vline(xintercept = (sum(trait.values * end.survivors.high.efficacy)/sum(end.survivors.high.efficacy)),
             colour = "#ce1256",
             linetype = "dashed", linewidth = 1)+
  ggtitle("Parental Population")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())



legend.df = data.frame(outcome = c("Avoids Insecticide",
                                   "Killed by Insecticide",
                                   "Survives Insecticide"),
                       yvals = 1:3)


legend.df = ggplot(legend.df, aes(x= 1, y = yvals, fill = outcome))+
  geom_tile()+
  geom_text(aes(label = outcome), size = 2)+
  scale_fill_manual(values = c("#6baed6", "#fb6a4a", "#807dba"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


the.layout = "
####BBBB####
####BBBB####
####BBBB####
####BBBB####
AAAA####DDDD
AAAA####DDDD
AAAA####DDDD
AAAA####DDDD
####CCCC####
EE##CCCC####
EE##CCCC####
####CCCC####
"



final.plot = before.selection +
  unexposed.plot +
  high.efficacy.plot +
  high.efficacy.end.plot +
  legend.df +
  plot_layout(design = the.layout)

final.plot

ggsave(plot = last_plot(),
       dpi = 600,
       scale = 5,
       units = "px",
       height = 400,
       width = 650,
       filename = "mechanistic_explanation_SI_decay.jpeg"
       )
