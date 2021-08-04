#Diagrammatic Representation of Polytruncate

z.values = create_normal_distribution(vector.length = 10000,
                                      trait.mean = 0,
                                      standard.deviation = 20)
relative.frequency = calculate_density_of_trait_values(vector.length = 10000,
                                                       trait.mean = 0,
                                                       standard.deviation = 20)*1000

df = data.frame(z.values, relative.frequency)

ggplot(df, aes(x=z.values, y=relative.frequency))+
  geom_line(size= 3, colour = "black")+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

#This is then separated into those that go on to encounter the insecticide; and those that
#do not encounter the insecticide.
ggplot(df, aes(x=z.values,
               y=relative.frequency*0.7))+
  geom_line(size = 3, colour = "red",
            alpha = 0.5)+
  geom_line(aes(x=z.values,
                y=relative.frequency*0.3), colour = "green",
            size = 3, alpha = 0.5)+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))


#
ggplot(df, aes(x=z.values,
               y=relative.frequency*0.7))+
  geom_line(size = 3, colour = "red",
            alpha = 0.5)+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))


convert_bioassay_survival_to_field_survival(bioassay.survival = 0,
                                            regression.coefficient = 0.48,
                                            regression.intercept = 0.15,
                                            current.insecticide.efficacy = 1)


vals = cumsum(relative.frequency)
percent = vals/(sum(relative.frequency))

df2 = data.frame(vals, percent, z.values, relative.frequency)

df3 = df2%>%
  dplyr::filter(percent <= 0.85005 & percent >= 0.84995)
#14.6536

df4 = df2%>%
  dplyr::filter(z.values >= 14.6536)

df5 = df2%>%
  dplyr::filter(z.values < 14.6536)

ggplot(df, aes(x=z.values,
               y=relative.frequency*0.7))+
  geom_line(size = 3, colour = "red",
            alpha = 0.5)+
  geom_area(data = df5, aes(x=z.values,
                            y=relative.frequency*0.7),
            fill = "black")+
  geom_area(data = df4, aes(x=z.values,
                            y=relative.frequency*0.7),
            fill = "orange")+
  geom_vline(xintercept = df3$z.values,
             colour = "orange")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

##
#We can then add these together:
df2$relative.frequency.1 = ifelse(z.values >= 14.6536,
                                  yes = relative.frequency*0.7,
                                  no = 0)
mean(df2$relative.frequency.1 * df$z.values)


ggplot(df, aes(x=z.values,
               y=relative.frequency))+
  geom_line(data = df, aes(x=z.values,
                            y=relative.frequency*0.3),
            colour = "green",
            size = 3)+
  geom_line(data = df4, aes(x=z.values,
                            y=relative.frequency*0.7),
            colour = "orange",
            size = 3)+
  geom_vline(xintercept = 0, colour = "black")+
  geom_vline(xintercept = mean(df2$relative.frequency.1 * df$z.values),
             colour = "orange")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))


###
Nu = sum(relative.frequency*0.3)
Zu = 0

Ne = sum(relative.frequency)*0.7*0.15
Ze = calculate_exposed_selection_differential_truncation(standard.deviation = 20,
                                                         field.survival = 0.15)

Ns = Nu+Ne

(Nu*Zu)+(Ne*Ze)/(Ne+Nu)
#8.05981

ggplot(df, aes(x=z.values,
               y=relative.frequency.update))+
  geom_line(colour = "blue",
            size = 3)+
  geom_vline(xintercept = 0,
             colour = "black")+
  geom_vline(xintercept = mean(df2$relative.frequency.1 * df$z.values),
             colour = "orange")+
  geom_vline(xintercept = 8.05981,
             colour = "blue")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))


