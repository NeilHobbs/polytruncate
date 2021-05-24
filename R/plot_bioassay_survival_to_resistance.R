#' Plot the relationship between survival and resistance calculated from bioassay survival
#' @import ggplot2
#'
#' @export
#'
#' @param maximum.bioassay.survival.proportion Should be set as 1.
#' @param michaelis.menten.slope Should be set as 1
#' @param half.population.bioassay.survival.resistance This is calculated using the calculate_half_population_resistance function
#' @param estimate.precision How precise your estimate of insecticide restistance intensity should be. Recommend values between 0.01 to 0.001
#' @param minimum.resistance.score Recommend setting as 0.
#' @param maximum.resistance.score This will depend on the half survival scale, but 25000 would be a good start.
#' @param minimum.bioassay.survival As a proportion, should be set as 0
#' @param maximum.bioassay.survival As a proportion, should be set as 1
#' @param divisions The resolution of the final plotted graph
#'
#' @return A ggplot of the relationship between resistance (x axis) and survival (y axis).

plot_bioassay_survival_to_resistance = function(maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                half.population.bioassay.survival.resistance = 900,
                                                estimate.precision = 0.01,
                                                minimum.resistance.score = 0,
                                                maximum.resistance.score = 25000,
                                                minimum.bioassay.survival = 0,
                                                maximum.bioassay.survival = 1,
                                                divisions = 0.01){

bioassay.survival.values=seq(minimum.bioassay.survival, maximum.bioassay.survival,
                                             by = divisions)
resistance.scores = c()

for(i in 1:length(bioassay.survival.values)){
  resistance.scores[i] = convert_bioassay_survival_to_resistance_score(
      maximum.bioassay.survival.proportion=maximum.bioassay.survival.proportion,
      michaelis.menten.slope=michaelis.menten.slope,
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance,
      bioassay.survival=bioassay.survival.values[i],
      estimate.precision=estimate.precision,
      minimum.resistance.value = minimum.resistance.score,
      maximum.resistance.value=maximum.resistance.score)
}


df = data.frame(bioassay.survival.values, resistance.scores)

  ggplot2::ggplot(df, aes(x=resistance.values, y = bioassay.survival.values)) +
    geom_line(colour = "red") +
    xlab("Polygenic Resistance Score") +
    ylab("WHO Cylinder Survival Proportion") +
    theme_classic()
}


#Need to figure out a way to get rid of the weird up-tick
#Also:
#Dotted line for 10% Survival
#Dotted line for 50% Survival
