#'@title Generate a near perfect Normal Distribution
#'
#'@description Create a reproducible and deterministic Normal distribution of trait values in a population.
#'
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviaton = The standard deviation of the trait mean in the population.


#Code obtained from stackoverflow, https://stackoverflow.com/questions/50258330/generate-a-perfectly-normally-distributed-sample-of-size-n-in-r, Dominique Makowski  #

create_normal_distribution = function(vector.length,
                                      trait.mean,
                                      standard.deviation){

normal.distribution = stats::qnorm(seq(from = 1/vector.length,
                                       to =1-1/vector.length,
                                       length.out = vector.length),
                 mean=trait.mean, sd=standard.deviation)

return(normal.distribution)
}

