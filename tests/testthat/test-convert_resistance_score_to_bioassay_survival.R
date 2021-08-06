test.values = c(1, 50, 100, 1000, 5000)

for(i in 1:length(test.values)){
  test_that("returns 50% survival",{
    expect_equal(convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                               trait.mean = test.values[i],
                                                               michaelis.menten.slope = 1,
                                                               half.population.bioassay.survival.resistance = test.values[i]),
                 0.5)})}
