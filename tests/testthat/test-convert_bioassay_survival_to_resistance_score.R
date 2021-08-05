#These tests confirm that the conversion from bioassay survival to resistance intensity work correctly:


#Error Messages
test_that("errror for incorrect max survival", {
  expect_error(convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 2,
                                               michaelis.menten.slope = 1, #must be set to 1 to work properly
                                               half.population.bioassay.survival.resistance = 900,
                                               bioassay.survival = 0.5,
                                               estimate.precision = 0.00001,
                                               minimum.resistance.value = 0,
                                               maximum.resistance.value = 25000), "maximum.bioassay.survival.proportion must equal 1.")})

test_that("errror for incorrect michaelis.menten.slope",{

  expect_error(convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                               michaelis.menten.slope = 2, #must be set to 1 to work properly
                                               half.population.bioassay.survival.resistance = 900,
                                               bioassay.survival = 0.5,
                                               estimate.precision = 0.00001,
                                               minimum.resistance.value = 0,
                                               maximum.resistance.value = 25000), "michaelis.menten.slope must equal 1")})

test.values = c(-1, 1.1)
for(i in 1:length(test.values)){
  test_that("errror bioassay survival",{

    expect_error(convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                 half.population.bioassay.survival.resistance = 900,
                                                 bioassay.survival = test.values[i],
                                                 estimate.precision = 0.00001,
                                                 minimum.resistance.value = 0,
                                                 maximum.resistance.value = 25000), "Bioassay survival must be between 0 and 1.")})}


# #Warning Messages
test_that("high minimum.resistance.value",{
  expect_warning(convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                 half.population.bioassay.survival.resistance = 900,
                                                 bioassay.survival = 0.5,
                                                 estimate.precision = 0.00001,
                                                 minimum.resistance.value = 50,
                                                 maximum.resistance.value = 25000), "High input for minimum.resistance.value, bioassay survival could be out of range.")
})

test_that("low maximum.resistance.value",{
  expect_warning(convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                 half.population.bioassay.survival.resistance = 900,
                                                 bioassay.survival = 0.5,
                                                 estimate.precision = 0.00001,
                                                 minimum.resistance.value = 0,
                                                 maximum.resistance.value = 1500), "Low input for maximum.bioassay.survival.proportion, bioassay survival could be out of range.")
})

test_that("low minimum.resistance.value",{
  expect_warning(convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                 half.population.bioassay.survival.resistance = 5000,
                                                 bioassay.survival = 0.5,
                                                 estimate.precision = 0.00001,
                                                 minimum.resistance.value = 0,
                                                 maximum.resistance.value = 4000), "half.population.survival.resistance outside resistance value range")
})
