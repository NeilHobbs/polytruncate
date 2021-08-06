trait.mean.values = runif(10, min = 0, max = 1000)
sd.values = runif(10, min = 10, max=50)

for(i in 1:length(trait.mean.values)){
test_that("maintains the trait mean", {
  expect_equal(mean(create_normal_distribution(vector.length=1000,
                                               trait.mean = trait.mean.values[i],
                                               standard.deviation = sd.values[i])), trait.mean.values[i])
})}


temp = create_normal_distribution(vector.length=1000,
                           trait.mean = 0,
                           standard.deviation = 12)

test_that("in value is negative of max value", {
    expect_equal(temp[1], -temp[1000])
  })
