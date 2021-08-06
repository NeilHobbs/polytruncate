test_that("not heritable", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 0,
                                                                      intervention.before.selection = 100,
                                                                      female.fitness.cost = 0,
                                                                      male.fitness.cost = 0)
    , 100)
})

test_that("male fitness cost works", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 1,
                                                                      intervention.before.selection = 100,
                                                                      female.fitness.cost = 0,
                                                                      male.fitness.cost = 2)
               , 99)
})

test_that("female fitness cost works", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 1,
                                                                      intervention.before.selection = 100,
                                                                      female.fitness.cost = 2,
                                                                      male.fitness.cost = 0)
               , 99)
})

test_that("female+male fitness cost works", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 1,
                                                                      intervention.before.selection = 100,
                                                                      female.fitness.cost = 2,
                                                                      male.fitness.cost = 2)
               , 98)
})
