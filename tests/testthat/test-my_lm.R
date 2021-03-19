test_that("correct output type", {
  expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder), list)
})
