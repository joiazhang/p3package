
output <- my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
test_that("output correct", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder), output)
})
