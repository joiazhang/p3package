test_that("alternative = less works", {
  expect_equal(my_t.test(my_gapminder$lifeExp, alternative = "less", 60), list("test_stat" = -1.67954774, "df" = 1703, "alternative" = "less", "p_val" = 0.046614385))
})

test_that("alternative = greater works", {
  expect_equal(my_t.test(my_gapminder$lifeExp, alternative = "greater", 60), list("test_stat" = -1.67954774, "df" = 1703, "alternative" = "greater", "p_val" = 0.95338562))
})

test_that("alternative = two.sided works", {
  expect_equal(my_t.test(my_gapminder$lifeExp, alternative = "two.sided", 60), list("test_stat" = -1.6795477, "df" = 1703, "alternative" = "two.sided", "p_val" = 0.09322877))
  expect_equal(my_t.test(x = my_gapminder$lifeExp, mu = 60), list("test_stat" = -1.679548, "df" = 1703, "alternative" = "two.sided", "p_val" = 0.09322877))
})

test_that("invalid alternative input throws error", {
  expect_error(my_t.test(my_gapminder$lifeExp, alternative = "not less, greater, or two.sided", 60))
})
