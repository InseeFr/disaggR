test_that("warning news factory", {
  warn_fun <- warning_news_factory("test_me")
  expect_warning(warn_fun())
  expect_no_warning(warn_fun())
})
