test_that("basic dirq class interface works", {
  path <- tempfile()
  q <- dirq(path)
  expect_true(file.exists(q$path()))
  expect_equal(q$count(), 0)

  q$add(list(data = 2:3), format = "json")
  q$add("\"128\"", format = "utf8")
  q$add("My sentence is not long.")
  expect_equal(q$count(), 3)

  first <- q$iter_first()
  expect_length(first, 1)
  cat(first)

  second <- q$iter_next(first)
  expect_length(second, 1)
  cat(second)

  third <- q$iter_next(second)
  expect_length(third, 1)
  cat(third)

  fourth <- q$iter_next(third)
  expect_null(fourth)

  expect_true(q$lock(third))
  expect_equal(q$get(third), "My sentence is not long.")
  expect_true(q$unlock(third))
})


# Setup
# require(devtools)
# devtools::update_packages("devtools")
# devtools::install_dev_deps()

# Typical workflow
# load_all()
# devtools::test() - tests/testthat/*.R
# devtools::document() - NAMESPACE
# devtools::build_readme() - README.Rmd
# devtools::check()