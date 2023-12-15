test_that("core dirq API works", {
  q <- dirq(tempfile())
  expect_true(file.exists(q$path()))
  expect_equal(q$count(), 0)
  q$add(list(data = 2:3), mode = "json")
  q$add("\"128\"")
  q$add("My sentence is not long.")
  expect_equal(q$count(), 3)
})


# Setup
# require(devtools)
# devtools::update_packages("devtools")
# devtools::install_dev_deps()

# Typical workflow
# load_all()
# devtools::test()
# devtools::check()