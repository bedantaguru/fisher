test_that("persistent_object_store works", {

  f_tst <- function(){
    h <-persistent_object_store("test")
    h$write("alt_tst","one_of_the_alt")
    expect_equal(h$read("alt_tst"), "one_of_the_alt")
    h$open_location()
    h$destroy()
    expect_false(dir.exists(h$store_path))
  }

  f_tst()

  # force redirect to rappsdir
  options(prefer_non_base_pkgs = TRUE)
  f_tst()

  options(prefer_non_base_pkgs = FALSE)

  # simulate rstudio :-)
  env <- new.env()
  env$.rs.getProjectDirectory <- function(){
    tempdir()
  }

  base::attach(what = env, pos = 2L, name = "test_attach1503")

  f_tst()

  detach(name = "test_attach1503")

})
