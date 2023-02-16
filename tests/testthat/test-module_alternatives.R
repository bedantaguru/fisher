test_that("alternatives works - I", {

  xyz <- function(x){
    alternatives()
  }


  xyz_alt_alt1 <- function(x){
    cat(x)
    "Alt1"
  }

  xyz_alt_alt2 <- function(x){
    cat(x)
    "Alt2"
  }

  xyz_altMeta_alt1 <- function(){
    list(
      system = list(
        abc = function() {
          cat("abc\n")
          TRUE
        },
        abc2 = function(){
          cat("abc2\n")
          TRUE
        }
      ))
  }


  xyz_altMeta_alt2 <- function(){
    cat("pkgs inst\n")
    list(
      packages = "methods"
    )
  }

  out1 <- capture_output(xyz("x input"))
  out2 <- capture_output(xyz("x input"))

  expect_true(grepl("abc",out1))
  expect_true(grepl("pkgs inst",out1))
  expect_false(grepl("abc",out2))
  expect_false(grepl("pkgs inst",out2))
  expect_true(grepl("No alternatives is set to use",out1))
  expect_true(grepl("No alternatives is set to use",out2))


  chk <- all(alternatives(xyz)$Available)
  expect_true(chk)
  # this is for sys.calls working fine check
  expect_true(all(alternatives(xyz)$Available))

  pkg_sim_done <- FALSE
  if(exists("is_available") & exists("not_available")){
    # it means {packageAvailabilitySimulate} present
    not_available("methods")
    # clean env alternatives_env (remove cached values)
    rm(list = ls(envir = alternatives_env), envir = alternatives_env)
    expect_equal(alternatives("xyz")$Available, c(TRUE, FALSE))
    out3 <- capture_output(
      expect_warning(alternatives(xyz, install = "alt2"))
    )
    expect_true(grepl("pkgs inst",out3))

    pkg_sim_done <- TRUE

  }

  out4 <- capture_output(xyz("x input"))
  expect_true(grepl("x input", out4))
  expect_true(grepl("No alternatives is set to use", out4))
  expect_true(grepl("No alternatives is set to use",
                    capture_output(xyz("x"))))

  expect_output(alternatives("xyz", use = "alt1"), "will use alternative <alt1>")
  expect_equal(xyz("x"), "Alt1")
  expect_failure(expect_equal(xyz("x"), "Alt2"))

  if(pkg_sim_done){
    expect_warning(alternatives("xyz", use = "alt2"),"failed to use alternative <alt2>")
    # reset {packageAvailabilitySimulate}
    not_available()
    # clean env alternatives_env (remove cached values)
    rm(list = ls(envir = alternatives_env), envir = alternatives_env)
    capture_output(alternatives(xyz))

  }

  expect_output(alternatives(xyz, use = "alt2"), "will use alternative <alt2>")
  expect_equal(xyz("x"), "Alt2")
  expect_failure(expect_equal(xyz("x"), "Alt1"))


})



test_that("alternatives works - II", {

  udf <- function(x, yoo, zoo){
    alternatives("udf")
  }

  noudf <- function(x, yoo, zoo){
    alternatives("noudf")
  }

  xudf <- udf

  udf_alt_alt1 <- function(x,yoo){
    x+yoo
  }

  udf_alt_alt2 <- function(x,yoo, zoo){
    x+yoo+zoo
  }

  udf_alt_alt3 <- function(x,yoo){
    x+yoo
  }



  udf_altMeta_alt1 <- function(){
    list(
      system = list(
        abc = "hello",
        abc2 = function(){
          "no logical"
        }
      )
    )
  }

  udf_altMeta_alt3 <- function(){
    list(
      system = list(
        abc = function(){
          TRUE
        }
      )
    )
  }

  udf_altMeta_alt4 <- function(){
    list(
      system = list(
        abc = function(){
          stop("fail", call. = FALSE)
        }
      )
    )
  }

  udf_altMeta_alt5 <- "not a fn"


  expect_output(
    expect_equal(
      udf(1,2,3),
      6
    ),
    "Using the first available"
  )

  # renaming alternative is not supported yet
  expect_error(
    xudf(1,2,3)
  )

  expect_output(alternatives(udf, use = "alt3"),
                "Method <udf> will use alternative <alt3>")

  expect_false("alt4" %in% alternatives(udf)$Alternatives )

  udf_alt_alt4 <- function(x,yoo, zoo){
    x+yoo*zoo
  }

  alternatives(udf, refresh = TRUE)

  expect_failure(expect_false("alt4" %in% alternatives(udf)$Alternatives))

  udf_alt_alt5 <- function(x,yoo, zoo){
    "fake"
  }

  expect_warning(
    alternatives(udf, refresh = TRUE),
    "Meta function for alternative <alt5> of method <udf> failed to run"
  )

  #  not a real test
  alternatives_attach("utils")
  alternatives_attach("methods")

  expect_error(alternatives_attach("A3"), "Kindly load the package")
  expect_error(noudf(),"No alternatives found")


  noudf_alt_alt1 <- function(x,yoo){
    x+yoo
  }

  noudf_altMeta_alt1 <- function(){
    list(
      system = list(
        abc = "FALSE this will be false"
      )
    )
  }

  alternatives(noudf, refresh = TRUE)

  expect_error(noudf(),"No alternatives is available for method <noudf>")



})

