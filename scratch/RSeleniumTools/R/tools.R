
#' Read source html code from the client
#'
#' @param client The selenium client from where the source is required to read
#'
#' @return An XML document (similar to \code{xml2::read_html})
#' @export
#'
#' @examples
#' selenium_storm()
#' cl <- selenium_storm_client()
#' read_source(cl)
read_source <- function(client){
  client$getPageSource()[[1]] %>% xml2::read_html()
}


#' Checks whether a specific set of texts are present in current browser or not
#'
#' @param client The selenium client
#' @param txt The text to search
#'
#' @return Logical scaler indicating presence of the text in the client
#' @export
#'
is_present_texts <- function(client, txt){

  read_source(client) %>% rvest::html_text() %>% stringr::str_detect(txt) %>% all

}

#' Wait for a Text to appear in the browser
#'
#' @param client The selenium client
#' @param txt The text to search
#' @param wait_time wait time in sec (default is 10)
#'
#' @export
#'
wait_for_texts <- function(client, txt, wait_time = 10){
  #attempt(ifelse(is_present_texts(client, txt), TRUE, stop()), wait_time = wait_time)
  attempt(need(is_present_texts(client, txt)), wait_time = wait_time)
}

#' Attempt something few times
#'
#' @param expr Expression to run
#' @param wait_time Wait time in sec (default 10)
#'
#' @return \code{NULL} if the \code{expr} fails to run (or genuinely returns \code{NULL}). Otherwise it returns (invisibly) the value of the expression.
#' @export
#'
#' @examples
#' print(attempt(log(1)))
#' print(attempt(log("a")))
attempt<- function(expr, wait_time = 10, num_iters = 5){
  suppressMessages(suppressWarnings(.attempt(expr = expr, wait_time = wait_time, num_iters = num_iters)))

}

# raw function
.attempt<- function(expr, wait_time = Inf, num_iters = Inf){
  t0 <- Sys.time()
  itr <- 0
  e <- NULL
  repeat({

    e <- try(expr, silent = T)

    if(!inherits(e, "try-error")){
      break()
    }else{
      Sys.sleep(0.1)
    }

    t1 <- Sys.time()
    te <- difftime(t1, t0, units = "sec")

    itr <- itr+1

    if(te>wait_time){
      break()
    }

    if(itr>num_iters){
      break()
    }

  })

  if(inherits(e, "try-error")){
    e <- NULL
  }

  invisible(e)

}


#' Alias for stopifnot
#' @details This is to be added in attempt block based on requirements. The \code{TRUE} logical value mentioned under \code{need} will end the attmpt block.
#' It should be given in the end of attempt block
#' @export
#'
#' @examples
#' set.seed(1)
#' attempt({
#' u <- rnorm(1,mean = -2)
#' need(u>0)})
#' u
#' #0.4016178
need <- stopifnot

#' Create a safe version of a function
#'
#' @param .f A function, formula, or atomic vector. Same as \code{purrr} style.
#' @param instant Default is FALSE, If enabled only one time \code{.f} will be attempted.
#'
#' @return a safe version of the input function
#' @export
#'
#' @examples
#' slog <- safe(log, instant = T)
#' print(slog(1))
#' print(slog("1"))
#'
safe <- function(.f, instant = F, wait_time = 5){
  .f <- purrr::as_mapper(.f)
  if(instant){
    .f_safe <- function(...){
      attempt(.f(...), wait_time = 0)
    }
  }else{
    .f_safe <- function(...){
      attempt(.f(...), wait_time = wait_time)
    }
  }

  return(.f_safe)

}

