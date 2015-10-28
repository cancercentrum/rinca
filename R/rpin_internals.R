


################################################################################
#                                                                              #
#                            help functions to rpin                            #
#                                                                              #
################################################################################


#' Generate random  pins from birth distribution (given by logspline)
#' @param x length of the generated pin vector
#' @param distribution a logspline object with distribution to sample from
#' @param ... arguments passed to birth2date
#' @return pin vector of length x
rpin.pin_internal <- function(x, distribution, ...){
  
  ## Pos 1 - 8
  pos18 <- 
      format(
          as.Date(
              round(
                  logspline::rlogspline(x, distribution), 
                  0), 
              origin = "1970-01-01"),
          format = "%Y%m%d")
    
    ## Add pos 9 - 12
    birthdate2pin(pos18, ...)
}



#' Given a birtdate, attach the last four digits to make a non-personal ("fake") pin
#' 
#' @param pos18 vector with birtdates
#' @param male_prob proportion of males in the outcome
#' @return pin vector
#' @keywords internal
birthdate2pin <- function(pos18, male_prob = 0.5){
  
  n <- length(pos18)
  
  ## Pos 9-10
  pos910 <- sample(98:99, n, replace = TRUE)
  
  ## Pos 11
  pos11 <- sample(x <- 0:9, n, replace = TRUE, prob = rep(c(1 - male_prob, male_prob), 5))
  
  ## pos 12 - With a bad workaround for luhn_algo that coud hopefully be dropped soon
  pos111 <- paste0(pos18, pos910, pos11)
  pos12 <- suppressMessages(Vectorize(luhn_algo)(paste0(pos111, "0")))
  
  ## Full pin
  pin <- paste0(pos111, pos12)
  
  ## "Formals"
  pin <- as.pin(pin)
  attr(pin, "non_personal") <- TRUE 
  
  pin
}


#' Resample duplicated subset of pin vector to make all elements unique
#' 
#' @param pin a pin vector with possibly non unique elements
#' @param recursive_fun function to regenerate duplicated pins
#' @param ... arguments pased to \code{recursive_fun}
#' @keywords internal
rpin_unique <- function (pin, recursive_fun, ...) {
  
  i  <- 0
  max_recursion <- getOption("expressions") / 10 # We divide by 10 since it will otherwise be too slow!
  
  ## Ue recursion until all values are unique
  while (any(dups <- duplicated(pin)) && i <= max_recursion){
    x <- sum(dups)
    pin[dups] <- do.call(recursive_fun, c(x, list(...))) # r_pos112(sum(dups))
    i  <- i + 1
  }
  
  if (i > max_recursion){
    stop("x is too big and the birth interval too short to make all pins unique!")
  }
  pin
}

