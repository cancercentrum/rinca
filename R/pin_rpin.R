#' Generate random (or anonymise existing pins to) non-personal ("fake") pins 
#'
#' \code{rpin} is a generic function to generate non-personal pins for testing and educational purposes.
#' \code{pin_anonymise} is a wrapper to anonymise/de-personalise existing pins.
#' 
#' A pin, where the birth number (digit 9-11 in a 12 number pin) falls in the interval [880, 999], is 
#' a valid personal identification number but is never assigned to an actual person.
#' Numbers of this form can instead be used for testing and educational procedures 
#' without the risk to intefer with personal (and possibly sensitive) data. 
#' 
#' @param x is either an integer (numeric vector of length one) specifing the length of the generated pin vector,
#' or a pin vector itself to be used for generating similair but anonymised pins (see section "Anonymise").
#' @param l_birth,u_birth are dates (or objects that can be coerced to such) constituting a possible time intervall,
#' limiting the period from which birth dates are drawn. 
#' If \code{x} is an integer, these are \code{"1900-01-01"} and \code{Sys.Date()} by default.
#' If \code{x} is a pin vector, these are matched to the birth years in pin.
#' @param unique Should all generated pins be unique, i e should the sampling be done without replacement (\code{TRUE} as default).
#' A possible relation between pins in \code{x} (if x is of class pin) will however be kept if \code{keep_rel = TRUE}.
#' @param male_prob probability that a generated pin refers to a man (\code{female_prob = 1 - male_prob}).
#' If \code{x} is an integer, \code{male_prob} is \code{0.5} by default.
#' If \code{x} is a pin vector, \code{male_prob} is estimated as the observed probability from \code{x}.
#' @param keep_rel Should a possible relationship between pins in \code{x} be kept in the output, i e
#' if the same pin is repeated in \code{x}, should pins at the same positions in the output also be repeated?
#' This is \code{TRUE} by default and works independently of \code{unique}.
#' @param ... additional arguments to be passed to or from methods.
#' 
#' @return
#' \code{rpin} returns a vector of class \code{pin} with length \code{x} if \code{x} is an integer or with length \code{length(x)} 
#' if \code{x} is itself a pin object. The object will also have an extra attribute \code{"non_personal"} set to \code{TRUE} to indicate
#' that the generated pins are non-personal ("fake").
#'
#' @section Simulation:
#' The simulation is done by the following steps:
#' \itemize{
#' \item A birthdate is simulated as described in section \code{Anonymise} or, if \code{x} is an integer,
#' by a uniform distribution from \code{[l_birth, u_birth]}.
#' \item The two first digits of the birth number is given by a discrete random sample from [88, 99]. 
#' Note that these numbers do not speify birthplace in this case (even if year of birth < 1990).
#' \item The last digit of the birth number is sampled from [0, 9] with probabilies according to \code{male_prob} 
#' (that is either specified explicity or as described in section \code{Anonymise}).
#' \item The control number is calculated from digit 1-11 by the Luhn Algorithm 
#' (\code{\link{luhn_algo}}).
#' }
#' 
#' @section Anonymise:
#' Given that \code{x} is an object of class \code{pin}, the output of \code{rpin} 
#' is a pin vector that tries to mimic \code{x} in all aspects 
#' except identifying real persons. 
#' The empirical age (birthday) distribution from \code{x} will be estimated by \code{\link{logspline}}.
#' A random sample of \code{length(x)} is drawn from that distribution. The last four digits are generated
#' as in section \code{Simulation} but with sex distribution estimated from \code{x}. The internal 
#' relationships between elements in \code{x} are maintaind as described for argument
#' \code{keep_rel}.
#' 
#' @export
#' @name rpin 
#' @import sweidnumbr
#' @examples
#' 
#' library(sweidnumbr)
#' set.seed(12345)
#' ## Generate some fake pins
#' p <- rpin(100)
#' 
#' ## Most pin-functions can be applied to p 
#' is.pin(p) # TRUE
#' pin_sex(p) # With mean(pin_sex(p) == "Male") -> male_prob when x -> Inf
#' table(pin_birthplace(p)) # non-informative
#' pin_age(p)
#' pin_to_date(p)
#' 
#' ## If we want to simulate university students in a med course in Sweden,
#' ## we migh try
#' p_ms <- rpin(100, l_birth = "1974-01-01", u_birth = "1994-01-01", male_prob = .25)
#' table(pin_sex(p_ms))
#' summary(pin_age(p_ms))
#' 
#' ## Now, assume for a moment that p_ms is actually real data that we want to anonymise.
#' ## The easy way:
#' p_ms2 <- rpin(p_ms)
#' ## We then have new (fake) numbers but with the same age- and sex distribuiton.
#' table(pin_sex(p_ms2))
#' summary(pin_age(p_ms2))
#' 
#' ## The empirical age distribution from p_ms itself could of course also generate 
#' ## birth dates outside of the empirical birthdate interval from p_ms. The default limit 
#' ## is to not generate pins with birth year before the birth year of the oldest pin in the input
#' ## (and wice versa for the upper limit). But we could also chose to not tolerate any
#' ## pins "older" than the "oldest" pin from the input
#' p_ms3 <- rpin(p_ms, l_birth = min(y <- pin_to_date(p_ms)), u_birth = max(y))
#' min(pin_to_date(p_ms3)) >= min(pin_to_date(p_ms))
#' max(pin_to_date(p_ms3)) <= max(pin_to_date(p_ms))
#' 
#' ## We can modify the sex distribution even though we keep the age-distribution 
#' x <- rpin(p_ms, male_prob = .01) 
#' x <- pin_sex(x) 
#' table(x)

rpin <- function(x, ...){
  UseMethod("rpin")
}



################################################################################
#                                                                              #
#                              Default rpin-method                             #
#                                                                              #
################################################################################

#' @export 
rpin.default <- function(x, ...){
  rpin(as.pin(x), ...)
}




################################################################################
#                                                                              #
#                              numeric rpin-method                             #
#                                                                              #
################################################################################

## This method is just a wrapper if x is neither integer, nor pin.
## It is not documented since it should not be used purposely.
#' @export
rpin.numeric <- function(x, ...){
  if (suppressWarnings(!any(is.na(as.pin(x))) && is.pin(as.pin(x)))){
    rpin(as.pin(x), ...)
  }
  else if (length(x) == 1){
    if ( abs(x - round(x)) >= .Machine$double.eps^0.5){
      warning("x rounded to nearest lower integer!")
    }
    rpin(as.integer(x), ...)
  }
}




################################################################################
#                                                                              #
#                              integer rpin-method                             #
#                                                                              #
################################################################################

#' @export
#' @rdname rpin
rpin.integer <- function(x,
                         l_birth = "1900-01-01",
                         u_birth   = Sys.Date(),
                         unique = TRUE,
                         male_prob = 0.5,
                         ...){
  
  if (l_birth > u_birth){ 
    stop("l_birth should not be a date later than u_birth!")
  }
  
  ## Pos 1-8 (Birthday)
  birth_width <- as.numeric(as.Date(u_birth) - as.Date(l_birth))
  birth_width <- max(birth_width, 1)
  rdates      <- sample.int(birth_width, x, replace = TRUE) - 1
  pos18      <- format(as.Date(rdates, origin = l_birth), format = "%Y%m%d")
  
  ## Add pos 9-12
  pin <- birthdate2pin(pos18, male_prob = male_prob, ...)
  
  ## Only unique pins if unique = TRUE. 
  ## Otherwise resample duplicated subset with rpin.pin_internal
  if (unique){
    pin <- rpin_unique(pin, recursive_fun = rpin.integer,
                       l_birth = l_birth, u_birth = u_birth,
                       unique = TRUE, male_prob = male_prob)
  } 
  
  pin
  
}




################################################################################
#                                                                              #
#                                pin rpin-method                               #
#                                                                              #
################################################################################

#' @export
#' @rdname rpin
rpin.pin <- function(x,
                     l_birth,
                     u_birth,
                     unique = TRUE,
                     male_prob = mean(pin_sex(x) == "Male"),
                     keep_rel = TRUE,
                     ...){
  
  ## If no birth limits specified, we use all years from x
  if (missing(l_birth)){
    l_birth <- format(min(pin_to_date(x)), format = "%Y")
    l_birth <- paste0(l_birth, "-01-01")
  }
  l_birth <- as.Date(l_birth)
  if (missing(u_birth)){
    u_birth <- format(max(pin_to_date(x)), format = "%Y")
    u_birth <- as.Date(paste0(u_birth, "-12-31"))
    u_birth <- min(u_birth, Sys.Date())
  }
  u_birth <- as.Date(u_birth)
  
  ## We only use data from the specified birth interval
  x_birth <- as.Date(pin_to_date(x))
  x_lim <- subset(x, x_birth >= l_birth & x_birth <= u_birth)
  if (length(x_lim) < 1){
    stop("There are no pins from 'x' with birth dates within the specified interval!")
  }
  
  ## We have found by testing that logspline requires at least 10 pins to work
  ## If we have fewer, we use uniform sampling
  if (length(x_lim) < 10){
    warning("Too few birth dates within [", l_birth, ", ", u_birth, "]",
            " for a valid empirical distribution estimate. ",
            "Birth dates are instead simulated uniformly from [", l_birth, ", ", u_birth, "].",
            call. = FALSE)
    return(rpin(length(x), l_birth = l_birth, u_birth = u_birth, 
         unique = unique, male_prob = male_prob)
    )
  }
  
  ## Empirical distribution for birthdays in x  
  dpin <- as.numeric(as.Date(pin_to_date(x_lim)))
  dpin <- logspline::logspline(dpin, lbound = l_birth, ubound = u_birth) 

    
  ## Generate a random sample of pins from the empirical distribution
  pin <- rpin.pin_internal(x = length(x_lim), distribution = dpin, male_prob = male_prob)
    
  ## Only unique pins if unique = TRUE. 
  ## Otherwise recursive resampling with subset of duplicates, using rpin.pin_internal
  if (unique){
    pin <- rpin_unique(pin, recursive_fun = rpin.pin_internal, 
                       distribution = dpin, male_prob = male_prob)
  } 
  
  ## But ... non unique values could be allowed if non unique pins exists in x
  ## Their relationship should be maintained if keep_rel = TRUE
  if (keep_rel && any(x_dups <- duplicated(x))){
    for (i in which(x_dups)){
      pin[i] <- pin[x == x[i]][1]
    }
  }
  
  pin
  
}



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

