
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
    if (missing(l_birth)) {
        l_birth <- format(min(pin_to_date(x)), format = "%Y")
        l_birth <- paste0(l_birth, "-01-01")
    }
    l_birth <- as.Date(l_birth)
    if (missing(u_birth)) {
        u_birth <- format(max(pin_to_date(x)), format = "%Y")
        u_birth <- as.Date(paste0(u_birth, "-12-31"))
        u_birth <- min(u_birth, Sys.Date())
    }
    u_birth <- as.Date(u_birth)
    
    ## We only use data from the specified birth interval
    x_birth <- as.Date(pin_to_date(x))
    x_lim <- subset(x, x_birth >= l_birth & x_birth <= u_birth)
    if (length(x_lim) < 1) {
        stop("There are no pins from 'x' with birth dates within the specified interval!")
    }
    
    ## We have found by testing that logspline requires at least 10 pins to work
    ## If we have fewer, we use uniform sampling
    if (length(x_lim) < 10) {
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
    if (unique) {
        pin <- rpin_unique(pin, recursive_fun = rpin.pin_internal, 
                           distribution = dpin, male_prob = male_prob)
    } 
    
    ## But ... non unique values could be allowed if non unique pins exists in x
    ## Their relationship should be maintained if keep_rel = TRUE
    if (keep_rel && any(x_dups <- duplicated(x))) {
        for (i in which(x_dups)) {
            pin[i] <- pin[x == x[i]][1]
        }
    }
    
    pin
    
}

