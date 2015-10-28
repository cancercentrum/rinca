
#' @export
#' @rdname rpin
rpin.integer <- function(x,
                         l_birth = "1900-01-01",
                         u_birth   = Sys.Date(),
                         unique = TRUE,
                         male_prob = 0.5,
                         ...){
    
    if (l_birth > u_birth) { 
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
    if (unique) {
        pin <- rpin_unique(pin, recursive_fun = rpin.integer,
                           l_birth = l_birth, u_birth = u_birth,
                           unique = TRUE, male_prob = male_prob)
    } 
    
    pin
    
}


