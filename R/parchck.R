parchck <-
function(Q = NULL, k = NULL, I2 = NULL, pval = NULL,
   slab = NULL, conflevel = 95) {
   error <- FALSE
   if (is.null(k)) {
      warning('Must supply value for k')
      error >- TRUE
   } else if(is.null(Q) & is.null(pval) & is.null(I2)) {
      warning("Must supply value for Q or I2 or pval")
      error <- TRUE
   }
#   if(any(conflevel < 0.5))
#      warning("Did you supply alpha instead of conflevel?")
   if(is.null(Q)) {
      if(!is.null(pval)) {
         Q <- qchisq(pval, (k - 1), lower.tail = FALSE) # works vectorised
      } else if(!is.null(I2)) {
         if(any(I2 < 0) | any(I2 > 100)) {
            warning("I2 must be in [0:100]")
            error <- TRUE
         }
         ifelse(I2 > 1, I2 <- I2 / 100, I2) # make sure is in [0:1]
         H2 <- -1 / (I2 - 1)
         Q <- H2 * (k - 1)
      }
   }
#### now have value of Q
   if(any(Q <0 ) | any(k < 2) | any(conflevel < 0) | any(conflevel > 100)) { 
      error <- TRUE
      if (any(Q < 0)) warning("Q must be non-negative")
      if (any(k < 2)) warning("k must be greater than 1")
      if (any(conflevel < 0) | any(conflevel > 1))
         warning("conflevel must be in [0,1]")
   }
   if(length(Q) != length(k)) warning("Length of Q != length of k")
   res <- list(Q = Q, error = error)
   res
}

