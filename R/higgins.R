`higgins` <-
function(Q = NULL, k = NULL, pval = NULL, slab = NULL, conflevel = 0.95) {
# k is number of studies, not df
   if (is.null(k)) stop('Must supply value for Q and k')
   if(is.null(Q) & is.null(pval)) stop("Must supply value for Q or pval")
   stopifnot(Q > 0, k > 1, (conflevel > 0 & conflevel < 1),
      (pval > 0 & pval < 1)
   )
   if(any(conflevel < 0.5))
      warning("Did you supply alpha instead of conflevel?")
   if(is.null(Q)) {
      Q <- qchisq(pval, (k - 1), lower.tail = FALSE) # works vectorised
   }
   stopifnot(length(Q) == length(k))
   HtoI2 <- function(H) (H^2 - 1) / H^2
   H <- pmax(1, sqrt(Q / (k - 1)))
   alpha <- 1 - conflevel
   zalpha <- qnorm(1 - alpha / 2)
   seln.H <- ifelse(Q > k, 0.5 * (log(Q) - log(k - 1)) /
         (sqrt(2 * Q) - sqrt(2 * k - 3)), # Q > k
      sqrt(ifelse(k > 2, (1 / (2 * (k - 2))) *
            (1 - 1 / (3 * (k - 2) ^ 2)), # k > 2
         NA))) # k == 2
   ll.H <- pmax(1, exp(log(H) - zalpha * seln.H))
   ul.H <- pmax(1, exp(log(H) + zalpha * seln.H))
   tempH = data.frame(Q = Q, k = k, H = H, ll = ll.H, ul = ul.H)
   tempI2 = data.frame(Q = Q, k = k, I2 = HtoI2(H),
       ll = HtoI2(ll.H), ul = HtoI2(ul.H))
   if(!is.null(slab) & (length(slab) == length(Q))) {
      rownames(tempH) <- slab
      rownames(tempI2) <- slab
   }
   res <- list(H = tempH, I2 = tempI2, call = match.call())
   class(res) <- "higgins"
   res
}
