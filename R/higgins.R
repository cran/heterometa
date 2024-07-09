`higgins` <-
function(Q = NULL, k = NULL, I2 = NULL, pval = NULL,
   slab = NULL, conflevel = 95) {
# k is number of studies, not df
   HtoI2 <- function(H) (H^2 - 1) / H^2
   chk <- parchck(Q, k, I2, pval, slab, conflevel)
   Q <- chk$Q
      if(chk$error) {
         H = as.data.frame(matrix(rep(NA, 5), nrow = 1))
         res <- list(H = H, I2 = H, call = match.call())
         class(res) <- c("higgins", "list")
      } else {
         H <- pmax(1, sqrt(Q / (k - 1)))
         alpha <- ifelse((conflevel > 1),
            1 - (conflevel / 100), conflevel)
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
         class(res) <- c("higgins", "list")
      }
      res
   }
