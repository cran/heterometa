`print.higgins` <-
function(x, type = "I2", na.print = "", ...) {
   if(is.na(x$H[1,1])) {
      warning("No results found from higgins")
   } else {
      if(type == "both") {
         temp <- cbind(x$H[,1:2], round(x$H[,3:5], 2),
            round(100 * x$I2[,3:5]))
         print(as.matrix(temp), ma.print = na.print, ...)
      } else if(type == "H") {
         H <- cbind(x$H[,1:2], round(x$H[,3:5], 2)) # round H to 2dp
         print(as.matrix(H), na.print = na.print, ...)
      } else if(type == "I2") {
         I2 <- cbind(x$I2[,1:2], round(100 * x$I2[,3:5])) # I2 as percent
         print(as.matrix(I2), na.print = na.print, ...)
      }
      else {
         warning("type must be H, I2, or both")
      }
   }
   invisible(x)
}
