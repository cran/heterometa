`print.higgins` <-
function(x, type = "I2", na.print = "", ...) {
   if(type == "H" | type == "both") {
      H <- cbind(x$H[,1:2], round(x$H[,3:5], 2)) # round H to 2dp
      print(as.matrix(H), na.print = na.print, ...)
   }
   if(type == "I2" | type == "both") {
      I2 <- cbind(x$I2[,1:2], round(100 * x$I2[,3:5])) # I2 as percent
      print(as.matrix(I2), na.print = na.print, ...)
   }
   invisible(x)
}
