# SYMMETRY PLOTS FOR Y AND RESIDUALS
# symmetryplot <- function (y) 
# {
#   y1 <- sort(y)
#   n <- length(y)
#   m <- median(y)
#   unten <- rev(m - y1[1:floor((n + 1)/2)])
#   oben <- y1[ceiling((n + 1)/2):n] - m
#   plot(unten, oben, type = "p", pch = 16, col = "blue")
#   abline(0, 1, col = "red", lwd = 2)
# }
# TO DO 
# I) NEEDS TITLES
# 
###################################################################################
###################################################################################
###################################################################################
require(ggplot2)
###################################################################################
###################################################################################
###################################################################################
y_symmetry <- function (y, title) 
{
  xlab <- deparse(substitute(y))
  txt.title <- if (missing(title))   paste("Symmetry plot of variable", xlab)
  else title    
  lower_half <- upper_half <- NULL
    y1 <- sort(y)
     n <- length(y)
     m <- median(y)
   low <- rev(m - y1[1:floor((n + 1)/2)])
    up <- y1[ceiling((n + 1)/2):n] - m
   da1 <- data.frame(lower_half=low, upper_half=up)
  ggplot(data=da1, aes(x=lower_half, y=upper_half))+
    geom_point()+
    geom_abline(slope=1, intercept=0, colour="red") +
    ggtitle(txt.title)
}
####################################################################################
####################################################################################
####################################################################################
resid_symmetry <- function (model, title) 
{
  xlab <- deparse(substitute(model))
txt.title <- if (missing(title))   paste("resid symmetry plot of model",xlab)
  else title  
lower_half <- upper_half <- NULL
  y <- resid(model)
  y1 <- sort(y)
  n <- length(y)
  m <- median(y)
  low <- rev(m - y1[1:floor((n + 1)/2)])
  up <- y1[ceiling((n + 1)/2):n] - m
  da1 <- data.frame(lower_half=low, upper_half=up)
  ggplot(data=da1, aes(x=lower_half, y=upper_half))+
    geom_point()+
    geom_abline(slope=1, intercept=0, colour="red") +
    ggtitle(txt.title)
}
####################################################################################
####################################################################################
####################################################################################