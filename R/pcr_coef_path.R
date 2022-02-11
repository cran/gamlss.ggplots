#########################################################################
#########################################################################
##### I have added col.fill 8-3-21
##### but it nedds more work on how to  deal with the legend
##### see page 115 of the book on ggplot to learn about legends
#########################################################################
#########################################################################
# this is a way to plot paths for a fitted model using fitPCR() function 
# for color gray((1:ncol(da))/(ncol(da)))[col]
# PCR_coef_path <- function(x, 
#                           legend = FALSE, 
#                             plot = TRUE,
#                         col.fill =  hcl.colors(ncol(da), palette="viridis")[col] )
# {
#        M <- length(x$gamma)
#       da <- t(x$coefficients[1:M, 1:M])
#   lambda <- group <- NULL
#      col <- c(col(da))
#      #coo <-  hcl.colors(ncol(da), palette="viridis")[col] 
#   if (legend)
#   {
#     pp <-  qplot(c(row(da)), c(da),  group = col.fill, colour = col.fill, geom = "line", 
#                  ylab="beta", xlab="lambda")
#   } else
#   {
#    # dat <- data.frame(lambda=c(row(da)), beta=c(da), group=c(col(da)))
#     dat <- data.frame(lambda=c(row(da)), beta=c(da), group=col.fill)
#     pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, col=group)) + 
#           geom_line() +
#           theme(legend.position = "none") 
#   }
#   if (plot) {
#     suppressWarnings(return(pp))
#   }
#   else {
#     invisible(pp)
#   }
# }
#########################################################################
#########################################################################
pcr_coef_path <- function(x, legend=FALSE, plot=TRUE)
{
     M <- length(x$gamma)
    da <- t(x$coefficients[1:M, 1:M])
    lambda <- group <- NULL
    col <- c(col(da))
    if (legend)
    {
    pp <-  qplot(c(row(da)), c(da),  group = col, colour = col, geom = "line",
           ylab="beta", xlab="lambda")
    } else
    {
      dat <- data.frame(lambda=c(row(da)), beta=c(da), group=c(col(da)))
      pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, col=group)) + 
            geom_line()+
            theme(legend.position = "none")
    }
if (plot) print(pp)
invisible(pp)
}
#########################################################################
#########################################################################
#########################################################################
# PCR_coef_path_gray <- function(x, 
#                             legend = FALSE, 
#                             plot = TRUE,
#                             col.fill =  hcl.colors(ncol(da), palette="viridis")[col] )
# {
#   M <- length(x$gamma)
#   da <- t(x$coefficients[1:M, 1:M])
#   lambda <- group <- NULL
#   col <- c(col(da))
#   #coo <-  hcl.colors(ncol(da), palette="viridis")[col] 
#   if (legend)
#   {
#     pp <-  qplot(c(row(da)), c(da),  group = col.fill, colour = col.fill, geom = "line", 
#                  ylab="beta", xlab="lambda")
#   } else
#   {
#     # dat <- data.frame(lambda=c(row(da)), beta=c(da), group=c(col(da)))
#     # poo <- gray((1:ncol(da))/(ncol(da)+1))[col]
#     # poo1 =  hcl.colors(ncol(da), palette="viridis")[col]
#     # dat <- data.frame(lambda=c(row(da)), beta=c(da), group=col.fill)
#     # pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, color=group)) + 
#     #  # geom_line(col=gray(.5)) +
#     #   geom_line()
#     #   theme(legend.position = "none") 
#       dat <- data.frame(lambda=c(row(da)), beta=c(da), group=col.fill)
#       pp <- ggplot(dat, aes(x=lambda, y=beta, group=group)) + 
#         geom_line(col=gray(.5)) +
#         theme(legend.position = "none")   
#   }
#   if (plot) {
#     suppressWarnings(return(pp))
#   }
#   else {
#     invisible(pp)
#   }
# }
#########################################################################
#########################################################################
# PCR_coef_path_viridis <- function(x, 
#                           legend = FALSE, 
#                           plot = TRUE,
#                           option = "D")
# {
#   M <- length(x$gamma)
#   da <- t(x$coefficients[1:M, 1:M])
#   lambda <- group <- NULL
#   col <- c(col(da))
#   #coo <-  hcl.colors(ncol(da), palette="viridis")[col] 
#   if (legend)
#   {
#     pp <-  qplot(c(row(da)), c(da),  group = col.fill, colour = col.fill, geom = "line", 
#                  ylab="beta", xlab="lambda")
#   } else
#   {
#     # dat <- data.frame(lambda=c(row(da)), beta=c(da), group=c(col(da)))
#     dat <- data.frame(lambda=c(row(da)), beta=c(da), coef=col)
#     
#     pp <-ggplot(dat, aes(x=lambda, y=beta, group=coef))+
#       geom_line(aes(color = coef)) +
#       scale_color_viridis(option = option)+
#       theme(legend.position = "none") 
#   }
#   if (plot) {
#     suppressWarnings(return(pp))
#   }
#   else {
#     invisible(pp)
#   }
# }
#########################################################################
#########################################################################
# PCR_coef_path_gray <- function(x, 
#                                   legend = FALSE, 
#                                   plot = TRUE)
# {
#   M <- length(x$gamma)
#   da <- t(x$coefficients[1:M, 1:M])
#   lambda <- group <- NULL
#   col <- c(col(da))
#   #coo <-  hcl.colors(ncol(da), palette="viridis")[col] 
#   if (legend)
#   {
#     pp <-  qplot(c(row(da)), c(da),  group = col.fill, colour = col.fill, geom = "line", 
#                  ylab="beta", xlab="lambda")
#   } else
#   {
#     # dat <- data.frame(lambda=c(row(da)), beta=c(da), group=c(col(da)))
#     dat <- data.frame(lambda=c(row(da)), beta=c(da), coef=col)
#     cccc <- col/max(col+50)
#     pp <-ggplot(dat, aes(x=lambda, y=beta, group=coef))+
#       geom_line(aes(color = coef)) +
#       geom_line(col=gray(cccc)) +
#       theme(legend.position = "none") 
#   }
#   if (plot) {
#     suppressWarnings(return(pp))
#   }
#   else {
#     invisible(pp)
#   }
# }

#########################################################################
#########################################################################
