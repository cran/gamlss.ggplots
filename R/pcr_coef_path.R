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
#########################################################################
# the path of beta coefficients for different smoothing parameters 
# for a PCR object
pcr_coef_path <- function(x, legend=FALSE, plot=TRUE)
{
  if (!inherits(x,"PCR")) stop("the object should be a PCR class")
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
# the path of beta coefficients for different smoothing parameters 
# for a gamlss model in which PCR is fitted  
pcr_path <- function(x, parameter=c("mu", "sigma", "nu", "tau"),
                     legend=FALSE, plot=TRUE)
{
if (!is(x,"gamlss")) stop("the object should be a  gamlss class")
  parameter <- match.arg(parameter)
         xx <- getSmo(x, parameter=parameter)
if (!is(xx,"PCR")) stop("not PCR object detected") 
        pcr_coef_path(xx, legend=legend, plot=plot)+
          geom_vline(xintercept = xx$pc, colour = "gray")
}  
#########################################################################
#########################################################################
#########################################################################
path.plot <- function(x,  parameter=c("mu", "sigma", "nu", "tau"),
                     xvar = c("norm", "dev", "lambda"))
{
  if (!is(x,"gamlss")) stop("the object should be a  gamlss class")
       parameter <- match.arg(parameter)
            xvar <- match.arg(xvar)
              xx <- getSmo(x, parameter=parameter)
  if (!is(xx[[1]],"glmnet")) stop("not glmnet object detected")            
             val <- xx[[2]]$optid
             dev <- xx[[1]]$dev.ratio
            norm <- colSums(abs(xx[[1]]$beta))
          lambda <- xx[[1]]$lambda 
             plot(xx[[1]], xvar=xvar)
 switch(xvar,
        "norm"=abline(v=norm[val], col="grey"),
        "dev"=abline(v=dev[val], col="grey"),
        "lambda"=abline(v=lambda[val], col="grey"))            
}                     
#########################################################################
#########################################################################
#########################################################################
# gnet_path <- function(x, parameter=c("mu", "sigma", "nu", "tau"),
#                    xvar = c("norm", "dev", "lambda"),
#                             plot=TRUE)
# {
#   browser()
#   if (!is(x,"gamlss")) stop("the object should be a  gamlss class")
#   parameter <- match.arg(parameter)
#       xvar <- match.arg(xvar)
#          xx <- getSmo(x, parameter)
#   if (is.null(xx)) stop("there is no smoothet for", parameter, "\n")
#         val <- xx[[2]]$optid
#         dev <- xx[[1]]$dev.ratio*100
#          df <- xx[[1]]$df
#          browser()
#        norm <- colSums(abs(xx[[1]]$beta))
#                #  apply(abs(xx[[1]]$beta),2, sum)
#      lambda <- xx[[1]]$lambda
#           M <- length(dev)
#          da <- as.matrix(xx[[1]]$beta)
#         # colSums(abs(da))
#       group <- NULL
#         col <- c(row(da))
#   switch (xvar, 
#         "norm"={
#           browser()
#           dat <- data.frame(norm=c(norm[col]), beta=c(da), group=c(row(da)))
#           names(dat)
#           pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, col=group)) + 
#             geom_line()+
#             theme(legend.position = "none")
#         },
#         "dev" ={dat <- data.frame(lambda=c(col(da)), beta=c(da), group=c(row(da)))
#         pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, col=group)) + 
#           geom_line()+
#           theme(legend.position = "none")},
#         "lambda"={dat <- data.frame(lambda=c(col(da)), beta=c(da), group=c(row(da)))
#         pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, col=group)) + 
#           geom_line()+
#           theme(legend.position = "none")})
#         
#   # if (legend)
#   # {
#   #   pp <-  qplot(c(col(da)), c(da),  group = col, colour = col, geom = "line",
#   #                ylab="beta", xlab="lambda")
#   # } else
#   # {
#   #   dat <- data.frame(lambda=c(col(da)), beta=c(da), group=c(row(da)))
#   #   pp <- ggplot(dat, aes(x=lambda, y=beta, group=group, col=group)) + 
#   #     geom_line()+
#   #     theme(legend.position = "none")
#   # }
#   if (plot) print(pp)
#   invisible(pp)
# }
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
