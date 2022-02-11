#########################################################################
#########################################################################
# function 5
######################################################################### 
resid_wp <- function(obj, resid,
                     value = 3, 
                points_col = "steelblue4", 
                  poly_col = "darkred",
             check_overlap = TRUE,
                     title,
                     ylim)
{
########################################################################
# local function 
gamlss_prep_data <- function (obj, value=3) 
  {
  #  color <- NULL
    rqres <- obj$residuals
#rqres_out <- abs(rqres) > value
      obs <- seq_len(length(rqres))
  #outlier <- rqres[rqres_out]
      obs <- obs[obj$weights!=0]
    rqres <- rqres[obj$weights!=0]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      # s <- splinefun(x, fitted(fit))
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))
out$color <- ifelse((abs(rqres) >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}
#####################################################################
other_prep_data <- function (resid, value=2) 
{
   # color <- NULL
    rqres <- resid
#rqres_out <- abs(rqres) > value
      obs <- seq_len(length(rqres))
#  outlier <- rqres[rqres_out]
      obs <- obs[!is.na(resid)]
    rqres <- rqres[!is.na(resid)]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))    
out$color <- ifelse((abs(rqres) >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                         "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}    
###########################################################################
getSE <- function(xlim, level=0.95)
{
      lz <- -xlim
      hz <- xlim 
      dz <- 0.25
       z <- seq(lz,hz,dz)
       p <- pnorm(z)   
      se <- (1/dnorm(z))*(sqrt(p*(1-p)/N))    
     low <- qnorm((1-level)/2)*se
    high <- -low
data.frame(high=high, low=low, z=z)
}
#######################################################################    
if (missing(obj)&&missing(resid))   stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
        N <- if (missing(obj)) length(resid) else obj$noObs
        d <- if (missing(obj)) other_prep_data(resid, value=value) 
             else               gamlss_prep_data(obj, value=value)
        x <- rqres <- z <- low <- high <- txt <- NULL
txt.title <- if (missing(title))  paste("Worm-plot for model", deparse(substitute(obj)))
             else title  
       se <- getSE(max(abs(d$x))+.5)
     ymax <- if (missing(ylim))  max(abs(d$rqres))+0.1 else ylim
       gg <- ggplot() + 
          geom_ribbon(data=se, aes(ymin = low, ymax = high, x = z), alpha = 0.1)+
          geom_point(data = d, aes(x = x, y = rqres),  color =  points_col, alpha=.8 ) + # shape = 1, must include argument label "data"
          geom_line(data = d, aes(x = x, y = fv), lty=1, colour=poly_col)+
         # geom_polygon(data = se, aes(x = z, y = low, alpha=0.1), 
         #               show.legend=FALSE, fill="lightgray")+
          geom_line(data = se, aes(x = z, y = low), lty=2)+
          #geom_polygon(data = se, aes(x = z, y = high, alpha=0.1), 
          #            show.legend=FALSE, fill="lightgray")+
          geom_line(data = se, aes(x = z, y = high), lty=2)+
          xlab("Unit normal quantile") + 
          ylab("Deviation") +
          coord_cartesian(ylim = c(-ymax, ymax)) +
          geom_hline(yintercept = 0, colour = "gray")+
          geom_vline(xintercept = 0, colour = "gray")+
          geom_text(data = d, aes(x = x, y = rqres, label = txt),
              hjust = -0.2, nudge_x = 0.05, size = 3,
              check_overlap = check_overlap, family = "serif", 
              fontface = "italic", colour = "darkred", na.rm = TRUE)+
          ggtitle(txt.title)
  suppressWarnings(return(gg))
}
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#  resid_wp1 <- function(object, resid,
#                    value = 2,
#                    overlap = 0,
#                    xlim.all = 4,
#                    xlim.worm = 3.5,
#                    show.given = TRUE,
#                    line = TRUE,
#                    ylim.all = 12*sqrt(1/length(resid)),
#                    ylim.worm = 12*sqrt(n.inter/length(resid)),
#                    cex = 1,
#                    cex.lab = 1,
#                    pch = 21,
#                    bg = "wheat",
#                    col = "red",
#                    bar.bg = c(num="light blue"),
#                    ...)
# {
######################################################################
#######################################################################  
#    ## get residuals
# if (missing(object)&&missing(resid))  stop(paste("A fitted object with resid() method or the argument resid should be used ", "\n", ""))
#   resid <- if (!missing(object)) residuals(object)  else resid
#   DataExist <- FALSE
#   if (!missing(object)&&any(grepl("data", names(object$call)))&&!(object$call["data"]=="sys.parent()()"))
#   {
#     DaTa <-  eval(object$call[["data"]]) #get(as.character(object$call["data"]))
#     DataExist <- TRUE
#   }
#       qq <- as.data.frame(qqnorm(resid, plot = FALSE))
#     qq$y <- qq$y - qq$x
#    level <- .95
#       lz <- -xlim.all
#       hz <- xlim.all
#       dz <- 0.25
#        z <- seq(lz,hz,dz)
#        p <- pnorm(z)
#       se <- (1/dnorm(z))*(sqrt(p*(1-p)/length(qq$y)))
#      low <- qnorm((1-level)/2)*se
#     high <- -low
#   if ( any(abs(qq$y)>ylim.all) )
#   {
#     warning("Some points are missed out ", "\n","increase the y limits using ylim.all" )
#   }
#   if ( any(abs(qq$x)>xlim.all) )
#   {
#     warning("Some points are missed out ", "\n","increase the x limits using xlim.all" )
#   }
#   plot(qq$x,qq$y,ylab="Deviation", xlab="Unit normal quantile",
#        xlim=c(-xlim.all,xlim.all), ylim=c(-ylim.all , ylim.all), cex=cex, pch=pch , bg = bg,cex.lab=cex.lab )
#   grid(lty = "solid")
#   abline(0, 0, lty = 2, col = col)
#   abline(0, 100000, lty = 2, col = col)
#   lines(z, low, lty=2)
#   lines(z, high, lty=2)
#   if(line == TRUE)
#   {
#     fit <- lm(qq$y ~ qq$x+I(qq$x^2)+I(qq$x^3)) #poly(qq$x,3))
#     s <- spline(qq$x, fitted(fit))
#     flags <- s$x > -xlim.all & s$x < xlim.all
#     lines(list(x = s$x[flags], y = s$y[flags]), col=col, lwd=.5)
#   }
#   return(list(y=qq$y, x=qq$x, z=z, low=low, high=high, coef=coef(fit)))
# }

#########################################################################
#########################################################################
########################################################################
# local function 
prep_data_for_wp <- function (obj, value=2) 
{
    color <- NULL
    rqres <- obj$residuals
rqres_out <- abs(rqres) > value
      obs <- seq_len(length(rqres))
  outlier <- rqres[rqres_out]
      obs <- obs[obj$weights!=0]
    rqres <- rqres[obj$weights!=0]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))
out$color <- ifelse((abs(out$rqres) >= value), 
                      c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  return(out)
}
#####################################################################

# END
#########################################################################
######################################################################### 
######################################################################### 
######################################################################### 
