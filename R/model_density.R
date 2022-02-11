#########################################################################
#########################################################################
#########################################################################
# this is to plot the densities of different residuals
model_density <- function(obj,..., 
                        #  hist.col = "black", 
                        #  hist.fill = "white",
                        #  dens.fill = "#FF6666",
                          title)
{
##############################################################################
##############################################################################
# local function
gamlss_prep_data <- function (obj, ... ) 
{
    rqres <- obj$residuals
    rqres <- rqres[obj$weights!=0]
      out <- data.frame(rqres = rqres, model=rep(names[[1]], length(rqres)))
    if (length(list(...)) > 0) 
    {
      i=1
      for (resp in list(...)) 
      {
        i= i+1
        res  <- resp[["residuals"]] 
        res  <- res[obj$weights!=0]
        resa <- data.frame(rqres=res, model=rep(names[[i]], length(rqres))) 
        out <- rbind(out, resa)
      }
    }
    return(out)    
}    
##############################################################################
##############################################################################
  rqres <- model <- NULL
names <- as.character(match.call()[-1])[1:(length(list(...))+1)]
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
if (length(names)<=1) stop("you need more than two models")
d <- gamlss_prep_data(obj, ...)
txt.title <- if (missing(title))   paste("Residual densities from different models")  else title
 # f <- d[d$color == "outlier", c("obs", "rqres")]
#  colnames(f) <- c("observation", "quan_resid")
 gg<-ggplot(data = d, aes(x = rqres)) + 
   geom_density(alpha = 0.3, aes(fill = model))+
   ggtitle(txt.title)
 return(gg)
}
#########################################################################
#########################################################################