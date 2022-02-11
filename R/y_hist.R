###########################################################################
###########################################################################
# plot a histogram and density function for any variable
###########################################################################
###########################################################################
y_hist <- function(y,
                  data,
          with.density = TRUE,
              hist.col = "black", 
             hist.fill = "white",
             dens.fill = "#FF6666",
              binwidth = (max(y)-min(y))/20,
                  from, to, title)
{
###########################################################################
other_prep_data <- function (y) 
{
  obs <- seq_len(length(y))
  obs <- obs[!is.na(y)]
    y <- y[!is.na(y)]
  out <- data.frame(obs = obs, y = as.numeric(y))
return(out)
}  
########################################################################### 
  ..density.. <- NULL 
         xlab <- deparse(substitute(y))
if (missing(y))  stop("the y is not declared")
if (with.density) # withe the density
{
txt.title <- if (missing(title))   paste("Histogram and density plot of",xlab)
    else title
if (!missing(data)) y <- get(xlab, envir = as.environment(data))
           xlimitfrom <- if (missing(from))   min(y) else from
             xlimitto <- if (missing(to))     max(y) else to  
          d <- other_prep_data(y) 
          f <- d[d$color == "outlier", c("obs", "y")]
colnames(f) <- c("observation", "y")
         gg <- ggplot(d, aes(x=y))+
               geom_histogram(aes(y=..density..), binwidth = binwidth, 
                              colour=hist.col, fill=hist.fill)+
               xlim(xlimitfrom, xlimitto)+ 
               geom_density(alpha=0.2, fill=dens.fill)+
               xlab(xlab) + 
               ylab("density") + 
               ggtitle(txt.title) 
}  else # without the density
{
    txt.title <- if (missing(title))   paste("Histogram of",xlab)
    else title
if (!missing(data)) y <- get(xlab, envir = as.environment(data))
           xlimitfrom <- if (missing(from))   min(y) else from
             xlimitto <- if (missing(to))     max(y) else to   
    gg <- ggplot(data.frame(y=y), aes(x=y))+
           geom_histogram(aes(y=..density..),binwidth = binwidth, colour=hist.col, fill=hist.fill)+
           xlab(xlab) + 
           xlim(xlimitfrom, xlimitto)+      
           ylab("histogram") + 
           ggtitle(txt.title) 
}  
  return(gg)
}
#########################################################################
#########################################################################
#########################################################################
#########################################################################
  