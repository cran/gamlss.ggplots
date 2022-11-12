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
if (with.density) # with the density
{
txt.title <- if (missing(title))   paste("Histogram and density plot of",xlab)
    else title
if (!missing(data)) y <- get(xlab, envir = as.environment(data))
           xlimitfrom <- if (missing(from))   min(y) else from
             xlimitto <- if (missing(to))     max(y) else to  
          d <- other_prep_data(y) 
         # f <- d[d$color == "outlier", c("obs", "y")]
#colnames(f) <- c("observation", "y")
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
y_dots <- function(y,
                   data,
                point.size = 2,
                 point.col = "gray",
                  line.col = c("red","red"),
                 line.type = c("solid", "dotted"),
                 line.size = c(1,1),
                x.axis.col = "black",
          x.axis.line.type = "solid",
                      seed = 123,
                   from, to, title)
{
#########################################################################
other_prep_data <- function (y, seed) 
  {
     obs <- seq_len(length(y))
     obs <- obs[!is.na(y)]
       y <- y[!is.na(y)]
      ly <- length(y)
      set.seed(seed)
    rand <- runif(ly)
     out <- data.frame(obs = obs, rand=rand, y = as.numeric(y))
  return(out)
  }  
#########################################################################   
  rand <- NULL 
  xlab <- deparse(substitute(y))
  if (missing(y))  stop("the y is not declared")
    txt.title <- if (missing(title))   paste("Plot of",xlab)
    else title
    if (!missing(data)) y <- get(xlab, envir = as.environment(data))
    xlimitfrom <- if (missing(from))   min(y) else from
    xlimitto <- if (missing(to))     max(y) else to  
     d <- other_prep_data(y, seed=seed) 
    gg <- ggplot(d, aes(y=rand, x=y))+
                 geom_point(size=point.size, col=point.col)+
      geom_vline(xintercept = quantile(y, .50), col=line.col[1], size=line.size[1], linetype=line.type[1])+
      geom_vline(xintercept = quantile(y, .90), col=line.col[2], size=line.size[2], linetype=line.type[2])+
      ggtitle(txt.title) +
      xlim(xlimitfrom, xlimitto)+
      xlab(xlab) + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.border = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             axis.line.x = element_line(size = 0.5, 
                  linetype = x.axis.line.type, colour = x.axis.col),
                    panel.background = element_blank())
  return(gg)
}
##########################################################################
##########################################################################
##########################################################################
##########################################################################


