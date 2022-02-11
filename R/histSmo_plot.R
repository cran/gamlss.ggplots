# this function is plotting in qqplot2
# density estimates for hisSmo() 
histSmo_plot <- function(x, 
             col_fill_bar = gray(.5), 
                  col_bar = "pink",
                 col_line = "darkblue",
               width_line = 1,
                 title, xlabel)
{
  if (!is(x,"histSmo")) stop("this function is designted for histSmo objects" )
  discrete <- x$discrete
  if (discrete) {
    #switch(type, hist = {
      freq <- x$counts/(sum(x$counts))
      xvar <- x$x
      dens <- x$density
      xname <- x$hist[["xname"]]
      #sum(density*(breaks[10]-breaks[9]))
      txt.title <- if (missing(title))  
        paste("smooth histogram from ",deparse(substitute(x)) )
      else title
      txt.xlabel <- if (missing(xlabel)) ""  
      else xlabel
        da <- data.frame(freq=freq, xvar=xvar, dens=dens)
  gg <- ggplot(data=da, aes(x=xvar, y=freq))+
        geom_col( col= col_bar, fill=col_fill_bar)+
        geom_line(aes(x=xvar, y=dens), col=col_line, lwd=width_line)+
        #geom_point(aes(x=xvar, y=dens))+
        ylab("Frequency")+ xlab(txt.xlabel)+ggtitle(txt.title)
  #  }, cdf = {
    #  plot(x$cdf, ylab = "cdf")
   # }, invcdf = {
   #   plot(x$invcdf, ylab = "invcdf", xlab = "p")
   # })
  }
  else {
    #switch(type, hist = {
      #breaks <- x$hist[["breaks"]]
      counts <- x$hist[["counts"]]
        freq <- counts/sum(counts)
      density <- x$hist[["density"]]
        mids <- x$hist[["mids"]]
       xname <- x$hist[["xname"]]
  #sum(density*(breaks[10]-breaks[9]))
       txt.title <- if (missing(title))  
         paste("smooth histogram of",xname)
       else title
  #{plot(x$hist, freq=FALSE); lines(x$x, x$density, col = 'red', lty = 1, lwd = 3)}
          da <- data.frame( freq=density, mids=mids, x=x$x, density=x$density )
       #   da <- data.frame(breaks=breaks, counts=counts, dens=density, mids=mids)
      gg <- ggplot(data=da, aes(x=mids, y=freq))+
        geom_col( col= col_bar, fill=col_fill_bar)+
        geom_line(aes(x=x, y=density), col=col_line, lwd=width_line)+
        ylab("density")+ xlab(xname)+ggtitle(txt.title)
      
     # plot(x$hist, freq = FALSE)
     # lines(x$x, x$density, col = "red", lty = 1, lwd = 3)
   # }, cdf = {
   #   plot(x$cdf, ylab = "cdf")
   # }, invcdf = {
   #   plot(x$invcdf, 0, 1, ylab = "invcdf", xlab = "p")
   # })
  }
gg  
}
