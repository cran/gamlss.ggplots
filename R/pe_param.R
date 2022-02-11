#################################################################
#################################################################
# created on the 08-06-2021
# to Do
# i) what is data set incules character vectors no declared as factors
#################################################################
#################################################################
pe_param <- function(obj = NULL, # the gamlss object
                    term = NULL, # which terms to get the derivative
                    data = NULL, # which data is needed here
                n.points = 100,  # number of points needed for evaluating the function
               parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                    type = c("parameter", "eta"),
                     how = c("median", "last", "fixed"),
                scenario = list(), # see below (1)
                    plot = TRUE,
                     col = "darkblue",
                    size = 1.3,
                    bins = 30, # for contour plot
                  filled = FALSE, #for contour plot
                    title) # whether to plot
{
     lterm <- length(term)
  name.obj <-  deparse(substitute(obj))
if (lterm==1) {gg <- pe_1param(obj=obj, term = term, data = data, n.points = n.points, 
                               parameter = parameter, type = type, how = how,
                               scenario = scenario,  col = col,
                               size = size, name.obj = name.obj, 
                               title = title)}
                               
else if (lterm==2) {
      gg <- pe_2param(obj = obj, terms = term, data=data, n.points = n.points, 
                     parameter = parameter, type = type, how = how,
                     scenario = scenario,
                     size = size, bins = bins,
                     filled = filled,
                     name.obj = name.obj, 
                     title = title)}
    else stop("only up to two way interactions can be plotted")
  gg  
}
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
# created on the 08-06-2021
# to Do
# i) what is data set inculeds character vectors no declared as factors
#################################################################
#################################################################
pe_1param <- function(obj = NULL, # the gamlss object
                     term = NULL, # which term to get the derivative
                     data = NULL, # which data is needed here
                 n.points = 100,  # number of points needed for evaluating the function
                parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                     type = c("parameter", "eta"),
                      how = c("median", "last", "fixed"),
                 scenario = list(), # see below (1)
               #      plot = TRUE,
                      col = "darkblue",
                     size = 1.3,
                 name.obj = NULL,
                     title) # whether to plot
{
#  scenario:a named list of the values to use for the other predictor terms. 
#  Variables omitted from this list will have values set to the median 
#  for continuous variables and 
#  the most commonly occuring level for factors 
#  or the last observation
if (is.null(obj)||!class(obj)[1]=="gamlss") stop("Supply a standard GAMLSS model in obj")
if (is.null(term))  stop("The model term is not set")
        x <-  y <- NULL
      how <- match.arg(how)
     type <- match.arg(type)
     type <- if (type=="parameter") "response" else "link"
parameter <- match.arg(parameter)
if (any(grepl("data", names(obj$call)))) 
  {
    DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
      eval(parse(text=as.character(obj$call["data"]))) else 
        get(as.character(obj$call["data"]))	
  }
else if (is.null(data)) stop("The data argument is needed in obj")
  v.names <- names(DaTa)
      pos <- which(v.names==term)
if (pos<1) stop("supply a  term")
if (is.factor(DaTa[,pos])) 
  {
     xvar <- levels(DaTa[,pos])
 n.points <- nlevels(DaTa[,pos])
it.is.factor <- TRUE
  } else
  {
    xvar <-  seq(from = min(DaTa[,pos]), to=max(DaTa[,pos]), length.out=n.points)
    it.is.factor <- FALSE
  }                
     mat <- matrix(0, nrow = dim(DaTa)[1]+n.points, ncol =dim(DaTa)[2])
dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names             
## creating new data         
for (i in 1:dim(dat.temp)[2])
  {
    if(pos==i)                      # if the variable of interest
    {                               # new data for x is
      dat.temp[,i] <- c(DaTa[,i],xvar) # min(x) to max(x)
    }
    else                            # for all other variables
    {                               # if scenario is set gets priority
      ma <- scenario[[v.names[i]]]
      if (is.null(ma))                  # if scenario in not set
      {
        if (how=="median")          # get the median for continuous 
          # or the level with high values for factor
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else median(DaTa[,i])
        }
        if (how=="last")       # otherwise get the last values
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else tail(DaTa[,i],1)
        }
      }
      dat.temp[,i] <- c(DaTa[,i],rep(ma,n.points))
    }
} # end going thought the variables
## predict           
fittted.orig <- predict(obj, newdata=tail(dat.temp, n.points), type = type, parameter = parameter)
      theFun <- splinefun(xvar, fittted.orig)
    name.obj <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
   txt.title <- if (missing(title))  
      paste("Partial effect of",term, "for", parameter, "for model", name.obj)
    else title
    yaxislabel <- if (type=="response") paste0("PE_param(", term, ")")
    else                paste0("PE_eta](", term, ")")
    da <- data.frame(y=fittted.orig, x=xvar)
    
    if (it.is.factor)
    {
      pp <-  ggplot(data=da, aes(x=x, y=y))+
        geom_point(color=col, size=size+2)+
        ylab(yaxislabel)+ xlab(term)+ ggtitle(txt.title)
      
    } else 
    {
      pp <- ggplot(data=da)+
        geom_line( aes(x=x, y=y), color=col, size=size)+
        ylab(yaxislabel)+ xlab(term)+ ggtitle(txt.title)
    }          
    return(pp)
  # } else
  # {
  #   if ((it.is.factor)) stop("there is no function saved for factors")
  #   else invisible(theFun)
  # }  
}

#################################################################
#################################################################
#################################################################
#################################################################
pe_2param <- function(obj = NULL, # the gamlss object
                    terms = NULL, # which terms to get the derivative
                     data = NULL, # which data is needed here
                 n.points = 100,  # number of points needed for evaluating the function
                parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                     type = c("parameter", "eta"),
                      how = c("median", "last", "fixed"),
                 scenario = list(), # see below (1)
                     size = 1.3,
                     bins = 30, # for contour plot
                   filled = FALSE, #for contour plot
                 name.obj = NULL,
                   title) # whether to plot
{
#  scenario:a named list of the values to use for the other predictor terms. 
#  Variables omitted from this list will have values set to the median 
#  for continuous variables and the most commonly occuring level for factors 
#  or the last observation
  if (is.null(obj)||!class(obj)[1]=="gamlss") stop("Supply a standard GAMLSS model in obj")
  if (is.null(terms))  stop("The model terms are not set")
              x <- y <- NULL
            how <- match.arg(how)
           type <- match.arg(type)
           type <- if (type=="parameter") "response" else "link"
      parameter <- match.arg(parameter)
if (any(grepl("data", names(obj$call)))) 
      {
        DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
          eval(parse(text=as.character(obj$call["data"]))) else 
            get(as.character(obj$call["data"]))	
      }
      else if (is.null(data)) stop("The data argument is needed in obj")
        v.names <- names(DaTa)
            pos <- match(terms, v.names)
  lpos <- length(pos)                    
if (lpos<=1) stop("supply 2 terms")
if (lpos>3) stop("only up to two terms are allowed")   
    WhichFactor <- sapply(DaTa[,pos], is.factor)
if (any(WhichFactor)) # if any is factor 
  {
         pf <- which(WhichFactor)
         pv <- which(!WhichFactor)
if (length(pf)==2)
     {
       fac1 <- levels(DaTa[,pos[1]])
n.points.f1 <- nlevels(DaTa[,pos[1]])
       fac2 <- levels(DaTa[,pos[2]])
n.points.f2 <- nlevels(DaTa[,pos[2]])
         d1 <- expand.grid(f1 = fac1, f2 = fac2)
  names(d1) <- terms
        mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1],ncol =dim(DaTa)[2]) 
       case <- 3 # both factors
     } else
     {
        fac <- levels(DaTa[,pos[pf]])
 n.points.f <- nlevels(DaTa[,pos[pf]])
        var <- seq(min(DaTa[,pos[pv]]), max(DaTa[,pos[pv]]), length.out=n.points)
         d1 <- expand.grid(x = var, f = fac)
  names(d1) <- terms
        mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1], ncol =dim(DaTa)[2])  
       case <- 2 # one factor one cont
     } 
  } else
  {
          x <- seq(min(DaTa[,pos[1]]), max(DaTa[,pos[1]]), length.out=n.points)
          y <- seq(min(DaTa[,pos[2]]), max(DaTa[,pos[2]]), length.out=n.points)
         d1 <- expand.grid(x = x, y = y)
  names(d1) <- terms
        mat <- matrix(0, nrow = dim(DaTa)[1]+n.points^2, ncol =dim(DaTa)[2])
       case <- 1
  }    
   dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names             
  ## creating new data         
for (i in 1:dim(dat.temp)[2])
  {
    if(pos[1]==i)                      # if the variable of interest
    {                               # new data for x is
   dat.temp[,i] <- c(DaTa[,i],d1[,1]) # min(x) to max(x)
    } else
      if(pos[2]==i)                      # if the variable of interest
      {                               # new data for x is
   dat.temp[,i] <- c(DaTa[,i],d1[,2]) # min(x) to max(x)
      }
    else                            # for all other variables
    {                               # if scenario is set gets priority
             ma <- scenario[[v.names[i]]]
  if (is.null(ma))                  # if scenario in not set
      {
        if (how=="median")          # get the median for continuous 
                                    # or the level with high values for factor
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
                else median(DaTa[,i])
        }
        if (how=="last")       # otherwise get the last values
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
                else tail(DaTa[,i],1)
        }
}
  dat.temp[,i] <-   c(DaTa[,i],rep(ma,dim(d1)[1]))       

    }
} # end going thought the variables
## predict     
fittted.orig <- predict(obj, newdata=tail(dat.temp, dim(d1)[1]), type = type, parameter = parameter)
   name.obj  <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
   txt.title <- if (missing(title))  
      paste("Partial effect of", terms[1], "and", terms[2], "for", parameter, "for model", name.obj)
             else title
         da <- data.frame(z=fittted.orig, d1)
if (case==1)
    {
      pp  <-  ggplot(da, aes_string(terms[1], terms[2]))
       if (filled)  pp<- pp+geom_contour_filled(aes(z = da[,1]), bins=bins/3 )
       else         pp <- pp+geom_contour(aes(z = da[,1]), bins=bins, size=size)#+
         pp<- pp+ ggtitle(txt.title)
    } 
  if (case==2)
    {
     pp <- ggplot(da, aes_string(x=terms[pv], y=da[,1], color=terms[pf]))+
           geom_line(size=size)+
         ggtitle(txt.title)
    }  
  if (case==3)
  {
   # ggplot(data=da, aes(x=x, y=y, group=factor(quantiles), colour=quantiles))+
  #    geom_line()+
    #  geom_point( size=size)+
    #  ylab(yaxislabel) + xlab(term)+ ggtitle(txt.title)
    pp <- ggplot(data=da, aes_string(x=terms[1], y=da[,1], group=terms[2], color=terms[2]))+
          geom_line()+
          geom_point(size=size+2)+
          ggtitle(txt.title)
    
  }
      return(pp)

}
#####################################################################
#####################################################################
#####################################################################
#####################################################################
require(grid)
pe_param_grid <- function(model, terms, maxcol=2, maxrow=3, ...)
{  
################################################################# 
define_region <- function(row, col){
viewport(layout.pos.row=row, layout.pos.col=col) }
#################################################################  
# function starts
  lterms <- length(terms)
if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
#if (lterms  <= 1) stop("only one term use pe_param()")
     norow <- ceiling(lterms/maxcol)
     nocol <- if (norow == 1)  lterms  else  maxcol    
        IJ <- expand.grid(j=1:nocol, i=1:norow)
grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=norow,ncol=nocol)))   
  GG <- list()
  # for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
for (p  in 1:lterms) 
  {
  title.term <- if (length(terms[[p]])==1) terms[[p]]
                else paste(terms[[p]], collapse=":")  
    GG[[title.term]] <- pe_param(model, term=terms[[p]], 
                                    title= title.term,...)
    print(GG[[title.term]], vp=define_region(IJ$i[p], IJ$j[p]))
  }
}
#######################################################################
#######################################################################
#######################################################################
#######################################################################

