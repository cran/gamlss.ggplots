################################################################################
################################################################################
################################################################################
# correlation for a data.frame
# it check whether thery are factor and only show the
# correlation for continuous variables
################################################################################
################################################################################
################################################################################
require(reshape2)
################################################################################
################################################################################
################################################################################
df_cor <- function(data, 
                     digits = 3,
                       plot = TRUE,
                   diag.off = TRUE,
              lower.tri.off = FALSE,  
                     method = c("square", "circle"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Corr",
                      title,
                    ggtheme = theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black", 
                     tl.srt = 45,
                        lab = TRUE, 
                    lab_col = "black", 
                   lab_size = 3) 
{
####################################################################
####################################################################
# local function 
  meltit <- function(mat)
  {
    rna <- rownames(mat)
    lrna <- length(rna)
    value <- as.vector(mat)
    Var1 <- gl(length(rna), 1, length = lrna*lrna, labels=rna)
    Var2 <- gl(length(rna), lrna, length = lrna*lrna, labels=rna)
    daf <-  na.omit(data.frame(Var1, Var2, value=value)) 
    daf
  }
#################################################################### 
####################################################################  
            dimD <- dim(data)
            daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
             Dim <- dim(daTa)
             diffDim  <- dimD[2]-Dim[2]
             if (diffDim > 0)
               {
               warning(cat(diffDim, 'factors have been omited from the data', "\n"))
              }
              CC <- cor(daTa)
              CC <- base::round(x = CC, digits = digits)
  if ( diag.off) diag(CC) <- NA
  if (lower.tri.off)  CC[lower.tri(CC)]<-NA            
       txt.title <- if (missing(title))  
          paste("Correlations from data",deparse(substitute(data)))
              else title  
if (plot==FALSE) return(CC)
          method <- match.arg(method)
            corr <- meltit(CC)
  colnames(corr) <- c("var_1", "var_2", "value")
       txt.title <- if (missing(title))  
                paste("Correlations from data",deparse(substitute(data)))
  else title  
   corr$abs_corr <- abs(corr$value) * 10
               p <- ggplot(data = corr, 
                 mapping = aes_string(x = "var_1", y = "var_2", fill = "value"))
if (method == "square") {
               p <- p + geom_tile(color = outline.color)
  }
else if (method == "circle") {
    p <- p + geom_point(color = outline.color, shape = 21, 
             aes_string(size = "abs_corr")) +
             scale_size(range = c(4, 10)) +
             guides(size = "none")
}
      label <- round(x = CC, digits = digits)               
  p <- p + scale_fill_gradient2(low = colors[1], high = colors[3], 
            mid = colors[2], midpoint = 0, limit = c(-1, 1), space = "Lab",
            name = legend.title)+ggtitle(txt.title)
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + theme(axis.text.x = element_text(angle = tl.srt, 
                vjust = 1, size = tl.cex, hjust = 1), 
                axis.text.y = element_text(size = tl.cex)) + 
                coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)  
  if (lab) {
    p <- p + ggplot2::geom_text(mapping = aes_string(x = "var_1", 
                                                     y = "var_2"), 
                  label = label, color = lab_col, size = lab_size)
  }
  p
}
###############################################################################
###############################################################################
###############################################################################