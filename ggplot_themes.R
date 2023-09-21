theme_quals <- function(){ 
  font <- "Calibri"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    theme(text = element_text(size = 18))
}

theme_abs_2023 <- function(){
  theme_classic() %+replace%
    theme(panel.background = element_rect(fill = "#FFFCF6"),
          plot.background = element_rect(fill = "#FFFCF6"),
          text = element_text(color = "#7A695A", size = 18),
          axis.text = element_text(color = "#7A695A"),
          axis.ticks = element_line(color = "#7A695A"),
          axis.line = element_line(color = "#7A695A"))
}