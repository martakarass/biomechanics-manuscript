require(ggplot2)

# define theme 
theme_ggpr <- function(){ 
  font <- "Arial"  
  # theme_bw(base_size = 14) %+replace%    
  theme_bw(base_size = 14) %+replace%    
    theme(legend.background = element_rect(fill=alpha('white', 0.6), color = NA),
          panel.grid.major = element_line(size = 0.3),  
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) 
}
theme_set(theme_ggpr())

message("The file config_figures.R with ggplot2 theme was read.")