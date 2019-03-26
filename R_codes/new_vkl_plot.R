new_vkl_plot <- function(kl, names) {
  require(ggpubr)
  names <- as.vector(names)
  color_set <- c("#440154FF", "#3B528BFF", "#FDE725FF", "#21908CFF", 
                 "#5DC863FF")
  indices <- which(names %in% cont_names)
  t <- sapply(names[indices], function(x) names(cont_names[which(cont_names == x)]))
  names <- replace(names, indices, t)
  df <- data.frame(kl, names)
    g2 <- ggdotchart(df, y = "kl", x = "names",
                     add = "segments",
                     add.params = list(color = color_set[4], size = 1.5),
                     dot.size = 2,   shape = 15, color = color_set[2],
                     rotate = T, sorting = 'des',
                     font.label = list(color = "white", size = 9, 
                                       vjust = 0.5),               
                     ggtheme = theme_minimal(),
                     ylab = 'Kullback-Leibler divergence'
    ) + geom_hline(yintercept = 0, linetype = 2, color = "lightgray")
    return(g2 + theme(axis.title.y = element_blank(), text = ggplot2::element_text(family = 'black lato')))
}


