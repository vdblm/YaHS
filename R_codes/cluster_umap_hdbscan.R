cluster_umap_hdbscan <- function(data, dim, min_pt, n_neighbour, groups ) {
  embeddings_clustrable <- umap::umap(data, verbose = TRUE, random_state=42,  n_neighbors = n_neighbour, n_components = dim, min_dist = 0, metric = 'cosine', method = 'umap-learn')
  embeddings <- data.frame(umap::umap(data, verbose = TRUE,  n_neighbors = n_neighbour, metric = 'correlation', method = 'umap-learn', random_state = 42)$layout)
  res <- dbscan::hdbscan(embeddings_clustrable$layout, minPts = min_pt)
  
  if (is.null(groups)){
    groups = as.factor(res$cluster)
  }
  g <- ggplot2::ggplot(data = embeddings, ggplot2::aes(
    x = embeddings[, 1],
    y = embeddings[, 2], col = groups
  )) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste("UMAP of the Population Under Study")) + scale_color_manual(
      name = 'Cluster',
      labels = c('noise', 'cluster 1', 'cluster 2'),
      values = c('gray', 'lightcoral', 'springgreen3')
    ) + theme_Publication()  +ggplot2::labs(x = 'Component 1', y = 'Component 2') + theme(axis.text = element_text(size = rel(1.2))) + theme(
      legend.title = element_text(size = rel(1.2), face = 'bold'),
      legend.text = element_text(size = rel(1.2)),
      plot.title = element_text(size = rel(1.2), face = 'bold'),
      axis.title = element_text(size = rel(1.2))
    )
  
  return(list(res = res, plot = g))
}


theme_Publication <- function(base_size = 14) {
  (
    ggthemes::theme_foundation(base_size = base_size)
    + ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(.5),
        hjust = 0.5
      ),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.key.size = grid::unit(0.2, "cm"),
      legend.spacing = grid::unit(0, "cm"),
      plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
      strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  )
  
}

scale_fill_Publication <- function(...) {
  ggplot2::discrete_scale("fill", "Publication", scales::manual_pal(
    values = c(
      "#386cb0",
      "#fdb462",
      "#7fc97f",
      "#ef3b2c",
      "#662506",
      "#a6cee3",
      "#fb9a99",
      "#984ea3",
      "#ffff33"
    )
  ), ...)
  
}

scale_colour_Publication <- function(...) {
  discrete_scale("colour",
                 "Publication",
                 scales::manual_pal(
                   values = c(
                     "#386cb0",
                     "#fdb462",
                     "#7fc97f",
                     "#ef3b2c",
                     "#662506",
                     "#a6cee3",
                     "#fb9a99",
                     "#984ea3",
                     "#ffff33"
                   )
                 ),
                 ...)
}