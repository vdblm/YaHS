library(qgraph)
library(igraph)
graph_plot <- function(data, gm, kl, g1, g2, graph.name) {
  graph <- gm$graph
  communities <- gm$communities
  betweenness <- gm$betweenness
  find_col <- function(x) {
    if (mean(data[g1, x]) - mean(data[g2, x]) >= 0) {
      print(x)
      return('lightcoral')
    }
    else
      return('springgreen3')
  }
  cols <- apply(kl, 1, function(x) {
    return(find_col(x[2]))
  })
  colorss <- c("#F4E42B",
               "#6AB4E3",
               "#266EB4",
               "#D05700",
               "#CC73A1",
               "#1B9E6C",
               "#F3951B")
  
  cross <- function (m, graph)
  {
    el <- as_edgelist(graph, names = FALSE)
    m1 <- m[el[, 1]]
    m2 <- m[el[, 2]]
    res <- m1 != m2
    if (!is.null(names(m1))) {
      names(res) <- paste(names(m1), names(m2), sep = "|")
    }
    res
  }
  weights <- ifelse(cross(communities, graph), 1, 300)
  weights[weights > 1] <-
    weights[weights > 1] / apply(ends(gm$graph, E(gm$graph)[which(weights > 1)]), 1, function(x)
      return(sqrt(gm$betweenness[x[1]])/2 + sqrt(gm$betweenness[x[2]])/2 + 1))
  
  labls <- as.vector(kl[1:5, 'variable'])
  btwn <- betweenness[names(V(graph))]
  com <- communities[names(V(graph))]
  layout <- layout_with_fr(gm$graph, weights = weights)
  name <-
    sapply(names(V(graph)), function(x)
      if (x %in% labls)
        return (x)
      else
        "")
  # return(name)
  qgraph::qgraph(
    as_adjacency_matrix(graph),
    layout = layout,
    groups = as.factor(com),
    vsize = sqrt(btwn) / 2 + 1,
    color = colorss[1:length(groups)],
    labels = names(V(graph)),
    label.norm = 'OOOOO',
    borders = F,
    edge.color = 'lightgray',
    filetype = 'png',
    filename = graph.name
  )
}