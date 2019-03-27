#' @references  Abreu, G. C. G., Edwards, D., & Labouriau, R. (2010). High-Dimensional Graphical Model Search with thegRapHDRPackage. Journal of Statistical Software, 37(1). \url{https://doi.org/10.18637/jss.v037.i01}
#'
#'
#' @author Elyas Heidari
#'
#' @section Additional arguments:
#' \describe{
#' \item{threshold}{A threshold for partial correlation thresholding method (default = 0.05). To be used only when the method "threshold" is used.}
#' \item{significance}{A cutoff for edge significance (default = 0.05). To be used only when the method "significance" is used.}
#' \item{rho}{(Non-negative) regularization parameter for glasso (default = 0.1). To be used only when the method "glasso" is used.}
#' }
#'
#' @return A list in which each element is the details of a specific fitting method.
#' \item{significance}{A data.frame containing edges with p.values.}
#' \item{graph}{an igraph object of the graphical model.}
#' \item{betweenness}{betweenness measurements of each node.}
#' \item{network}{a visNetwork plot of the graph.}
#' \item{communities}{a named vector indicating the community of each node.}
#'
#' @export
#'
#' @examples
#' data("NHANES")
#' ## Using raw data
#' ## No need to choose the continuous variables (They will be detected automatically)
#' glasso_ggm <- ggm(data = NHANES[1:1000, ], methods = c("glasso"), levels = 15)
#'
#' ## Using preprocessed data
#' data <- data_preproc(NHANES, levels = 15)
#' data$SEQN <- NULL
#' glasso_sin_ggm <- ggm(data = data[1:1000, 1:74], methods = c("glasso", "sin"),
#' plot = TRUE, rho = 0.2, significance = 0.03)
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  gRbase cov2pcor stepwise
#' @importFrom  purrr map
#' @importFrom  gRim cmod
#' @importFrom  graph graphNEL
#' @importFrom  SIN sinUG getgraph
#' @importFrom  glasso glasso
#' @importFrom  igraph cluster_louvain betweenness membership V intersection
#' @importFrom  stats C cov.wt
#' @importFrom  methods as
#' @importFrom  graph nodes
#' @importFrom  dplyr %>%




ggm <-
  function(data,
           methods = c("glasso"),
           community = TRUE,
           betweenness = TRUE,
           plot = FALSE,
           levels = NULL,
           ...) {
    if (!is.null(levels))
      data <- data_preproc(data, levels = levels)
    
    is.cat <- function(var) {
      return(is.factor(var))
    }
    
    data <- data[, sapply(data, function(x)
      ! is.cat(x))]
    
    arguments <- list(...)
    threshold <- arguments$threshold
    significance <- arguments$significance
    rho <- arguments$rho
    
    if (is.null(threshold))
      threshold <- 0.05
    
    if (is.null(significance))
      significance <- 0.05
    
    if (is.null(rho))
      rho = 0.1
    
    model <- gRim::cmod(~ . ^ ., data = data)
    S <- stats::cov.wt (data, method = "ML")$cov
    PC <- gRbase::cov2pcor(S)
    othermodels <- list()
    if ("aic" %in% tolower(methods)) {
      othermodels$aic <- aic <- gRbase::stepwise(model)
    }
    if ("bic" %in% tolower(methods)) {
      othermodels$bic <- gRbase::stepwise(model, k = log(nrow(data)))
    }
    if ("test" %in% tolower(methods)) {
      othermodels$test <- gRbase::stepwise(model, criterion = "test")
    }
    if ("threshold" %in% tolower(methods)) {
      Z <- abs(PC)
      Z[Z < threshold] <- 0
      diag(Z) <- 0
      Z[Z > 0] <- 1
      g.thresh <-  methods::as(Z, "graphNEL")
      othermodels$thresh <- gRim::cmod(g.thresh, data = data)
    }
    if ("sin" %in% tolower(methods)) {
      psin <- SIN::sinUG(S, n = nrow(data))
      othermodels$gsin <-
        methods::as(SIN::getgraph(psin, significance), "graphNEL")
    }
    if ("glasso" %in% tolower(methods)) {
      C <- stats::cov2cor(S)
      res.lasso <- glasso::glasso(C, rho = rho)
      AM <- abs(res.lasso$wi) > threshold
      diag(AM) <- F
      g.lasso <- methods::as(AM, "graphNEL")
      graph::nodes(g.lasso) <- colnames(data)
      othermodels$glasso <- g.lasso
    }
    othermodels <- othermodels %>% purrr::map(methods::as, "igraph")
    commonedges <- do.call(igraph::intersection, othermodels)
    val <- graph_vis(
      commonedges,
      community = community,
      plot = plot,
      betweenness = T,
      directed = F
    )
    
    vnet <- val$network
    edges <- vnet$x$edges
    
    test_matrix <-
      test_assoc(data, vnet$x$nodes$id, levels = levels)
    e_names <- rbind(edges$from, edges$to)
    p_values <-
      apply(e_names, 2, function(x)
        test_matrix[x[1], x[2]])
    title = paste0("<p>", paste("p.value =", p_values), "</p>")
    edges[, "title"] <- title
    edges[, "weight"] <- p_values
    
    significance <- data.frame(edges$from, edges$to)
    significance$p.value <- p_values
    
    vn <-
      visNetwork::visNetwork(vnet$x$nodes, edges, height = "500px", width = "100%")  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      ))
    if (community) {
      list(
        significance = significance,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn,
        communities = val$communities
      )
    } else {
      list(
        significance = significance,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn
      )
    }
  }
