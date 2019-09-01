#' Find community-representative variables.
#'
#'
#' @description
#' Uses conditional mutual information to find the most representative variable in each community.
#'
#'
#' @param dataset A dataframe.
#' @param community A named vector contains community number for each variable of the data.
#'
#'
#' @details
#' The function uses the sum of conditional mutual information as a criterion to find the most representative variable in a community.
#' It uses \code{\link[infotheo]{entropy}} function to compute the mutual information.
#'
#'
#' @author Vahid Balazadeh, Elyas Heidari
#'
#' @examples
#' data("NHANES")
#'
#' ## Using \code{\link[muvis]{ggm}} graph with \code{weighted = TRUE}
#' gm <- ggm(data = NHANES[1:1000, ], levels = 10)
#' repres_vars <- find_repres(data = NHANES[1:1000, ], community = gm$communities)
#'
#' @return a named vector contains representative variable for each community.
#' @export
#'
#'
#' @importFrom infotheo entropy discretize
#'

find_repres <- function (dataset,
                         community,
                         method = 'emp') {
  find_rep_comm <- function(comm_var) {
    dt <- infotheo::discretize(dataset[, comm_var])
    ent <- function(x) {
      Hx <- infotheo::entropy(dt[,!comm_var %in% x], method) - infotheo::entropy(dt, method)
      Hother <- sum(sapply(comm_var, function(y) {
        if (x == y)
          return(0)
        return(infotheo::entropy(dt[,!comm_var %in% c(y)], method)
               - infotheo::entropy(dt[,!comm_var %in% c(x, y)], method))
      }))
      return((length(comm_var) - 1) * Hx + Hother)
    }
    scores <- sapply(comm_var, function(x)
      ent(x))
    return(c(comm_var[which.max(scores)], max(scores)/sum(scores)))
  }
  comms <- unique(community)
  comm_vars <-
    sapply(comms, function(x)
      names(community[community == x]))
  res <- sapply(comm_vars, find_rep_comm)
  repres <- res[1, ]
  avg_info <- res[2, ]
  names(repres) <- comms
  res <- list(repres, avg_info)
  return(res)
}
