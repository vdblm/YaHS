new_vvkl <- function (data, var1, var2, permute = 0, levels = NULL, plot = F, 
                      var1.name = "var1", var2.name = "var2") 
{
  color_set <- c("#440154FF", "#3B528BFF", "#FDE725FF", "#21908CFF", 
                 "#5DC863FF")
  
  if (!is.null(levels)) 
    data <- data_preproc(data, levels = levels)
  is.cat <- function(var) {
    return(is.factor(var))
  }
  if (is.null(levels)) 
    lvl <- max(sapply(data, nlevels))
  else lvl <- levels
  lvl = 20
  kl.calc <- function(data, group1, group2) {
    to.ret <- 1:dim(data)[2] %>% purrr::map(function(x) kl.calc.vec(data[, 
                                                                         x], group1, group2))
    return(unlist(to.ret))
  }
  kl.calc.vec <- function(vec, group1, group2) {
    freqs <- list(group1 = c(), group2 = c())
    if (!is.cat(vec)) {
      rangee <- c(min(vec[group1], vec[group2]), max(vec[group1], 
                                                     vec[group2]))
      freqs$group1 <- entropy::discretize(vec[group1], 
                                          lvl, r = rangee)
      freqs$group2 <- entropy::discretize(vec[group2], 
                                          lvl, r = rangee)
      freqs$group1 <- replace(x = freqs$group1, list = which(freqs$group1 == 
                                                               0), 1)
      freqs$group2 <- replace(x = freqs$group2, list = which(freqs$group2 == 
                                                               0), 1)
    }
    else {
      na <- levels(factor(vec)) %>% purrr::map(function(x) list(group1 = max(1, 
                                                                             sum(vec[group1] == x)), group2 = max(1, sum(vec[group2] == 
                                                                                                                           x)))) %>% purrr::map(function(x) freqs <<- list(group1 = c(freqs$group1, 
                                                                                                                                                                                      x$group1), group2 = c(freqs$group2, x$group2)))
    }
    kl1 <- entropy::KL.plugin(freqs$group1, freqs$group2)
    kl2 <- entropy::KL.plugin(freqs$group2, freqs$group1)
    if (kl1 == Inf) 
      return(abs(kl2/2))
    else if (kl2 == Inf) 
      return(abs(kl1/2))
    return((abs(entropy::KL.plugin(freqs$group1, freqs$group2)) + 
              abs(entropy::KL.plugin(freqs$group2, freqs$group1)))/2)
  }
  p.val <- function(x, vec) {
    return(which(sort(vec, decreasing = T) < x)[1]/length(vec))
  }
  lm <- lm(var2 ~ var1)
  sm <- summary(lm)
  res <- residuals(lm)
  names(res) <- 1:length(res)
  frac = 0.05
  down <- head(order(res), frac * dim(data)[1])
  up <- tail(order(res), frac * dim(data)[1])
  if (plot) {
    data$kind <- 0
    data[up, 'kind'] <- 1
    data[down, 'kind'] <- -1
    g <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2)) + 
      ggplot2::geom_jitter(aes(col = factor(kind))) + ggplot2::geom_smooth(method = "lm", se = TRUE, color = "black") +
      ggplot2::labs(x = var1.name, y = var2.name, color = var1.name) +  scale_color_manual(values = color_set) + 
      theme_minimal() + ggplot2::theme(plot.title = element_blank(), text = element_text(family = 'black lato')) + guides(col = F)
    data$kind <- NULL
  }
  data <- data.frame(data)
  up.down <- c(up, down)
  kl <- kl.calc(data, up.down[1:length(up)], up.down[(length(up) + 
                                                        1):length(up.down)])
  if (permute > 0) {
    kl.df <- data.frame()
    na <- 1:permute %>% purrr::map(function(x) permute::shuffle(up.down)) %>% 
      purrr::map(function(x) list(up = x[1:length(up)], 
                                  down = x[(length(up) + 1):length(x)])) %>% purrr::map(function(f) kl.calc(data, 
                                                                                                            f[[1]], f[[2]])) %>% purrr::map(function(x) kl.df <<- rbind(kl.df, 
                                                                                                                                                                        x))
    kls <- 1:dim(kl.df)[2] %>% purrr::map(function(i) p.val(kl[i], 
                                                            kl.df[, i]))
    df <- data.frame(KL = kl, row.names = colnames(data), 
                     variable = colnames(data), p.value = unlist(kls))
    kl <- df[order(-df$KL), ]
  }
  else {
    df <- data.frame(KL = kl, variable = colnames(data), 
                     row.names = colnames(data))
    kl <- df[order(-df$KL), ]
  }
  if (plot) {
    return(list(kl = kl, plot = g))
  }
  else {
    return(kl)
  }
}
