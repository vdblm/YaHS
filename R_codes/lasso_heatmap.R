lasso_heatmap <- function(coeffs, var, data, smp_size, intercept =NULL) {
  require(leaflet)
  require(glmnetUtils)
  colorss <- c("white",
               "#6AB4E3",
               "#266EB4",
               "#D05700",
               "#CC73A1",
               "#1B9E6C",
               "#F3951B")
  dot_chart <- function(coeffs, min, max) {
    g2 <- ggdotchart(coeffs, x = "name", y = "coef",
                     add = "segments",
                     add.params = list(color = "black", size = 1),
                     dot.size = 2,   
                     rotate = T,
                     font.label = list(color = "white", size = 9, 
                                       vjust = 0.5),               
                     ggtheme = theme_minimal(),#+ ylim(min, max),
                     ylab = 'Model Coefficients'
    ) + geom_hline(yintercept = 0, linetype = 2, color = "lightgray") + theme(panel.grid.minor = element_blank()) 
    return(g2)
  }
  
  scale_cont_data <- function(data) {
    data <- as.data.frame(data)
    is.fact <- sapply(data, is.factor)
    data[, !is.fact] <- scale(data[, !is.fact])
    return(data)
  }
  plt_htmap <- function(mdf, is.fact) {
    if (is.fact)
      g <-
        ggplot(data = mdf, aes(x = samples, y = variable, fill = value)) +
        geom_tile() + theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    else
      g <-
        ggplot(data = mdf, aes(x = samples, y = variable, fill = value)) + geom_tile() + scale_fill_gradient2(low = "blue",
                                                                                                              high = "red",
                                                                                                              mid = "white") + theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    return(g)
  }
  # coeffs is a named vector
  data <- scale_cont_data(data)
  htmp_vars <- as.vector(coeffs$name)
  cont_vars <- htmp_vars[sapply(htmp_vars, function(x) !is.factor(data[, x]))]
  dis_vars <- htmp_vars[sapply(htmp_vars, function(x) is.factor(data[, x]))]
  data <- data[order(data[, var]), ]
  smpls <- sort(sample(nrow(data), smp_size))
  data <- data[smpls, ]
  
  df <- data[, htmp_vars]
  if(length(htmp_vars) == 1) {
    df <- data.frame(df)
    colnames(df) <- htmp_vars
  }
  dis_df <- df[, dis_vars]
  if(length(dis_vars) == 1){
    dis_df <- data.frame(dis_df)
    colnames(dis_df) <- dis_vars
  }
  cont_df <- df[, cont_vars]
  if(length(cont_vars) == 1){
    cont_df <- data.frame(cont_df)
    colnames(cont_df) <- cont_vars
  }
  dis = F
  cont = F
  if(length(dis_vars) > 0){
    dis_df$samples <- c(1:nrow(data))
    dis_df <- melt(dis_df, measure.vars = dis_vars)
    coeffs_dis <- coeffs$name[coeffs$name %in% dis_vars]
    coefff <- coeffs[which(coeffs$name %in% as.vector(coeffs_dis)), ]
    ordered_name <- coefff[order(-coefff[, 'coef']), 'name']
    dis_df$variable <- factor(dis_df$variable, levels = ordered_name)
    index = sapply(levels(dis_df$variable), function(x) which(coeffs$name == x))
    pal <- colorNumeric(c('white', '#6AB4E3', '#266EB4'), domain = c(1:length(unique(dis_df$value))))
    dis_g <- ggplot(data = dis_df, aes(x = samples, y = variable, fill = value)) +
      geom_tile() + theme_minimal() + scale_fill_manual(values = sapply(c(1:length(unique(dis_df$value))), function(x) pal(x))) + 
      labs(x = 'Samples') + guides(fill = guide_legend(title = 'Categorical\n Variable')) + theme( axis.text.y = element_blank(), axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    
    dis_dot <- dot_chart(coeffs[index, ], min=min(coeffs$coef), max=max(coeffs$coef)) + labs(x = '')
    dis = T
  }
  
  if(length(cont_vars) > 0){
    cont_df$samples <- c(1:nrow(data))
    cont_df <- melt(cont_df, measure.vars = cont_vars)
    coeffs_cont <- coeffs$name[coeffs$name %in% c
                               ont_vars]
    coefff <- coeffs[which(coeffs$name %in% as.vector(coeffs_cont)), ]
    ordered_name <- coefff[order(-coefff[, 'coef']), 'name']
    cont_df$variable <- factor(cont_df$variable, levels = ordered_name)
    index = sapply(levels(cont_df$variable), function(x) which(coeffs$name == x))
    cont_g <- ggplot(data = cont_df, aes(x = samples, y = variable, fill = value)) +
      geom_tile() + theme_minimal() + scale_fill_gradient2(low = "blue",
                                                           high = "red",
                                                           mid = "white", name='Continuous\n Variable')  + theme(axis.text.y = element_blank(),axis.title.y = element_blank(), axis.title.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    cont_dot <- dot_chart(coeffs[index, ], min=min(coeffs$coef), max=max(coeffs$coef)) + theme(axis.title.x = element_blank()) + labs(x = '') 
    cont=T
  }
  # dis_df$color <- colorss[as.integer(dis_df$value)]
  # 
  
  # cont_df$color <- pal(cont_df$value)
  data$samples <- c(1:nrow(data))
  # return(data)
  dd = 1/3
  if(!is.factor(data[,var])){
    g <-
      ggplot2::ggplot(data, ggplot2::aes(x = samples, y = data[, var], color = data[, var])) +
      ggplot2::geom_jitter() + theme_minimal()   + scale_color_gradient2(name=paste0("", var, "\n(Response \n Variable)"), low = 'white', mid = '#6AB4E3', high = '#266EB4') + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank()) #+ theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  else{
    dd = 1/6
    pall <- colorNumeric(c('white', '#266EB4'), domain = c(1:length(unique(data[, var]))))
    g <-
      ggplot2::ggplot(data, ggplot2::aes(x = samples, y = var, fill = data[, var])) +
      ggplot2::geom_tile() + theme_minimal()   + scale_fill_manual(name = paste0("", var, "\n(Response \n Variable)"), values = sapply(c(1:length(unique(data[,var]))), function(x) pall(x))) + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    # return(g)
  }
  
  # plt_list <- lapply(htmp_vars, function(x) {
  #   is.fact <- is.factor(data[, x])
  #   df <- data.frame(x = data[ordrd_smpls, x])
  #   colnames(df) <- x
  #   df <- scale_cont_data(df)
  #   df$samples <- ordrd_smpls
  #   return(plt_htmap(melt(df, measure.vars = x), is.fact))
  # })
  
  if(dis && cont){
    
    ret <- grid.arrange(g, cont_dot, cont_g, dis_dot, dis_g, heights = c(dd * (log(1 + length(cont_vars)) +  log(1 + length(dis_vars))), log(1 + length(cont_vars)), log(1 + length(dis_vars))),
                        layout_matrix = rbind(c(NA, 1),
                                              c(2, 3),
                                              c(4, 5)))
  }
  else if(dis)
    ret <- grid.arrange(g, dis_dot, dis_g, heights = c(1, length(dis_vars)), widths = c(1, 2),
                        layout_matrix = rbind(c(NA, 1),
                                              c(2, 3)))
  else if(cont)
    ret <- grid.arrange(g, cont_dot, cont_g, heights = c(1, length(cont_vars)), widths = c(1, 2),
                        layout_matrix = rbind(c(NA, 1),
                                              c(2, 3)))
  return(ret)
  
}

find_related_features <- function(data, var, cores) {
  scale_cont_data <- function(data) {
    data <- scale(data)
    return(data)
  }
  registerDoMC(cores = cores)
  is.cat <- is.factor(data[, var])
  data <- scale_cont_data(data)
  data <- data.matrix(data)
  if (is.cat)
    cvg <- cva.glmnet(
      as.formula(paste(var, '~.')),
      data = data,
      family = 'multinomial',
      type.measure = 'class',
      type.multinomial = 'grouped',
      parallel = T,
      standardize = F
    )
  else
    cvg <-
    cva.glmnet(
      as.formula(paste(var, '~.')),
      data = data,
      type.measure = 'mse',
      family = 'gaussian',
      parallel = T,
      standardize = F
    )
  out <- coef(cvg$modlist[[9]], s = 'lamb
da.1se')
  coeffs <- data.frame(name = out@Dimnames[[1]][out@i], coef = out@x[2:length(out@x)])
  return(list(cva.glm= cvg, coeffs = coeffs, intercept = out@x[1]))
}