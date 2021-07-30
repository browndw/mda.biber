#' to other functions for more detailed and controlled plotting.
#' @param data_frame A data.frame containing 1 categorical variable (as a factor) and continous (numeric) variables.
#' @param n_factors The number of factors to be calculated in the factor analysis.
#' @param cor_min The correlation threshold for including variables in the factor analysis.
#' @param threshold A value indicating the threshold at which variables should be included in dimension score calculations (the default is .35).
#' @return An mda data structure containing scores, means by group, and factor loadings
#' @export
mda_loadings <- function(data_frame, n_factors, cor_min=.20, threshold=.35) {
  
  # retrieve numberic variables
  nums <- unlist(lapply(data_frame, is.numeric))
  fact <- unlist(lapply(data_frame, is.factor))
  
  # text conditions
  if (sum(fact == TRUE) != 1) stop ("You must have a single categorial variable formated as a factor.")
  if (sum(nums == TRUE) < 2) stop ("You must have multiple numeric variables.")
  
  # separate numeric variables from categorical variable
  d <- data_frame[ , nums]
  g <- data_frame[ , fact]
  
  # create correlation matrix
  m_cor <- cor(d, method = "pearson")
  diag(m_cor) <- 0
  
  # trim variables that fall below correlation threshold
  t <- apply(m_cor, 1, function(x) max(abs(x), na.rm = T) > cor_min)
  m_trim <- d[, t]
  m_z <- data.frame(scale(m_trim, center = TRUE, scale = TRUE))
  
  # carry out factor analysis and return loadings
  fa1 <- factanal(m_trim, factors = n_factors, rotation="promax")
  f_loadings <- as.data.frame(unclass(fa1$loadings))
  
  idx <- seq(1:ncol(f_loadings))
  
  # generate scores for either individual observations or cagegory means
  dim_score <- lapply(idx, function(i){
    pos <- row.names(f_loadings)[which(f_loadings[,i] > threshold, arr.ind=T)]
    neg <- row.names(f_loadings)[which(f_loadings[,i] < -threshold, arr.ind=T)]
    pos_sums <- rowSums(m_z[pos])
    neg_sums <- rowSums(m_z[neg])
    dim_score <- mapply(function (x,y) x-y, pos_sums, neg_sums)
    dim_score <- data.frame(cbind(dim_score, as.character(g)), stringsAsFactors = F)
    colnames(dim_score) <- c("score", "group")
    dim_score$score <- as.numeric(dim_score$score)
    return(dim_score)
  })
  
  g_score <- lapply(idx, function(i){aggregate(score~group, dim_score[[i]], mean)})
  
  # format scores and return
  dim_score <- lapply(idx, function(i) data.table::setnames(dim_score[[i]],  c(colnames(f_loadings[i]), "group")))
  dim_score <- do.call("cbind", dim_score)
  dim_score <- dim_score[,unique(colnames(dim_score))]
  dim_score <- dim_score[c("group", setdiff(names(dim_score), "group"))]
  
  g_scores <- lapply(idx, function(i) data.table::setnames(g_score[[i]],  c("group", colnames(f_loadings[i]))))
  g_scores <- suppressWarnings(Reduce(function(...) merge(..., by = "group", all=T), g_scores))
  
  attributes(dim_score)$threshold <- threshold
  attributes(dim_score)$loadings <- f_loadings
  attributes(dim_score)$group_means <- g_scores
  dim_score <- structure(dim_score, class = c("mda", "data.frame"))
  return(dim_score)
}


plot_scree <- function(data_frame, cor_min=.20) {
  nums <- unlist(lapply(data_frame, is.numeric))
  if (sum(nums == TRUE) < 2) stop ("You must have multiple numeric variables.")
  d <- data_frame[ , nums]
  m_cor <- cor(d, method = "pearson")
  diag(m_cor) <- 0
  threshold <- apply(m_cor, 1, function(x) max(abs(x), na.rm = T) > .2)
  m_trim <- d[, threshold]
  ev <- eigen(cor(m_trim))
  ap <- nFactors::parallel(subject=nrow(m_trim), var=ncol(m_trim), rep=100, cent=.05)
  nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  nFactors::plotnScree(nS, legend = F)
}

plot_stick <- function(mda_data, n_factor) {
  
  if (class(mda_data)[1] != "mda") stop ("Your mda_data must be an mda object.")
  scores <- attributes(mda_data)$group_means
  
  factor_n <- paste0("Factor", n_factor)
  
  scores <- dplyr::mutate(scores, pos_neg = ifelse(!!as.name(factor_n) > 0, "High", "Low"))
  
  p1 <- ggplot2::ggplot(scores, ggplot2::aes(y = !!as.name(factor_n), x = 1, label = group, fill = pos_neg)) +
    ggplot2::geom_point(shape = 21) +
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggrepel::geom_text_repel(
      nudge_x      = 0.25,
      direction    = "y",
      hjust        = 0,
      segment.size = 0.1,
      size = 2
    ) +
    ggplot2::xlim(1, 2) +
    ggplot2::theme(legend.position = "none")
  
  return(p1)
}

plot_mda <- function(mda_data, n_factor) {
  
  if (class(mda_data)[1] != "mda") stop ("Your mda_data must be an mda object.")
  loadings <- attributes(mda_data)$loadings
  scores <- attributes(mda_data)$group_means
  threshold <- attributes(mda_data)$threshold
  
  factor_n <- paste0("Factor", n_factor)
  
  scores <- dplyr::mutate(scores, pos_neg = ifelse(!!as.name(factor_n) > 0, "High", "Low"))
  
  loadings <- data.frame(var_cat = row.names(loadings), loadings)
  loadings <- tidyr::pivot_longer(loadings, -var_cat, names_to = "factor")
  loadings <- dplyr::filter(loadings, factor == factor_n)
  loadings <- dplyr::arrange(loadings, value)
  loadings <- dplyr::filter(loadings, value > threshold | value < -threshold)
  loadings <- dplyr::mutate(loadings, text_label = format(round(value, 3)))
  loadings <- dplyr::mutate(loadings, pos_neg = ifelse(value > 0, "High", "Low"))
  
  loading_max <- ifelse(max(loadings$value) > 1, max(loadings$value), 1)
  loading_min <- ifelse(min(loadings$value) < -1, min(loadings$value), -1)
  
  p1 <- ggplot2::ggplot(scores, ggplot2::aes(y = !!as.name(factor_n), x = 1, label = group, fill = pos_neg)) +
    ggplot2::geom_point(shape = 21) +
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggrepel::geom_text_repel(
      nudge_x      = 0.25,
      direction    = "y",
      hjust        = 0,
      segment.size = 0.1,
      size = 2
    ) +
    ggplot2::xlim(1, 2) +
    ggplot2::theme(legend.position = "none")
  
  p2 <- ggplot2::ggplot(data.frame(x = 0, y = -2:2), ggplot2::aes(x, y)) +
    ggplot2::geom_segment(ggplot2::aes(x=0, xend =0, y=1, yend = 2), size=.25,
                 arrow = ggplot2::arrow(length = ggplot2::unit(0.25,"cm")), color = "gray40") +
    ggplot2::geom_segment(ggplot2::aes(x=0, xend =0, y=-1, yend = -2), size=.25,
                 arrow = ggplot2::arrow(length = ggplot2::unit(0.25,"cm")), color = "gray40") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::unit(c(-.9,-5,-.9,-1), "lines"))
  
  if(sum(loadings$pos_neg == "High") > 0 & sum(loadings$pos_neg == "Low") > 0){
    
    p3 <- ggplot2::ggplot(loadings, ggplot2::aes(x = factor, y = reorder(var_cat, value), fill = value)) +
      ggrepel::geom_text_repel(ggplot2::aes(label = var_cat),hjust = 0, nudge_x = 1.5, segment.size = 0.1, size = 2.5, box.padding = .05) +
      ggplot2::geom_tile(ggplot2::aes(width=0.25, height=0.9)) +
      ggplot2::geom_text(ggplot2::aes(label = text_label, color = pos_neg), size = 3) +
      ggplot2::scale_colour_manual(values=c("white", "black")) +
      ggplot2::scale_fill_gradientn(limits = c(loading_min,loading_max),
                           colours=c(viridis::viridis_pal()(2)[2], "white", viridis::viridis_pal()(2)[1]),
                           breaks=c(-threshold, threshold)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::facet_wrap(~ pos_neg, ncol = 1) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank()
      )  +
      ggplot2::theme(plot.margin = ggplot2::unit(c(.1, .25, .1,-2), "lines"))
  }
  
  if(sum(loadings$pos_neg == "High") > 0 & sum(loadings$pos_neg == "Low") == 0) {
    
    p3 <- ggplot2::ggplot(loadings, (ggplot2::aes(x = factor, y = reorder(var_cat, value), fill = value))) +
      ggrepel::geom_text_repel(ggplot2::aes(label = var_cat),hjust = 0, nudge_x = 1.5, segment.size = 0.1, size = 2.5, box.padding = .05) +
      ggplot2::geom_tile(ggplot2::aes(width=0.25, height=0.9)) +
      ggplot2::geom_text(ggplot2::aes(label = text_label, color = pos_neg), size = 3) +
      ggplot2::scale_colour_manual(values=c("white", "black")) +
      ggplot2::scale_fill_gradientn(limits = c(loading_min,loading_max),
                           colours=c(viridis::viridis_pal()(2)[2], "white", viridis::viridis_pal()(2)[1]),
                           breaks=c(-threshold, threshold)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(plot.margin = ggplot2::unit(c(.1, .25, nrow(loadings)*4,-2), "lines"))
    
  }
  
  if(sum(loadings$pos_neg == "High") == 0 & sum(loadings$pos_neg == "Low") > 0) {
    
    p3 <- ggplot2::ggplot(loadings, ggplot2::aes(x = factor, y = reorder(var_cat, value), fill = value)) +
      ggrepel::geom_text_repel(ggplot2::aes(label = var_cat), hjust = 0, nudge_x = 1.5, segment.size = 0.1, size = 2.5, box.padding = .05) +
      ggplot2::geom_tile(ggplot2::aes(width=0.25, height=0.9)) +
      ggplot2::geom_text(ggplot2::aes(label = text_label, color = pos_neg), size = 3) +
      ggplot2::scale_colour_manual(values=c("white", "black")) +
      ggplot2::scale_fill_gradientn(limits = c(loading_min,loading_max),
                           colours=c(viridis::viridis_pal()(2)[2], "white", viridis::viridis_pal()(2)[1]),
                           breaks=c(-threshold, threshold)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(plot.margin = ggplot2::unit(c(nrow(loadings)*4, .25, .1,-2), "lines"))
    
  }
  
  p4 <- ggpubr::ggarrange(p1, p2, p3, nrow = 1, widths = c(1.5, .1, 1.5))
  
  return(p4)
}

