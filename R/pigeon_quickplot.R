#' Quick ggplot2 parameters
#'
#' @param method What settings you want
#' @example
#' pigeon_plot("boxjitter")
pigeon_quickplot <- function(method = "default"){
  ##$ Made for quick exploration, not actual plotting

  ##$ Simple Box & Jitter plot
  if (method == "default" | method == "boxjitter"){

    list(
      ggplot2::geom_bar(stat = "summary", position = "dodge",fun.y = "mean", width =.5, alpha = .6),
      ggplot2::geom_jitter(width = .3),
      ggplot2::geom_hline(aes(yintercept = 0.0), color = "black", linetype = "dashed"),
      ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = .1)
    )

  }

}

