plot_walking_path <- function(df, limits = NA){
  if (!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 to continue.")
  }
  plt <- ggplot(df, aes(Position.X, Position.Y))
  plt <- plt + geom_path()
  if(length(limits) == 2 && !all(is.na(limits))){
    plt <- plt + xlim(0, obj$location_size[1]) + ylim(0, obj$location_size[2])
  }
  plt
}