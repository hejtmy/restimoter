#' Helper function
#' 


euclid_distance <- function(point1, point2){
  return(sqrt(sum((point2 - point1)^2)))
}

euclid_distance_col <- function(points){
  points_shifted <- rbind(c(0,0), points[1:nrow(points) - 1, ])
  points_sub <- points - points_shifted
  vec_sums <- apply(points_sub, 1, function(x) sqrt(sum(x ^ 2)))
  return(vec_sums)
}