pHSPEpoints <- function(roc_points, threshold) {
  roc_points.filtered <- roc_points[roc_points[,1] >= threshold,]
  roc_points.filtered
}
