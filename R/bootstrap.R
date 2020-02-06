#' bBootstrap bounding boxes
#' @description bootstrap bounding boxes in order to produce smooth box distribution
#' @param x input 2-d matrix: first column is wisth, second column is height of the bounding box
#' @param scale how to rescale each box. For example 2 means it will randomly dowscaled-upscaled
#' within a range of 1/2 to 2. Which means it's size will be within a range being 2x smaller to 2x larger
#' then original bounding box
#' @param size size of bootstrapped result
#' @param quantiles keep resulting samples within these quantiles of the original distribution
#' @export
bootstrap_boxes = function(x, scale = 2, size = nrow(x) * 10, quantiles = c(0.05, 0.95)) {
  w_quantiles = quantile(x[, 1], quantiles)

  h_quantiles = quantile(x[, 2], quantiles)

  N = nrow(x)
  rows = sample(N, size, replace = T)
  upscale_downscale = sample(c(-1, 1), size, replace = T)
  x_scales = runif(size, min = 1, max = scale)**upscale_downscale
  x_resample = x[rows, , drop = F] * x_scales

  # filter out boxes which are out of the range of original scales
  x_resample = x_resample[x_resample[, 1] %between% w_quantiles, , drop = F]
  x_resample = x_resample[x_resample[, 2] %between% h_quantiles, , drop = F]
  x_resample
}
