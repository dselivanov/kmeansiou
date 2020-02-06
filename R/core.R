# box = (w, h)
iou = function(src, dst) {
  w = pmin(src[, 1], dst[[1]])
  h = pmin(src[, 2], dst[[2]])
  inters = w * h
  union = src[, 1] * src[, 2] + dst[[1]] * dst[[2]] - inters
  return(inters / union)
}

init_centroids = function(boxes, k) {
  N = nrow(boxes)
  # area = boxes[, 1] * boxes[, 2]
  # # as a first centroid take largest box
  # centroid = boxes[which.max(area), ]
  centroid = boxes[sample(N, 1), ]
  centroids = list(centroid)
  for (j in seq_len(k - 1)) {
    # create a matrix where in each column we will keep distances
    # from points to already choosen centroids
    dd = matrix(0, nrow = N, ncol = length(centroids))
    for (i in seq_along(centroids)) {
      dd[, i] = iou(boxes, centroids[[i]])
    }
    # distance_to_choosen_centroids = rowSums(dd)
    new_centroid = boxes[which.min(rowSums(dd)), ]
    centroids = c(centroids, list(new_centroid))
  }
  centroids = do.call(rbind, centroids)
}

print_status = function(..., verbose) {
  if (verbose) {
    message(...)
  }
}

#' K-means clustering of 2-d data with IoU distance
#' @description Allows to cluster 2-dimensional with k-means using intersection over union distance.
#' This is useful for anchors initialization in the YOLO-family of models for object detection.
#' See Redmon et al. (2016) <arXiv:1612.08242>, Redmon et al. (2016) <arXiv:1804.02767>.
#' @param x input 2-d matrix: first column is wisth, second column is height of the bounding box
#' @param k number of clusters
#' @param iter number of k-means iterations
#' @param init 2-d matrix of initial centroids. Should have \code{k} rows
#' @param verbose logical, whether to print information during fitting the model
#' @return list of 2 values. First is a matrix of cluster centers,
#' second is cluster membership indices.
#' @export
kmeans_iou = function(
  x,
  k,
  iter = 10,
  init = x[sample(nrow(x), size = k), , drop = FALSE],
  verbose = FALSE) {

  stopifnot(is.matrix(x))
  N = nrow(x)
  centroids = init

  for (j in seq_len(iter)) {
    ious = matrix(0, nrow = N, ncol = k)
    for (i in seq_len(k)) {
      ious[, i] = iou(x, centroids[i, ])
    }
    cluster_membership = max.col(ious)
    # recalculate centroids
    centroids = matrix(0, nrow = k, ncol = ncol(x))
    clust_iou = 0
    # clust_iou = numeric(k)
    for (ind in seq_len(k)) {
      cluster_membership_index = cluster_membership == ind
      clust_iou = clust_iou + sum(ious[cluster_membership_index, ind, drop = FALSE])
      centroids[ind, ] = colMeans(x[cluster_membership_index, , drop = FALSE])
    }
    print_status(sprintf("%s avg IOU: %.3f", Sys.time(), clust_iou/N), verbose = verbose)
  }
  # sort centroid in increasing order as it was in original YOLO implementation
  centroids2 = centroids**2
  clusters_ord = sqrt(centroids2[, 1] +  centroids2[, 2])
  clusters_ord = order(clusters_ord)
  centroids = centroids[clusters_ord, ]
  cluster_membership = match(cluster_membership, clusters_ord)
  list(centroids = centroids, cluster_membership = cluster_membership)
}

# useful to copy-paste cluster to YOLO config
yolo_centroids_string = function(clusters, w = 1024, h = 1024) {
  paste(as.integer(clusters$centroids[, 1] * w), as.integer(clusters$centroids[, 2] * h), sep = ',', collapse = ', ')
}

