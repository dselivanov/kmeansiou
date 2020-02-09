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
#' @param early_stop integer - stop k-means if there is no improvement during last \code{early_stop} iterations
#' @param verbose logical, whether to print information during fitting the model
#' @return list of 2 values. First is a matrix of cluster centers,
#' second is cluster membership indices.
#' @export
kmeans_iou = function(
  x,
  k,
  iter = 10,
  init = x[sample(nrow(x), size = k), , drop = FALSE],
  early_stop = 3L,
  verbose = FALSE) {

  stopifnot(is.matrix(x))
  N = nrow(x)
  centroids = init
  iter_without_improvement = 0
  best_iter = 0
  best_score = 0
  best = list()
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
    score = clust_iou / N
    if (score > best_score) {
      best_score = score
      best_iter = j
      best = list(centroids = centroids, cluster_membership = cluster_membership)
      iter_without_improvement = 0L
    } else {
      iter_without_improvement = iter_without_improvement + 1L
    }
    if (iter_without_improvement >= early_stop) {
      msg = sprintf("stoping at iteration %d - no improvement during last %d iterations",
                    j, early_stop)
      print_status(msg, verbose = verbose)
      break
    }

    print_status(sprintf("%s iter %d avg IOU: %.3f", Sys.time(), j, score), verbose = verbose)
  }
  print_status(sprintf("best iter = %d, best score = %.3f", best_iter, best_score), verbose = verbose)
  # sort centroid in increasing order as it was in original YOLO implementation
  centroids2 = best$centroids**2
  clusters_ord = sqrt(centroids2[, 1] +  centroids2[, 2])
  clusters_ord = order(clusters_ord)
  best$centroids = best$centroids[clusters_ord, ]
  best$cluster_membership = match(best$cluster_membership, clusters_ord)
  best
}

# useful to copy-paste cluster to YOLO config
yolo_centroids_string = function(clusters, w = 1024, h = 1024) {
  paste(as.integer(clusters$centroids[, 1] * w), as.integer(clusters$centroids[, 2] * h), sep = ',', collapse = ', ')
}

