#' printTree
#'
#' Print a tree object, returned from splittree() for example, (as a text tree).
#'
#' @param tree.obj A tree object of class InitTreeR
#'
#' @export
#'
#' @examples printTree (tree.obj = tree)
printTree <- function(tree.obj) {
  print(partykit::as.partynode(cleanTree(tree.obj)))
}


#' plotTree
#'
#' Plots a tree object, returned from splittree() for example, on the current graphics device.
#'
#' @param tree.obj A tree object of class InitTreeR
#'
#' @details Plot the tree (realised with Partykit (Toolkit for Recursive Partytioning)).
#'
#' @examples plotTree(tree.obj = tree)
#'
#' @export
plotTree <- function(tree.obj) {
  # Daten werden hier noch raus gezogen
  # weil sie danach zum plotten entfernt werden
  data <- tree.obj[[1]]$data

  # Nicht benoetigte Attribute aus dem Baum nehmen
  # um keine Warnungen zu erhalten
  tree.obj <- cleanTree(tree.obj = tree.obj)

  plot(partykit::party(partykit::as.partynode(tree.obj), data))
}


cleanTree <- function(tree.obj) {
  # Test:
  # Um die Warnungen nicht mehr zu erhalten, kann man beim Plotten
  # den Baum noch einmal durchlaufen und die Elemente entfernen
  # die fuer die Warnungen verantwortlich sind
  for (i in 1:length(tree.obj)) {
    if (!is.null(tree.obj[[i]]$target)) {
      tree.obj[[i]]$target <- NULL
    }
    if (!is.null(tree.obj[[i]]$partitions)) {
      tree.obj[[i]]$partitions <- NULL
    }
    if (!is.null(tree.obj[[i]]$improvements)) {
      tree.obj[[i]]$improvements <- NULL
    }
    if (!is.null(tree.obj[[i]]$data)) {
      tree.obj[[i]]$data <- NULL
    }
  }

  return(tree.obj)
}


#' splitOptions
#'
#' Returns a list of improvements for the (next) split at the given split point. The list is sorted by improvements in descending order.
#'
#' @param tree.obj A tree object of class InitTreeR
#' @param split.point	(integer) non-negative numeric; Node ID of terminal node where you want to perform your split.
#'
#' @export
#'
#' @examples splitOptions(tree.obj = tree, split.point = 1)
splitOptions <- function(tree.obj, split.point) {

  v.attributes <- names(tree.obj[[split.point]]$partitions)
  v.improvements <- tree.obj[[split.point]]$improvements

  result <- data.frame(Attribute = v.attributes, Improvement = v.improvements)
  result <- result[order(-result$Improvement), ]

  return(result)
}

