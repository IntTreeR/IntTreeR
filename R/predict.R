#' doPrediction
#'
#' Predict the target attribute for the new data of a test set.
#'
#' @param tree.obj a tree object of class InitTreeR
#' @param predict.data a data.frame object with test data which should get predicted.
#' @param pred.class an boolean for showing the predicted class in the output (optional: default = FALSE)
#'
#' @export
#'
#' @examples doPrediction(tree.obj = tree, dataset = dat)
doPrediction <- function(tree.obj, predict.data, pred.class) {

  # Wenn Parameter fuer automatischen Baum aufbau nicht angegeben
  # dann automatisch FALSE -> NEIN
  if (missing(pred.class)) {
    pred.class <- FALSE
  }


  # Entscheidung in welches Blatt die jeweiligen Daten reingeschoben werden.
  # Aus den gegebenen Daten alle Spalten, ausser Zielvariable
  # in data.sort schreiben
  data.sort <- predict.data[, (names(predict.data) != tree.obj[[1]]$target)]
  # Danach die Zielvariable als letzte anhaengen
  data.sort[, tree.obj[[1]]$target] <- predict.data[, tree.obj[[1]]$target]


  predict.obj <- partykit::party(partykit::as.partynode(cleanTree(tree.obj)), tree.obj[[1]]$data)

  if (!pred.class) {
    # Wenn die klasse nicht predictet werden soll, werden hier die Nodes zurueckgegeben
    return(predict(predict.obj, data.sort))
  } else {

    preds <- predict(predict.obj, data.sort)

    predictions <- c()
    node.predictions <- c()

    target <- tree.obj[[1]]$target

    for (i in preds) {
      target.factor <- as.factor(tree.obj[[i]]$data[, target])
      sum.target <- summary(target.factor)

      node.pred <- names(sum.target[which(sum.target == max(sum.target))])
      pred <- max(sum.target) / nrow(tree.obj[[i]]$data)

      predictions <- append(predictions, pred)
      node.predictions <- append(node.predictions, node.pred)
    }
    # Setting Names for Vector of predictions
    names(predictions) <- node.predictions

    return(predictions)
  }

}


