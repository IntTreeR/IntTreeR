#' initTree
#'
#' The function builds a tree object by generating the root node.
#'
#' @param dataset	a data.frame object for the following model building.
#' @param v.target	 column name of the target variable in your dataset.
#' @param auto	default FALSE. If TRUE the tree builds up automatically.
#' @param theta	non-negative float or double; Improvement restriction when the tree is built automatically (auto = TRUE). Stops if the current best improvement is less than theta.
#'
#' @details This function is a method to charge the best partitions as well as their related improvements.
#'
#' @return A tree object of class InitTreeR
#' @export
#'
#' @examples tree <- initTree(dataset = data, v.target =”income”, auto = FALSE)
initTree <- function(data, v.target, auto, theta) {  # Daten und Zielattribut; zus. opt. Parameter

  # Wenn Parameter fuer automatischen Baum aufbau nicht angegeben
  # dann automatisch FALSE -> NEIN
  if (missing(auto)) {
    auto <- FALSE
  } else { # Dann wurde auto angegeben
    #
    if (auto) {
      print("automated supplied and = TRUE")

      if (missing(theta)) {
        print ("[INFO]: No Argumemt theta given. Choosing splits for theta = 0")
        theta <- 0
      } else {
        # Theta angegeben und
        if (theta < 0) {
          print ("[INFO]: Min. value for theta is 0. Choosing splits for theta = 0")
          theta <- 0
        }
      }
    }

  }


  # Aus den gegebenen Daten alle Spalten, ausser Zielvariable
  # in data.sort schreiben
  data.sort <- data[, (names(data) != v.target)]
  # Danach die Zielvariable als letzte anhaengen
  data.sort[, v.target] <- data[, v.target]


  # Aufrufen der Partition Funktion mit den gegebenen Daten
  l.partitions <- createPartitions(data = data.sort, v.target = v.target)

  # Aufrufen der Funktion zum berechnen der besten Partitionen
  # sowie deren zugehoerige improvements
  result.best.partitions <- calcBestPartitions(data = data.sort,
                                           v.target = v.target,
                                           l.partitions = l.partitions)


  tree.obj <- list(
    list(
          id = 1L,
      target = v.target,
  partitions = result.best.partitions@partitions.best.splits,
improvements = result.best.partitions@v.improvements,
        data = data.sort))

  # Wenn man automated auf true setzt, beginnt hier die dazugehoerige Verarbeitung
  if (auto) {
    finished.tree.obj <- autoTree(tree.obj = tree.obj, theta = theta)
    return(finished.tree.obj)
  }

  return(tree.obj)
}


# Funkion zum Mapping der Partitionen(left(1)/right(2)) zu einem Partysplit
mapSplit <- function(data, p.left, p.right, split.col, complement) {

  if (is.factor(data[, split.col])) {
    # Verarbeitung von faktoriellen splits

    split.feature.index <- sort(unique(data[[split.col]]))
    map.index <- vector()

    if (is.null(p.left)) {
      # Linke Partition ist NULL -> Alles nach Rechts
      return(partykit::partysplit(varid = split.col, breaks = 0, index = c(2L, 1L), info = "s"))

    } else if (is.null(p.right)) {
      # Rechte Partition ist NULL -> Alles nach Links
      return(partykit::partysplit(varid = split.col, breaks = 0, info = "s"))

    } else {
      # Keine Partition ist NULL -> Aufteilung nach Schleife

      for (i in 1:length(split.feature.index)) {
        if (split.feature.index[i] %in% p.left) {
          map.index[i] <- 1L
        } else {
          map.index[i] <- 2L
        }
      }

      #print(paste("Return1- varid: ", split.col, "   index: ", map.index))
      return(partykit::partysplit(varid = split.col, index = map.index))
    }


  } else {
    # Verarbeitung von numeric splits
    # Partitionen sind bei NUMERISCH aufsteigend sortiert (Bsp. 1,2,3,4)
    # Wenn p.left NULL -> NULL | 1,2,3,4 .. wenn p.right NULL -> 1,2,3,4 | NULL
    if (is.null(p.left)) {
      # Links ist NULL; also trenne bei MIN rechts (p.left NULL -> NULL | 1,2,3,4)
      min.right <- min(as.numeric(p.right))

      avg.features <- min.right

    } else if (is.null(p.right)) {
      # Rechts ist NULL; also trenne bei MAX left (p.right NULL -> 1,2,3,4 | NULL)
      max.left <- max(as.numeric(p.left))

      avg.features <- max.left

    } else {
      # Keine Partition NULL
      max.left <- max(as.numeric(p.left))
      min.right <- min(as.numeric(p.right))

      # AVG aus groester Wert der linken Seite und kleinsten Wert der rechten Seiten
      # ist der Splitpoint
      avg.features <- (max.left + min.right) * 0.5
    }


    # Bei normalen Split 0, bei Surrogate Split 0 oder 1.
    if (complement == 0) {
      # links(<=) rechts(>)

      #print(paste("Return 2- varid: ", split.col, "   breaks: ", avg.features))
      return(partykit::partysplit(varid = split.col, breaks = avg.features))
    } else {
      # links(>) rechts(<=)

      return(partykit::partysplit(varid = split.col, breaks = avg.features, index = c(2L, 1L)))
    }
  }
}


