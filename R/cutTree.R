#' cutTree
#'
#' Cut a single split from your tree (only possible on the lowest level of the tree).
#'
#' @param tree.obj Tree object, on which the cut will be performed with the given cut point.
#' @param cut.point (integer) non-negative numeric; Node ID of parent node at where you want to cut the tree (has to be the parent node of two leafs)
#'
#' @export
#'
#' @details Please note that it is only possible to cut at the lowest level. If you want to cut on a higher level, you have to cut multiple times.
#'
#' @examples cutTree(tree.obj = tree, cut.point = 3)
cutTree <- function(tree.obj, cut.point) {

  #Ein Abschneiden ist nach unserer Implementierung nur
  #zulaessig, wenn der in cut.point referenzierte Knoten
  #in $kids zwei Knoten angibt, welche selbst is.null($kids)=TRUE aufweisen

  #Das ist der Fall, wenn ein Knoten keine 2 children referenziert
  if (is.null(tree.obj[[cut.point]]$kids[1]) || is.null(tree.obj[[cut.point]]$kids[2])) {
    print("[INFO]: Node does not reference children")
    return(tree.obj)
  } else {
    #Das ist der Fall, wenn ein Knoten 2 children referenziert
    #diese aber nicht direkt aufeinander folgen
    #es also keine blaetter sind
    c1 <- tree.obj[[cut.point]]$kids[1]
    c2 <- tree.obj[[cut.point]]$kids[2]

    if (!((c1+1) == c2)) {
      print("[INFO]: Node does not reference 2 direct children")
      return(tree.obj)
    }
    if (!is.null(tree.obj[[c1]]$kids)) {
      print("[INFO]: Node does not reference leafs as children")
      return(tree.obj)
    }
    if (!is.null(tree.obj[[c2]]$kids)) {
      print("[INFO]: Node does not reference leafs as children")
      return(tree.obj)
    }
  }

  # Hier wird abgefangen, dass der Root knoten weggeschnitten wird
  # dieser darf nur als Parameter angegeben werden, wenn bereits kinder vorhanden sind
  if (is.null(tree.obj[[1]]$kids)) {
    print("[INFO]: Cannot remove root node.")
    return(tree.obj)
  }

  # End error catching..
  #-------------------------------------


  # Was passiert beim Wurzelknoten?
  tmp.tree <- tree.obj

  # Wenn man NULL setzt, werden die Elemente in der Liste
  # jeweils um einen Platz nach vorne gezogen um also die
  # beiden Elemente nach dem cut.point zu entfernen
  # muss zwei mal das folgende Element entfernt werden
  tmp.tree[[cut.point + 1]] <- NULL
  tmp.tree[[cut.point + 1]] <- NULL

  #-------------------------------------

  # IDs ANPASSEN
  for (i in 1:length(tmp.tree)) {

    if (i <= cut.point) {
      # Vor dem Split Point, bleiben alle Elemente auf derselben Stelle
      tmp.tree[[i]] <- tree.obj[[i]]
    }

    if (i > cut.point) {
      # Hinter dem Split Point, werden alle Elemente von zwei Stellen weiter genommen
      tmp.tree[[i]] <- tree.obj[[i + 2]]

      # Und erhalten auch die entsprechende ID dieses Platzes
      tmp.tree[[i]]$id <- as.integer(i)
    }
  }
  #-------------------------------------

  # An dem Cut Point werden die nun unnoetigen Attribute entfertn
  tmp.tree[[cut.point]]$split <- NULL
  tmp.tree[[cut.point]]$surrogates <- NULL
  tmp.tree[[cut.point]]$kids <- NULL

  #-------------------------------------

  # Beim beschneiden des Baumes aendern sich ebenfalls wieder alle
  # Referenzen, die hinter dem Punkt liegen, an dem der Baum gekuerzt wurde
  for (i in 1:length(tmp.tree)) {

    if (!is.null(tmp.tree[[i]]$kids[1])) {
      if (tmp.tree[[i]]$kids[1] > cut.point) {
        tmp.tree[[i]]$kids[1] <- as.integer(tmp.tree[[i]]$kids[1] - 2)
      }
    }

    if (!is.null(tmp.tree[[i]]$kids[2])) {
      if (tmp.tree[[i]]$kids[2] > cut.point) {
        tmp.tree[[i]]$kids[2] <- as.integer(tmp.tree[[i]]$kids[2] - 2)
      }
    }
  }

  #-------------------------------------

  return(tmp.tree)
}
