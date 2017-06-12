autoTree <- function(tree.obj, theta) {
  # Tree Obj wird so lange gesplittet, bis:
  # die Laenge des Baumes nach der Schleife gleich bleibt
  # oder das neue Improvement den Wert theta nicht mehr ?berschreitet

  tmp.tree.before <- tree.obj
  continue <- TRUE

  while(continue) {

    #
    tmp.tree.after <- checkThetaTree(tree.obj = tmp.tree.before, theta = theta)

    if (length(tmp.tree.before) == length(tmp.tree.after)) {
      # keine aenderung, kann abgebrochen werden
      return(tmp.tree.after)
    } else {
      # before objekt fuer den kommenden lauf setzen
      tmp.tree.before <- tmp.tree.after
    }


  }

  #return(tmp.tree)
}

checkThetaTree <- function(tree.obj, theta) {

  # durchlaufe baum..
  for(i in 1:length(tree.obj)) {

    # wenn ein node keine kinder hat, ist es ein blatt -> check ob Split
    if (is.null(tree.obj[[i]]$kids)) {

      # daher nehmen wir uns zuerst die verbesserungen fuer diesen split punkt
      split.opt <- splitOptions(tree.obj = tree.obj, split.point = i)
      split.opt <- split.opt[1,]

      if (split.opt$Improvement > theta) {
        print(paste0("[runTree] split at: ", i))

        # node ist blatt und das impr ist besser als theta
        # also wird ein split gemacht
        new.tree.obj <- splitTree(tree.obj = tree.obj,
                                  split.point = i,
                                  split.feature = split.opt$Attribute)

        # das neue objekt geht zuruck
        return(new.tree.obj)
      }
    }
  }
  # wenn kein split gefunden wurde, der besser ist
  # geht das objekt so zuruck, wie es kam
  print("[runTree] no new split")
  return(tree.obj)
}
