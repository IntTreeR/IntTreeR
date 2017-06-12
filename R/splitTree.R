#' splitTree
#'
#' Performs a new binary split at a selected splitpoint.
#'
#' @param tree.obj The tree object, on which the split will be performed with the given split point.
#' @param split.point (integer) non-negative numeric; Node ID of terminal node where you want to perform your split.
#' @param split.featurecharacter or string; Column name of the attribute which should be used for the split.
#' @param max.surrogates(Optional) (Integer) non-negative; the maximum number of surrogates, which will be calculated for the split. (Default is 3)
#' @param custom.split(Optional) (Integer) custom point to split (only for numeric attributes)	
#' @details Missing values are handled via Surrogate Splits. In most details, it follows „Leo Breiman et. al. (1984). “quite closely.
#'
#' @export
#'
#' @examples tree <- splitTree(tree.obj = tree, split.point = 1, split.feature = “income”)
splitTree <- function(tree.obj, split.point, split.feature, max.surrogates, custom.split) {
	
	if (missing(custom.split)) {
    # Default Parameter: NULL
    custom.split <- NULL
	}
	
	
	
  if (missing(max.surrogates)) {
    # Default Parameter: Maximal 3 Surrogate Splits

    max.surr.splits <- 3

  } else {

    max.surr.splits <- max.surrogates
    max.surr.splits <- as.integer(max.surrogates)

    if(max.surr.splits < 0) {
      print("Invalid Value for max.surrogates")
      return(tree.obj)
    }
  }

  # Knoten koennen nicht merhfach gesplittet werden
  if (!is.null(tree.obj[[split.point]]$kids)) {
    stop ("Multiple splits on the same Node are not allowed!")
  }
  # Knoten koennen nur gesplittet werden, bei min 2 Datensaetzen
  if (nrow(tree.obj[[split.point]]$data) < 2) {
    print ("Not enough Data to perform splits!")
    return(tree.obj)
  }
  # Wenn das Split Attribut, nur noch mit einer Auspraegung
  # in den uebergebenen Daten vorhanden ist: Info ausgeben und kein Split moeglich
  if (length(unique(tree.obj[[split.point]]$data[, split.feature])) == 1) {
    print ("[INFO]: Split Attribute with only one feature in data.")
    return(tree.obj)
  }

  #-------------------------------------
  new.tree.obj <- list()

  #-------------------------------------
  for (i in 1:length(tree.obj)) {
    if (i <= split.point) {

      # Vor dem Split Point, bleiben alle Elemente auf derselben Stelle
      new.tree.obj[[i]] <- tree.obj[[i]]
    }
    if (i > split.point) {

      # Hinter dem Split Point, werden alle Elemente zwei Stellen weiter gesetzt
      new.tree.obj[[i + 2]] <- tree.obj[[i]]
      # Und erhalten auch die entsprechende ID dieses Platzes
      new.tree.obj[[i + 2]]$id <- as.integer(i + 2)
    }
  }
  #-------------------------------------


  #-------------------------------------
  # Weil sich alle Referenzen hinter dem Split Point durch das verschieben aendern
  # muss der Baum noch einmal durchlaufen und alle Referenzen angepasst werden
  for (i in 1:length(new.tree.obj)) {
    if (!is.null(new.tree.obj[[i]]$kids[1]))  {
      if (new.tree.obj[[i]]$kids[1] > split.point) {
        # print(paste0("Referenz um 2 erhohen bei: ", i, " auf ", as.integer(new.tree.obj[[i]]$kids[1] + 2)))
        new.tree.obj[[i]]$kids[1] <- as.integer(new.tree.obj[[i]]$kids[1] + 2)
      }
    }

    if (!is.null(new.tree.obj[[i]]$kids[2])) {
      if (new.tree.obj[[i]]$kids[2] > split.point) {
        # print(paste0("Referenz um 2 erhohen bei: ", i, " auf ", as.integer(new.tree.obj[[i]]$kids[2] + 2)))
        new.tree.obj[[i]]$kids[2] <- as.integer(new.tree.obj[[i]]$kids[2] + 2)
      }
    }
  }
  #-------------------------------------


  #-------------------------------------
  # Am Ende das Split Attribut an den Splitpoint anfuegen
  # Varid entspr. Spaltenname
  split.col <- which(colnames(tree.obj[[split.point]]$data) == split.feature)


	# Wenn eine explizite Partitionierung uebergeben wird, dann wird diese genommen
	# ansonsten wird die beste berechnete Partition genommen (gelassen).
	if (!is.null(custom.split)) {
		tree.obj[[split.point]]$partitions[[split.col]]$left = 	custom.split
		tree.obj[[split.point]]$partitions[[split.col]]$right = custom.split
	} 
	
  # Am splitpoint des Baum Objektes, wird mapsplit aufgerufen, die aus den Parametern einen
  # Partysplit erzeugt. Dieser wird zusaetzlich in das Baum Objekt gehangen
  # DAS IST DER NORMALE SPLIT DES BAUMS; (Deswegen auch statisch comp=0 )
  	new.tree.obj[[split.point]]$split <- mapSplit(data = tree.obj[[split.point]]$data,
    	                                          p.left = tree.obj[[split.point]]$partitions[[split.col]]$left,
        	                                      p.right = tree.obj[[split.point]]$partitions[[split.col]]$right,
            	                                  split.col = split.col,
                	                              complement = 0)
 
  # ---------------------------------------------------------------------

  # Jetzt werden fuer den gewaehlten Split Point alle Surrogate Trenner bestimmt
  surrogate.splits.obj <- calcSurrogateSplit(v.mvattribute = split.feature,
                                                  v.target = tree.obj[[1]]$target,
                                             df.attributes = tree.obj[[split.point]]$data,
                                              l.partitions = tree.obj[[split.point]]$partitions)


  # ---------------------------------------------------------------------
  # Berechnung der Wahrscheinlichkeit fuer Majoritaet(Mehrheit)
  # Abschneiden des Data Frame nach der Majoritaet
  df.temp <- tree.obj[[split.point]]$data
  p.temp <- tree.obj[[split.point]]$partitions

  majority <- nrow(df.temp[which(df.temp[, split.feature] %in% p.temp[[split.feature]]$left), ]) / nrow(df.temp)


  # ---------------------------------------------------------------------

  # Diese Surrogate werden absteigend durchlaufen und jeweils ein
  # Partysplit durch die mapsplit funktion generiert
  l.surrogates <- list()
  for (i in 1:nrow(surrogate.splits.obj@df.best.splits)) {

    # Es kann nur ein Surrogate (Partysplit) erstellt werden
    # wenn das im Parameter festgelegte maximum noch nicht erreicht ist
    if (i <= max.surr.splits) {

      current.prob <- surrogate.splits.obj@df.best.splits$prob[i]

      if((majority > current.prob) || ((1-majority) > current.prob)) {
        # Fuer alle surrogate Splits, die eine geringere Wahrscheinlichkeit
        # aufweisen, als der Mehrheitsentscheid, werden keine Split Objekte angelegt
        # diese Werte koennen in der Schleife uebersprungen werden
        next()
      }

      # findet Spaltenummer des Surrogate Trenners in den Original Daten
      surrogate.col <- which(colnames(tree.obj[[split.point]]$data) == surrogate.splits.obj@df.best.splits[i, ]$attribute)

      # fuer jeden Schleifendurchgang, einen Split generieren und diesen anschliessend an die Liste
      # der Surrogate Trenner anhaengen
                          # data, p.left, p.right, split.col, complement
      current.mapsplit <- mapSplit(data = tree.obj[[split.point]]$data,
                                 p.left = surrogate.splits.obj@partitions.best.splits[[i]]$left,
                                p.right = surrogate.splits.obj@partitions.best.splits[[i]]$right,
                              split.col = surrogate.col,
                             complement = surrogate.splits.obj@df.best.splits[i, ]$comp)


      l.surrogates[[i]] <- current.mapsplit
    }
  }

  # hier muss jetzt noch das Split Objekt fuer die Mehrheitsentscheidung
  # generiert und der Liste der surrogate trenner (l.surrogates) angehangen werden
  if(majority > (1-majority)) {
    # P fuer knoten Links ist groesser: Alles nach Links
    #print(paste0("mehrheitssplit alles nach links ", majority))


    majority.split <- mapSplit(data = tree.obj[[split.point]]$data,
                               p.left = tree.obj[[split.point]]$partitions[[split.col]]$left,
                               p.right = NULL,
                               split.col = split.col,
                               complement = 0)
  } else {
    # P fuer Knoten Rechts ist groesser: Alles nach Rechts
    #print(paste0("mehrheitssplit alles nach rechts ", (1-majority) ))

    majority.split <- mapSplit(data = tree.obj[[split.point]]$data,
                               p.left = NULL,
                               p.right = tree.obj[[split.point]]$partitions[[split.col]]$right,
                               split.col = split.col,
                               complement = 0)
  }
  # an die bestehende Liste +1 wird der Merheitssplit angehangen
  l.surrogates[[(length(l.surrogates)+1)]] <- majority.split



  # ---------------------------------------------------------------------
  # Hinzufuegen der fertigen Liste der Surrogate Trenner an den aktuellen Splitpoint
  new.tree.obj[[split.point]]$surrogates <- l.surrogates


  # Referenz der Children an den Splitpoint anfuegen
  new.tree.obj[[split.point]]$kids <- c((split.point + 1), (split.point + 2))
  new.tree.obj[[split.point]]$target <- tree.obj[[1]]$target
  #-------------------------------------

  #-------------------------------------
  # Beide Children fuer den Split generieren
  c1 <- list()
  c2 <- list()
  # Die dazugehoerigen IDs vergeben ([1nach Split], [2nach Split])
  c1$id <- as.integer(split.point + 1)
  c1$target <- tree.obj[[1]]$target
  c2$id <- as.integer(split.point + 2)
  c2$target <- tree.obj[[1]]$target
  # Und die Children in das neue Baum Objekt setzen
  new.tree.obj[[split.point + 1]] <- c1
  new.tree.obj[[split.point + 2]] <- c2
  #-------------------------------------

  # Hier wird das Temporaere Baum Objekt erzeugt
  # Zuerst wird das vorhandene objekt kopiert
  temp.list <- new.tree.obj
  # Dann von den fuer Partykit unwichtigen Attributen gesaeubert
  temp.list <- cleanTree(temp.list)

  # Ein temporaeres Baumobjekt wird erzeugt, um die Daten aus den neuen Children
  # herausziehen zu koennen
  temp.party <- partykit::party(partykit::as.partynode(temp.list), new.tree.obj[[split.point]]$data)

  # Diese Daten werden den neu generierten children in der Liste angehangen
  new.tree.obj[[split.point + 1]]$data <- temp.party[[split.point + 1]]$data
  new.tree.obj[[split.point + 2]]$data <- temp.party[[split.point + 2]]$data


  # ALLE Partitionen aus den Daten in den children erstellen
  c1.partitions <- createPartitions(data = new.tree.obj[[split.point + 1]]$data,
                                v.target = new.tree.obj[[1]]$target)

  c2.partitions <- createPartitions(data = new.tree.obj[[split.point + 2]]$data,
                                v.target = new.tree.obj[[1]]$target)

  # Aus diesen Partitionen die besten auswaehlen und die Improvements speichern
  c1.partition.results <- calcBestPartitions(data = new.tree.obj[[split.point + 1]]$data,
                                         v.target = new.tree.obj[[1]]$target,
                                     l.partitions = c1.partitions)

  c2.partition.results <- calcBestPartitions(data = new.tree.obj[[split.point + 2]]$data,
                                         v.target = new.tree.obj[[1]]$target,
                                     l.partitions = c2.partitions)


  # Diese Partitionen und die Improvements in die children schreiben
  new.tree.obj[[split.point + 1]]$partitions <- c1.partition.results@partitions.best.splits
  new.tree.obj[[split.point + 2]]$partitions <- c2.partition.results@partitions.best.splits

  new.tree.obj[[split.point + 1]]$improvements <- c1.partition.results@v.improvements
  new.tree.obj[[split.point + 2]]$improvements <- c2.partition.results@v.improvements

  # Danach wird der Baum nicht mehr gebraucht und entfernt..
  rm(temp.party)

  return(new.tree.obj)
}
