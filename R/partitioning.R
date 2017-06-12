# createPartitions: sortiert ggf. die Auspraegungen aller Merkmale der Daten
# und bildet durch die partition Funktion eine Liste aller Partitionen fuer
# alle Merkmale in den Daten
# gibt eine Liste (Merkmale) mit Elementen Liste (Partitionen ) mit Elementen Liste (je eine Partition)
createPartitions <- function(data, v.target) {

  partitioned <- list()

  for (i in colnames(data)) {
    if (i == v.target) {
      next()
    }

    v.attribute <- i
    col.attribute <- which(colnames(data) == as.character(v.attribute))
    col.target <- which(colnames(data) == as.character(v.target))

    # auspraegungen
    features <- na.exclude(unique(data[, col.attribute]))
    features.target <- unique(data[, col.target])


    # Unterscheidung ob das betrachtete attribut ein Faktor/Numerisch/...
    ## das attribut ist faktor und hat 2 auspraegungen [yes;no]/[good;bad]
    if (length(features) == 1) {
      # Das aktuelle Attribut hat nur EINE Auspraegung in den Daten
      partitioned[[i]][[1]] <- list(left = features[1], right = NULL)

    } else if (is.factor(data[, col.attribute]) && length(features) == 2) {
      # Attribut mit genau 2 Auspraegungen
      features <- sort(features)
      features <- as.vector(features)

      # Partition Funktion muss bei nur 2 Auspraegungen nicht aufgerufen werden
      partitioned[[i]][[1]] <- list(left = as.vector(features[1]), right = as.vector(features[2]))
      ########################################################################

    } else if (is.factor(data[, col.attribute]) && length(features) > 2) {
      # Attribut mit mehreren Auspraegungen

      #ToDo
      #Wie sieht das aus, wenn ein metrisches Zielattribut gewaehlt ist?

      feature.processed <- c()
      feature.order <- c()
      # Wenn das Zielattribut 2 Auspraegungen hat
      if (length(features.target == 2)) {
        for (j in features) {
          # Wird fuer jedes Feature bestimmt, wie oft diese Auspraegung vorhanden ist
          # waehrend die Zielvariable gleichzeitig in erster Auspraegung vorhanden ist
          n.feat.target <- nrow(data[which((data[, col.attribute] == j) & (data[, col.target] == features.target[1])), ])
          # Dann wie oft die Auspraegung des Features generell vorkommt
          # so kann der Relative Anteil dieses Features bestimmt werden
          n.feat <- nrow(data[which(data[, col.attribute] == j), ])
          # relativer anteil der auspraegung bestimmen
          rel.share <- n.feat.target / n.feat
          # anhaengen des gerade berechneten anteils
          feature.order <- append(feature.order, rel.share)
          # und ebenso wird das aktuelle feature einem vektor angefuegt
          feature.processed <- append(feature.processed, j)
        }
        features <- data.frame(feature = feature.processed, feature.order = feature.order)
        # nach den besten features sortieren
        features <- features[order(feature.order), ]
        # danach wieder nur die features spalte entnehmen
        features <- features$feature

        # Partition Funktion liefert eine Liste mit List Elementen
        p <- partition(features = features)
        for (j in 1:length(p)) {
          partitioned[[i]][[j]] <- p[[j]]
        }
      }
      ########################################################################

    } else if (is.numeric(data[,col.attribute]) || is.integer(data[,col.attribute])) {
      # Numerisches Attribut mit X Auspraegungen
      features <- sort(features)
      features <- as.vector(features)

      p <- partition(features = features)
      for (j in 1:length(p))
      {
        partitioned[[i]][[j]] <- p[[j]]
      }
      ########################################################################

    }
    #Ende Schleife
  }
  return(partitioned)
}


# partition: verwendet einen Vektor von (sortierten) Auspraegungen (features)
# um durch verschieben alle moeglichen partitionen zu erstellen und
# gibt eine Liste (Aller Partitionen des Merkmals) mit Elementen von Liste (je eine Partition) zurueck
partition <- function(features) {

  partitioned <- list()
  fRight <- features
  fLeft <- c()

  for (i in 1:(length(features) -1)) {
    element <- list()

    fLeft[length(fLeft) +1] <- as.character(features[i])
    fRight <- fRight[-1]

    # List mit allen Elementen der aktuellen Partition
    # geteilt in left und right
    if (is.numeric(features) || is.integer(features)) {
      element <- list(left = as.numeric(fLeft), right = as.numeric(fRight))
    } else {
      element <- list(left = fLeft, right = as.character(fRight))
    }

    # Liste mit allen Elementen der aktuellen Partition
    # wird als EIN Element der Liste partitioned hinzugefuegt
    partitioned[[i]] <- element
  }
  return(partitioned)
}


inttree.partition.results <- setClass("inttree.partition.results", slots = c(v.improvements = "vector",
                                                                     partitions.best.splits = "list"))
# calcBestPartitions: verwendet die Daten (ggf. der Children),
# die Zielvariable und eine Liste (ALLER) Partitionen,
# gibt ein Objekt mit den besten Partitionen und den dazugehoerigen Improvements
calcBestPartitions <- function(data, v.target, l.partitions) {
  # Vector zum Speichern der besten Improvements
  best.improvements <- c()
  # Liste mit den dazugehoerigen Partitionen
  best.partitions <- list()

  for (i in names(l.partitions)) {
    # improvement kann nicht kleiner 0 sein, deswegen startwert -1 und ggf ueberschreiben (if..)
    best <- -1
    for (j in 1:length(l.partitions[[i]])) {

      # aktuelle Partition aus Masterliste aller Partitionen fuer jeweiliges Merkmal nehmen
      p.left <- l.partitions[[i]][[j]]$left
      p.right <- l.partitions[[i]][[j]]$right

      # Hier landet man, wenn ein Merkmal nur noch eine Auspraegung in den Daten aufweist
      # dann koennen keine Partitionen gebildet werden und das Improvement ist 0
      if (is.null(p.left) || is.null(p.right)) {

        current.improve <- 0
      } else {
        # anhand dieser Partition wird das aktuelle Improvement berechnet
        current.improve <- calcImprovement(data[, i], data[, v.target], p.left, p.right)
      }

      # Sonderfall Division durch 0 abfangen (in diesem Fall wird das Improvement auf 0 gesetzt)
      # der Fall tritt zum Beispiel ein wenn nur noch ein Good da ist und hierbei das Merkmal um das es gerade
      # geht NA ist.
      if (is.nan(current.improve)) {
        current.improve <- 0
      }

      # wenn das Improvement der aktuellen Partition besser ist als die vorherigen, wird die Partition gespeichert
      if (current.improve > best) {
        # Bei zwei identischen Improvements, wird so das erste zurueckgeliefert
        #print("New Best")
        best <- current.improve
        best.p.left <- p.left
        best.p.right <- p.right
      }

    }
    # anhaengen des besten Improvements fuer das aktuelle Merkmal
    best.improvements <- append(best.improvements, best)
    # Erstellen einer Liste mit linker und rechter Partition, als beste Partitionen fuer das aktuelle Merkmal
    best.partitions.feature <- list(left = best.p.left, right = best.p.right)
    best.partitions[[i]] <- best.partitions.feature
  }

  result.obj <- new("inttree.partition.results", v.improvements = best.improvements,
                                         partitions.best.splits = best.partitions)

  return(result.obj)
}
