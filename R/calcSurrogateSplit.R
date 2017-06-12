inttree.surrogate.split <- setClass("inttree.surrogate.split", slots = c(df.best.splits = "data.frame",
                                                                         partitions.best.splits = "list"))

# String MissingValue Attribut, Zielattribut, DataFrame aller Merkmale, liste aller Partitionen
calcSurrogateSplit <- function(v.mvattribute, v.target, df.attributes, l.partitions) {
  #print("Beginn der calcSurrogateSplit Funktion!!!")

  vec.sur.res <- c()  # leerer Vektor fuer Surrogate Split Endtabelle
  lvl.target <- unique(df.attributes[, v.target])
  target.occur <- c()

  # Relative Anteile der Auspraegungen des Zielattributs berechnen
  for (i in 1:length(lvl.target)) {
    target.occur[i] <- nrow(df.attributes[df.attributes[, v.target] == lvl.target[i], ])
  }

  target.rel.occur <- target.occur / nrow(df.attributes)

  # Data Frame mit mv.attribute und zielattribut anlegen
  # die Spalte mit dem Attribut mit Missing Values wird dann aus df.attributes geloescht
  df.mv <- data.frame(mv.attribute = df.attributes[, v.mvattribute], target.attribute = df.attributes[, v.target])
  # Hinzufuegen der Spalte DistMVattribute
  df.mv <- cbind(df.mv, DistMVattribute = c(0))
  # Attribut mit NA rauswerfen
  df.attributes <- df.attributes[, -which(names(df.attributes) %in% v.mvattribute)]

  # 1. In df.mv werden der DistMVAttribute Spalte L/R zugewiesen
  # :wenn die Auspraegung von mv.attribute in der linken/rechten partition des v.mvattribute vorkommt
  # (hier muesste man evtl auch noch darauf pruefen, ob nur noch eine Auspr. vorhanden ist)
  df.mv[which(df.mv$mv.attribute %in% l.partitions[[v.mvattribute]]$left), ]$DistMVattribute <- 'L'
  df.mv[which(df.mv$mv.attribute %in% l.partitions[[v.mvattribute]]$right), ]$DistMVattribute <- 'R'

  # In df.mv steht jetzt in der Spalte mv.attribute L oder R
  # In target.attribute die zugehoerigen Auspraegungen des Zielattributes

  # Anlegen von Spalten DistTemp und DistBest
  # DistTemp wird im Folgenden bei jedem Schleifendurchlauf ueberschrieben mit dem aktuellen Merkmal
  # DistBest wird immer nur dann ueberschrieben, wenn ein neuer bester Split gefunden wurde
  df.mv <- cbind(df.mv, DistTemp = c(0), DistBest = c(0))

  # Durchschleifen aller Attribute der
  sur.col <- c()  # Namen der Attribute, welche die Surrogate Splits berechnen

  for (i in colnames(df.attributes)) {
    # print(i)
    # Abgesehen vom Zielattribut ..
    if (i == v.target) {
      next()
    } else {
      sur.col <- append(sur.col, i)
      # Bei numerischen Attributen
      if (is.numeric(df.attributes[, i]) || is.integer(df.attributes[, i])) {
        # Numerisches Attribut & die rechte Partition ist NULL
        #                        (Der Fall, wenn nur eine Auspraegung in den verbleibenden Daten)
        if (is.null(l.partitions[[i]]$right)) {

          # hier noch anpassen, falls das nicht geht
          df.mv$DistTemp <- 'L'

        # Numerisches Attribut & aber mit mehr als einer Auspraegung in den verbleibenden Daten
        } else {
          #

          df.mv[which(df.attributes[, i] %in% l.partitions[[i]]$left), ]$DistTemp <- 'L'
          df.mv[which(df.attributes[, i] %in% l.partitions[[i]]$right), ]$DistTemp <- 'R'
        }

      } else {
        # Bei Attributen vom Typ Faktor
        if (is.null(l.partitions[[i]]$right)) {

          df.mv$DistTemp <- 'L'

        } else {

          df.mv[which(df.attributes[, i] %in% l.partitions[[i]]$left), ]$DistTemp <- 'L'
          df.mv[which(df.attributes[, i] %in% l.partitions[[i]]$right), ]$DistTemp <- 'R'
        }
      }
    }

    res <- c()
    vec.sur.res.comp <- c()
    for (i in lvl.target) {
      numerator <- nrow(df.mv[(df.mv$DistMVattribute == df.mv$DistTemp) & (df.mv$target.attribute) == i, ])
      denominator <- nrow(df.mv[(df.mv$target.attribute == i) & (!is.na(df.mv$mv.attribute)), ])
      a <- numerator / denominator
      res <- append(res, a)
    }

    prob.sur <- sum(target.rel.occur * res)
    vec.sur.res <- append(vec.sur.res, prob.sur)
  }

  vec.sur.res.comp <- (1 - vec.sur.res)

  # Data Frame mit (1) Prob des jeweiligen Surrogate Split
  # (2) Angabe ob Komplement :0 oder nicht :1
  # (3) Angabe des jeweiligen Merkmals
  df.sur.prob.mapping0 <- data.frame(prob = vec.sur.res, comp = c(0), attribute = sur.col)
  df.sur.prob.mapping1 <- data.frame(prob = vec.sur.res.comp, comp = c(1), attribute = sur.col)
  df.sur.prob <- rbind(df.sur.prob.mapping0, df.sur.prob.mapping1)

  # Ordnen der Wahrscheinlichkeiten absteigend.
  # Abschneiden der geringen Gegenwahrscheinlichkeiten
  df.sur.prob <- df.sur.prob[order(-df.sur.prob$prob), ]
  df.sur.prob <- df.sur.prob[-(((nrow(df.sur.prob) / 2 ) + 1) : nrow(df.sur.prob)), ]

  # Ermittlung beste Wahrscheinlichkeit aller Attribute und deren Komplemente
  best <- df.sur.prob[df.sur.prob$prob == max(df.sur.prob$prob), ]
  search.attr <- as.character(best$attribute)


  # Suche Rechter und Linker Knoten aus Partitionsliste
  sur.split.partitions <- list()
  for (i in df.sur.prob$attribute) {
    current.comp <- df.sur.prob$comp[which(df.sur.prob$attribute == i)]

    if (current.comp == 0) {
      sur.split.partitions[[i]]$left <- l.partitions[[i]]$left
      sur.split.partitions[[i]]$right <- l.partitions[[i]]$right

    } else if (current.comp == 1) {
      sur.split.partitions[[i]]$left <- l.partitions[[i]]$right
      sur.split.partitions[[i]]$right <- l.partitions[[i]]$left
    }
  }


  result.obj <- new("inttree.surrogate.split", df.best.splits = df.sur.prob,
                                       partitions.best.splits = sur.split.partitions)

  return(result.obj)
}
