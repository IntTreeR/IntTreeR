# Abh.Attribut, Zielattribut, linkePartition, rechtePartition
calcImprovement <- function(v.Attribute, v.Target, left, right) {
  # Alle Werte aus Beiden als Parameter gegebene Vektoren
  # werden in zwei knoten geteilt (child.1, child.2)
  # die Gesamtheit aller Werte der Vektoren bildet somit
  # den Parent der beiden children
  v.Attribute <- as.factor(v.Attribute)
  v.Target <- as.factor(v.Target)

  # Jetzt sind die Spalten zum Suchen zusammengefuehrt
  parent <- data.frame(v.Attribute, v.Target)
  nParent <- nrow(parent)
  lvl.target <- unique(v.Target)

  # Wenn die Daten (ohne NA) nur noch eine Auspraegung des Zielattributs enthalten
  # kann direkt 0 zurueckgeliefert werden
  parent.no.na <- parent[!is.na(v.Attribute), ]
  lvl.parent.no.na <- length(unique(parent.no.na[, 2]))
  rm(parent.no.na)
  if (is.null(right)) {
    return(0)
  }


  # Impurity des Parent Knoten = P(Knoten1) auszaehlen wie viele Auspr1 und Auspr2
  sum.squared.share <- 0
  count.Parent <- c()
  for (i in 1:length(lvl.target)) {
    sum.squared.share <- sum.squared.share + (length(v.Target[v.Target == lvl.target[i]]) / nParent) ^2
    # Relative Anteile der jeweiligen Auspraegungen des Parents
    count.Parent <- append(count.Parent, length(v.Target[v.Target == lvl.target[i]]))
  }
  # Berechnung der Impurity fuer den Parent, basierend auf den relativen Auspraegungen
  impurity.parent <- 1 - sum.squared.share

  # Das sind die Missing Values der Kategorie zum Split
  na.count <- length(parent[is.na(parent$v.Attribute), ])
  #print(paste0("Missing Values v.Attribute: ", na.count))

  # UND HIER DANN DIE IMPURITY DER CHILDREN
  impurity.children <- 0
  count.Left <- c()
  count.Right <- c()
  count.Target <- c()
  for (i in lvl.target) {
    # (ZAEHLER) Zaehlen von allen
    # deren Auspraegungen im Linken/Rechten Knoten und gleichzeitig
    # die Zielvariable der i-ten Auspraegung entspricht
    # JEWEILS ALLE DIE DEN CHILDREN LEFT ODER RIGHT ZUGEORDNET WERDEN
    p.inleft.target <- parent[which(parent$v.Attribute %in% left & parent$v.Target == i), ]
    p.inright.target <- parent[which(parent$v.Attribute %in% right & parent$v.Target == i), ]

    count.Left <- append(count.Left, nrow(p.inleft.target))
    count.Right <- append(count.Right, nrow(p.inright.target))


    # Zaehlen von allen
    # deren Auspraegungen nicht MissingValues (NA) sind und gleichzeitig
    # die Zielvariable der i-ten Auspraegung entsprcht
    p.notna.targetI <- parent[which(!is.na(parent$v.Attribute) & parent$v.Target == i), ]
    count.Target <- append(count.Target, nrow(p.notna.targetI))

  }

  #Impurity
  # 1. schleifen um wahrscheinlichkeiten berechnen
  # Schleife
  target.i.node1 <- c()
  target.i.node2 <- c()

  for (i in 1:length(count.Parent)) {
    # P(Auspraegung und Knoten 1 und 2)
    p.i.node1 <- (count.Parent[i] / sum(count.Parent)) * (count.Right[i] / count.Target[i])
    p.i.node2 <- (count.Parent[i] / sum(count.Parent)) * (count.Left[i] / count.Target[i])

    # P (Auspraegung und Knoten 1 und 2)
    target.i.node1 <- append(target.i.node1, p.i.node1)
    target.i.node2 <- append(target.i.node2, p.i.node2)
  }
  # 2. Vektor aus 1 aufsummieren
  # P(Knoten 1 und 2)
  pnode1 <- sum(target.i.node1)
  pnode2 <- sum(target.i.node2)

  # 3. Jedes Element von Vektor mit Kehrwert aus 2 Multiplizieren
  # Schleife
  bed.target.node1 <- c()
  bed.target.node2 <- c()

  # anstatt den Kehrwert zu mutiplizieren wird dividiert
  bed.target.node1 <- target.i.node1 / pnode1
  bed.target.node2 <- target.i.node2 / pnode2

  # 4. Summe der quadrierten Elemente aus Vektor in 3
  sum.impurity.node1 <- sum((bed.target.node1) ^2)
  sum.impurity.node2 <- sum((bed.target.node2) ^2)


  # Improvement berechnen
  improvement <- impurity.parent - (pnode1 * (1 - sum.impurity.node1)) - (pnode2 * (1 - sum.impurity.node2))
  return(improvement)
}
