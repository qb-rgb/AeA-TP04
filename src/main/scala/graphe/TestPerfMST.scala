/**
 * Affiche les moyennes de temps d'éxécution des algorithmes de MST implémentés
 *
 * @author Quentin Baert
 */
object TestPerfMST {

  // Éxécute une coloration en fonction d'une chaîne passée en paramètre
  private def executeColoration(graph: Graph, mstName: String): Long =
    mstName match {
      case "kruskal" => {
        val start = System.currentTimeMillis
        graph.getKruskalMST
        System.currentTimeMillis - start
      }
      case _ => {
        val start = System.currentTimeMillis
        graph.getPrimMST
        System.currentTimeMillis - start
      }
    }

  // Génère x graphes aléatoirement
  private def generateXGraphs(x: Int, n: Int, proba: Float): List[Graph] =
    (
      for (i <- 1 to x)
        yield RandomGraphGenerator.generateErdosRenyiGraph(n, proba)
    ).toList

  /*
   * Affiche la moyenne du nombre de couleurs trouvés par un algo ainsi que son
   * temps d'éxécution
   */
  private def printAverage(graphs: List[Graph], title: String, id: String): Unit = {
    val l = graphs.size

    println("----- " + title + " -----")

    val resultList = for (g <- graphs) yield executeColoration(g, id)
    val time = resultList.foldLeft(0 : Long)((acc, e) => acc + e)

    println("Temps moyen : " + time / l + " ms")
  }

  def main(args: Array[String]): Unit = {
    // Nombre de graphes sur lesquels faire les moyennes
    val graphNb = args(0).toInt
    // Nombre de sommets par graphe
    val graphVertices = args(1).toInt
    // Probabilité avec lesquelles seront générées les graphes
    val probas = List(0.1f, 0.3f, 0.5f, 0.7f, 0.9f)

    // Affichage des moyennes
    for (p <- probas) {
      val graphs = generateXGraphs(graphNb, graphVertices, p)
      println("===== PROBA : " + p + " =====")
      printAverage(graphs, "PRIM", "prim")
      printAverage(graphs, "KRUSKAL", "kruskal")
      println()
    }
  }

}
