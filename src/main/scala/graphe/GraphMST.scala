object GraphMST {

  def main(args: Array[String]): Unit = {
    val path = args(0)

    val graph = GraphBuilder.buildGraph(path)

    println("===== GRAPHE =====")
    println(graph)
    println()

    println("===== PRIM =====")
    println(graph.getPrimMST)
    println()

    println("===== KRUSKAL =====")
    println(graph.getKruskalMST)
    println()
  }

}
