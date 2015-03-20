/**
 * Représente un graphe valué
 *
 * @constructor construit un nouveau graphe valué
 * @param vertexes noeuds du graphe
 * @param edges arêtes du graphe
 *
 * @author Quentin Baert
 */
class Graphe[T](val vertexes: Set[Vertex[T]], val edges: Set[Edge[T]]) {

  ///////////////
  // ATTRIBUTS //
  ///////////////

  /**
   * Permet d'accéder aux noeuds du graphe par leur identifiant
   */
  val vertexesId: Map[T, Vertex[T]] =
    (this.vertexes map (x => x.id -> x)).toMap

  //////////////
  // MÉTHODES //
  //////////////

  /**
   * Donne toutes les arêtes du graphe reliées à un sommet donné
   *
   * @param vertex noeud dont on souhaite récupérer les arêtes
   * @return arêtes du graphe reliées au sommet vertex
   */
  def getEdgesFromVertex(vertex: Vertex[T]): Set[Edge[T]] =
    this.edges filter (e => (e.v1 == vertex) || (e.v2 == vertex))

  /**
   * Donne toutes les arêtes du graphe reliées à un sommet donné
   *
   * @param vertexId identifiant du noeud dont on souhaite récupérer les arêtes
   * @return arêtes du graphe reliées au sommet d'identifiant vertexId
   */
  def getEdgesFromVertexId(vertexId: T): Set[Edge[T]] =
    this getEdgesFromVertex this.vertexesId(vertexId)

  /**
   * Retourne un nouveau graphe avec un noeud supplémentaire
   *
   * @param vertex noeud à ajouter au graphe
   * @return nouveau graphe avec un noeud supplémentaire
   */
  def addVertex(vertex: Vertex[T]): Graphe[T] =
    new Graphe[T](this.vertexes + vertex, this.edges)

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param edge arête à ajouter au graphe
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdge(edge: Edge[T]): Graphe[T] =
    if ((this.vertexes contains edge.v1) &&
        (this.vertexes contains edge.v2))
      new Graphe[T](this.vertexes, this.edges + edge)
    else
      throw new Error("Graphe.addEdge : Impossible d'ajouter l'arête")

}
