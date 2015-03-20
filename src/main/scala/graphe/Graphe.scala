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

  val vertexesId: Map[T, Vertex[T]] =
    (this.vertexes map (x => x.id -> x)).toMap

  //////////////
  // MÉTHODES //
  //////////////

  def addVertex(vertex: Vertex[T]): Graphe[T] =
    new Graphe[T](this.vertexes + vertex, this.edges)

  def addEdge(edge: Edge[T]): Graphe[T] =
    if ((this.vertexes contains edge.v1) &&
        (this.vertexes contains edge.v2))
      new Graphe[T](this.vertexes, this.edges + edge)
    else
      throw new Error("Graphe.addEdge : Impossible d'ajouter l'arête")

  def getEdgesFromVertex(vertex: Vertex[T]): Set[Edge[T]] =
    this.edges filter (e => (e.v1 == vertex) || (e.v2 == vertex))

  def getEdgesFromVertexId(id: T): Set[Edge[T]] =
    this getEdgesFromVertex this.vertexesId(id)

}
