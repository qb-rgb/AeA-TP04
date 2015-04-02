/**
 * Représente un graphe valué
 *
 * @constructor construit un nouveau graphe valué
 * @param vertexes noeuds du graphe
 * @param edges arêtes du graphe
 *
 * @author Quentin Baert
 */
class Graph[T](val vertexes: Set[Vertex[T]], val edges: Set[Edge[T]]) {

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
  def getVertexEdges(vertex: Vertex[T]): Set[Edge[T]] =
    this.edges filter (e => (e.v1 == vertex) || (e.v2 == vertex))

  /**
   * Donne toutes les arêtes du graphe reliées à un sommet donné
   *
   * @param vertexId identifiant du noeud dont on souhaite récupérer les arêtes
   * @return arêtes du graphe reliées au sommet d'identifiant vertexId
   */
  def getVertexEdges(vertexId: T): Set[Edge[T]] =
    this getVertexEdges this.vertexesId(vertexId)

  /**
   * Donne les succésseurs d'un sommet du graphe
   *
   * @param vertex sommet dont on souhaite récupérer les succésseurs
   * @return ensemble des succéseurs de vertex
   */
  def getVertexNeighbours(vertex: Vertex[T]): Set[Vertex[T]] =
    (this getVertexEdges vertex) map (e => e other vertex)

  /**
   * Détermine si le graphe contient un cycle en partant d'un sommet donné ou non
   *
   * @param vertex sommet à partir duquel trouver un cycle
   * @return true si le graphe contient cycle en partant de vertex, false sinon
   */
  def containsCycleFrom(vertex: Vertex[T]): Boolean = {
    /*
     * toCheck : ensemble de sommets qu'il reste à parcourir
     * tagged  : ensemble de sommets déjà marqués
     * father  : map donnant le sommet père d'un autre sommet (dans le parcours)
     */
    def detectCycleFrom(
      toCheck: List[Vertex[T]],
      tagged: Set[Vertex[T]],
      father: Map[Vertex[T], Vertex[T]]): Boolean = {

      // Si tous les sommets ont été parcourus : pas de cycle
      if (toCheck.isEmpty)
        false
      else {
        // Sommet à traiter
        val vertex = toCheck.head

        // Si ce sommet est déjà marqué : cycle détecté
        if (tagged contains vertex)
          true
        else {
          val neighbours = this getVertexNeighbours vertex
          // Voisins du sommet courrant
          val finalNeighbours =
            // Le père du sommet courant n'est pas pris en compte
            if (father contains vertex)
              neighbours - father(vertex)
            else
              neighbours

          // Si un des voisins du sommet courant est déjà à traiter : cycle détecté
          if (finalNeighbours exists (v => toCheck contains v))
            true
          else
            /*
             * Appel récursif avec :
             *    les voisins du sommets courant ajoutés aux sommets à traiter
             *    le sommet courant ajouté aux sommets marqués
             *    la table des père mise à jour
             */
            detectCycleFrom(
              finalNeighbours.toList ++ toCheck.tail,
              tagged + vertex,
              father ++ (finalNeighbours map (v => v -> vertex))
            )
        }
      }
    }

    val initialFather = ((this getVertexNeighbours vertex) map (v => (v -> vertex))).toMap

    detectCycleFrom(List(vertex), Set(), initialFather)
  }

  /**
   * Détermine si le graphe contient un cycle
   *
   * @return true si le graphe contient un cycle, false sinon
   */
  def containsCycle: Boolean = {
    def testWithAllVertexes(vertexes: Set[Vertex[T]]): Boolean =
      if (vertexes.isEmpty)
        false
      else if (this containsCycleFrom vertexes.head)
        true
      else
        testWithAllVertexes(vertexes.tail)

    testWithAllVertexes(this.vertexes)
  }

  /**
   * Retourne un nouveau graphe avec un noeud supplémentaire
   *
   * @param vertex noeud à ajouter au graphe
   * @return nouveau graphe avec un noeud supplémentaire
   */
  def addVertex(vertex: Vertex[T]): Graph[T] =
    new Graph[T](this.vertexes + vertex, this.edges)

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param edge arête à ajouter au graphe
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdge(edge: Edge[T]): Graph[T] =
    if ((this.vertexes contains edge.v1) &&
        (this.vertexes contains edge.v2))
      new Graph[T](this.vertexes, this.edges + edge)
    else
      throw new Error("Graph.addEdge : Impossible d'ajouter l'arête")

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param v1 première extrémitée de l'arête
   * @param v2 seconde extrémitée de l'arête
   * @param weight poids de l'arête
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdgeBetween(v1: Vertex[T], v2: Vertex[T], weight: Int): Graph[T] =
    this addEdge (new Edge(v1, v2, weight))

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param v1 première extrémitée de l'arête
   * @param v2 seconde extrémitée de l'arête
   * @param weight poids de l'arête
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdgeBetween(v1Id: T, v2Id: T, weight: Int): Graph[T] = {
    val edge = new Edge(this.vertexesId(v1Id), this.vertexesId(v2Id), weight)
    this addEdge edge
  }

  /**
   * Donne l'arbre couvrant minimum du graphe grâce à l'algorithme de Prim
   *
   * @return arbre couvrant minimum du graphe
   */
  def getPrimMST: Graph[T] = {
    /*
     * v  : Ensemble des points marqués
     * e  : Ensemble des arêtes sortante de l'ensemble de points marqués
     * fr : Ensemble des arêtes à garder pour l'arbre couvrant minimum
     */
    def prim(v: Set[Vertex[T]], e: Set[Edge[T]], fe: Set[Edge[T]]): Graph[T] =
      if (this.vertexes forall (v contains _))
        new Graph(v, fe)
      else {
        // Arête avec le poids minimum
        val minEdge = e minBy (x => x.weight)
        // Extrémité de l'arête qui est déjà marqué
        val taggedVertex = if (v contains minEdge.v1) minEdge.v1 else minEdge.v2
        // Sommet à ajouter aux sommets marqués
        val vertex = minEdge other taggedVertex
        // Arêtes à ajouter à e
        val edgesToAdd =
          (this getVertexEdges vertex) filterNot (e => v contains (e other vertex))
        // Nouvel ensemble d'arêtes à considérer
        val newE = (e - minEdge) ++ edgesToAdd

        prim(v + vertex, newE, fe + minEdge)
      }

    val vertex = this.vertexes.head
    val vertexEdges = this getVertexEdges vertex

    prim(Set(vertex), vertexEdges, Set())
  }

  override def toString: String =
    this.edges mkString "\n"

}
