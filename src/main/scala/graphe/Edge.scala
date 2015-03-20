/**
 * Représente une arête valuée d'un graphe
 *
 * @constructor construit une nouvelle arête
 * @param v1 première extrémité de l'arête
 * @param v2 seconde extrémité de l'arête
 * @param weight poids de l'arête
 *
 * @author Quentin Baert
 */
case class Edge[T](
  val v1: Vertex[T],
  val v2: Vertex[T],
  val weight: Int) {

  override def equals(other: Any): Boolean = other match {
    case that: Edge[_] => (that canEqual this) &&
                          this.v1 == that.v1 &&
                          this.v2 == that.v2 &&
                          this.weight == that.weight
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Edge[_]]

  override def hashCode: Int =
    41 * (41 * (41 + weight) + v1.hashCode ) + v2.hashCode

}

