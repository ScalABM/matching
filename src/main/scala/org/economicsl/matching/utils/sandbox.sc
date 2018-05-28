import scala.collection.immutable.Queue


def depthFirstSearch[A](graph: Map[A, Set[A]], from: A): List[A] = {

  @annotation.tailrec
  def loop(unvisited: List[A], visited: Set[A], result: List[A]): List[A] = unvisited match {
    case Nil =>
      result
    case h :: t if visited.contains(h) =>
      loop(t, visited, result)
    case h :: t => graph.get(h) match {
      case None =>
        loop(t, visited + h, h :: result)
      case Some(successors) =>
        loop(successors.foldRight(unvisited)(_ :: _), visited + h, h :: result)
    }
  }

  loop(List(from), Set.empty, List.empty).reverse

}

def breadthFirstSearch[A](graph: Map[A, Set[A]], from: A): List[A] = {

  @annotation.tailrec
  def loop(unvisited: Queue[A], visited: Set[A], result: List[A]): List[A] = unvisited.headOption match {
    case None =>
      result
    case Some(h) if visited.contains(h) =>
      val (_, t) = unvisited.dequeue
      loop(t, visited, result)
    case Some(h) => graph.get(h) match {
      case None =>
        val (_, t) = unvisited.dequeue
        loop(t, visited + h, h :: result)
      case Some(successors) =>
        loop(successors.foldRight(unvisited)((a, q) =>  q.enqueue(a)), visited + h, h :: result)
    }
  }

  loop(Queue(from), Set.empty, List.empty).reverse

}

def stronglyConnectedComponent[A](graph: Map[A, Set[A]], from: A): Set[Set[A]] = {

  case class Visited(index: Int = 0, lowLink: Int = 0)

  @annotation.tailrec
  def loop(stack: List[A], visited: Map[A, Visited], index: Int, stacked: Map[A, Boolean], result: Set[Set[A]]): Set[Set[A]] = stack match {
    case Nil =>
      result
    case h :: _ => graph.get(h) match {
      case None => // h has only incoming edges and should end up in its own connected component!
        loop(stack, visited, index, stacked, result)
      case Some(successors) =>
        val (updatedStack, updatedStacked) = successors.foldLeft((stack, stacked)){
          case ((as, abs), a) => (a :: as, abs.updated(a, true))
        }
        val updatedVisited = successors.foldLeft(visited + (h -> Visited())) { case (avs, a) =>
          (visited(h), avs(a)) match {
            case (v, w) if updatedStacked(a) && (w.lowLink < v.lowLink) => avs.updated(h, v.copy(lowLink = w.lowLink))
            case _ => avs
          }
        }
        // if h is a root node then generate a connected component
        if (updatedVisited(h).lowLink == updatedVisited(h).index) {

          @annotation.tailrec
          def pop(s: List[A], nodes: Set[A]): (List[A], Set[A]) = s match {
            case Nil => (s, nodes)
            case a :: as =>
              if (a == h) (as, nodes + a) else pop(as, nodes + a)
          }

          // If v is a root node, pop the stack and generate an SCC
          val (remainingStack, component) = pop(stack, Set.empty)
          val updatedStacked = component.foldLeft(stacked)((s, w) => s.updated(w, false))
          loop(remainingStack, visited, index, updatedStacked, result + component)
        } else {
          loop(updatedStack, updatedVisited, index + 1, updatedStacked, result)
        }
    }
  }

  val visitedWithDefault = Map.empty[A, Visited].withDefaultValue(Visited())
  val stackedWithDefault = Map.empty[A, Boolean].withDefaultValue(false)
  loop(List(from), visitedWithDefault, 0, stackedWithDefault, Set.empty)

}


val g: Map[Int, Set[Int]] = Map(1 -> Set(2, 3, 4), 2 -> Set(5, 6), 3 -> Set(7), 4 -> Set())
depthFirstSearch(g, 1)
breadthFirstSearch(g, 1)

val c: Map[Int, Set[Int]] = Map(1 -> Set(2), 2 -> Set(3), 3 -> Set(4), 4 -> Set(1, 5), 5-> Set(6), 6 -> Set(7), 7 -> Set(8), 8 -> Set(5))
stronglyConnectedComponent(c, 3)