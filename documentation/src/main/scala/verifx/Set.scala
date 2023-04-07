package verifx

class Set[V] {
  /**
   * @param elem Value to add to the set.
   * @return The same set but extended with the given value.
   */
  def add(elem: V): Set[V] = ???

  /**
   * @param elem Value to remove from the set.
   * @return The same set but without the given value.
   */
  def remove(elem: V): Set[V] = ???

  /**
   * @param elem Some value.
   * @return True if the given value is in the set. False, otherwise.
   */
  def contains(elem: V): Boolean = ???

  /**
   * @return True if the set is empty. False, otherwise.
   */
  def isEmpty(): Boolean = ???

  /**
   * @return True if the set is not empty. False, otherwise.
   */
  def nonEmpty(): Boolean = ???

  /**
   * @param that A set of the same type.
   * @return The union of this set and the provided set.
   */
  def union(that: Set[V]): Set[V] = ???

  /**
   * @param that A set of the same type.
   * @return The set difference of this set and the provided set.
   */
  def diff(that: Set[V]): Set[V] = ???

  /**
   * @param that A set of the same type.
   * @return The intersection of this set and the provided set.
   */
  def intersect(that: Set[V]): Set[V] = ???

  /**
   * @param that A set of the same type.
   * @return True if this set is a subset of `that` set, false otherwise.
   */
  def subsetOf(that: Set[V]): Boolean = ???

  /**
   * @param f A function to map over every element of the set.
   * @return The set that results from applying the given function on every element.
   */
  def map[W](f: V => W): Set[W] = ???

  /**
   * @param f A predicate to filter elements.
   * @return A set containing only the elements that fulfill the given predicate.
   */
  def filter(f: V => Boolean): Set[V] = ???

  /**
   * @param f Predicate to check.
   * @return True if the predicate holds for all elements. False otherwise.
   */
  def forall(f: V => Boolean): Boolean = ???

  /**
   * @param f Predicate to check.
   * @return True if the predicate holds for at least one element. False otherwise.
   */
  def exists(f: V => Boolean): Boolean = ???
}
