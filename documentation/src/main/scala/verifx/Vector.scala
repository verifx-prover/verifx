package verifx

class Vector[V] {
  val size: Int = ???

  /**
   * @param idx Index in the vector between 0 and [[size]] - 1
   * @param elem Value to write at the given index.
   * @return A modified vector.
   */
  def write(idx: Int, elem: V): Vector[V] = ???

  /**
   * Appends the element at the end of the vector.
   * Thereby, effectively growing the array with an extra element.
   * @param elem Element to append.
   * @return The extended vector.
   */
  def append(elem: V): Vector[V] = ???

  /**
   * @param idx Index in the vector between 0 and [[size]] - 1
   * @return The value at the given index in this vector.
   */
  def get(idx: Int): V = ???

  /**
   * @param that Other vector to zip with.
   * @tparam W Type of elements contained by the other vector.
   * @return A vector combining the elements that are in both vectors.
   */
  def zip[W](that: Vector[W]): Vector[Tuple[V, W]] = ???

  /**
   * @param f Function to map over the vector.
   * @return Vector of mapped elements.
   */
  def map[W](f: V => W): Vector[W] = ???

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
