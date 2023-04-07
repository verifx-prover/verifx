package verifx

class LList[V] {
  val size: Int = ???

  /**
   * Constraints the list to only contain elements that are in range (i.e. between 0 and [[size]] - 1).
   * Only use this in the logical world,
   * e.g. `lst.wellFormed() =>: ...`
   */
  def wellFormed(): Boolean = ???

  /**
   * @param pos Index between 0 and [[size]] - 1.
   * @return The value that is stored at the given index.
   */
  def get(pos: Int): V = ???

  /**
   * @param pos Index at which to insert the given element.
   *            Must be between 0 and [[size]].
   *            Inserting at position 0 is prepending.
   *            Inserting at [[size]] is appending.
   * @param x Value to insert.
   * @return The extended list.
   */
  def insert(pos: Int, x: V): LList[V] = ???

  /**
   * @param pos Index between 0 and [[size]] - 1.
   * @return The modified list.
   */
  def delete(pos: Int): LList[V] = ???

  /**
   * @param that List to zip with.
   * @tparam W Type of the elements in the other list.
   * @return A list combining the elements from both lists in a tuple.
   *         The zipped list has the size of the smallest list.
   */
  def zip[W](that: LList[W]): LList[Tuple[V, W]] = ???

  /**
   * @param f Function to map over the list.
   * @return The list that results from applying the function on every element.
   */
  def map[W](f: V => W): LList[W] = ???

  /**
   * @param f A predicate.
   * @return True if the predicate holds for all elements. False, otherwise.
   */
  def forall(f: V => Boolean): Boolean = ???

  /**
   * @param f A predicate.
   * @return True if the predicate holds for at least one element. False otherwise.
   */
  def exists(f: V => Boolean): Boolean = ???
}
