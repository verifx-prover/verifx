package verifx

class Map[K, V] {
  /**
   * @param key Key to add.
   * @param value Value to associate to the given key.
   * @return A copy of the map with the additional key-value pair.
   */
  def add(key: K, value: V): Map[K, V] = this

  /**
   * @param key Key to remove.
   * @return A copy of the map, without the given key.
   */
  def remove(key: K): Map[K, V] = this

  /**
   * @param key A key.
   * @return True if the key is present in the map. False, otherwise.
   */
  def contains(key: K): Boolean = false

  /**
   * Beware, `get` is unsafe.
   * Only use it if you are sure the key is in the map (e.g. after an `if(map.contains(key))` test).
   * @param key A key that is '''known''' to be in the map.
   * @return The value that is associated to the given key.
   */
  def get(key: K): V = ???

  /**
   * Safe version of `get` providing a default value in case the key does not exist.
   * @param key A key.
   * @param default Value to use if the key is not present.
   * @return The value that is associated to the given key, or the default value if the key is not present in the map.
   */
  def getOrElse(key: K, default: V): V = default

  /**
   * @param f Function to map over the key-value pairs.
   * @return The map that results from applying the given function on every key-value pair.
   */
  def map[W](f: (K, V) => W): Map[K, W] = ???

  /**
   * @param f Function to map over the values.
   * @return The map that results from applying the given function on every value.
   */
  def mapValues[W](f: V => W): Map[K, W] = ???

  /**
   * @param f Predicate to filter key-value pairs.
   * @return A copy of the map containing only the key-value pairs that fulfill the given predicate.
   */
  def filter(f: (K, V) => Boolean): Map[K, V] = ???

  /**
   * @param m A map to zip with.
   * @tparam W Type of the values in the other map.
   * @return A map containing '''only''' the keys that are in both maps.
   *         Their values are bundled in a tuple.
   */
  def zip[W](m: Map[K, W]): Map[K, Tuple[V, W]] = ???

  /**
   * @param m A map to combine with.
   * @param f Function to combine values.
   * @return A map containing '''all''' keys from both maps.
   *         If a key is present in both maps, its values are combined using the provided function.
   *         Otherwise, the value is copied to the resulting map.
   */
  def combine(m: Map[K, V], f: (V, V) => V): Map[K, V] = ???

  /**
   * @return A set containing all the keys of this map.
   */
  def keys(): Set[K] = ???

  /**
   * @return A set containing all the values of this map.
   */
  def values(): Set[V] = ???

  /**
   * @return True if the mapping is bijective (i.e. one-to-one), false otherwise.
   */
  def bijective(): Boolean = ???

  /**
   * @return A set of all the key-value pairs in this map.
   */
  def toSet(): Set[Tuple[K, V]] = ???

  /**
   * @param f Predicate to check.
   * @return True if the predicate holds for all key-value pairs. False otherwise.
   */
  def forall(f: (K, V) => Boolean): Boolean = ???

  /**
   * @param f Predicate to check.
   * @return True if the predicate holds for at least one key-value pair. False otherwise.
   */
  def exists(f: (K, V) => Boolean): Boolean = ???
}
