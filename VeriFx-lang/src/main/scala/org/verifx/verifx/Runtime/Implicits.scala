package org.verifx.verifx.Runtime

/**
 * Extends Scala's `Map` and `Set` data types with VeriFx methods.
 * To this end, the `Implicits` object defines implicit conversions.
 */
object Implicits {
  implicit class MapOps[K, V](m: Map[K, V]) {
    def add(k: K, v: V): Map[K, V] = m + (k -> v)
    def remove(k: K): Map[K, V] = m - k
    def gett(k: K): V = m.get(k).get
    def forall(f: (K, V) => Boolean): Boolean = m.forall(f.tupled)
    def exists(f: (K, V) => Boolean): Boolean = m.exists(f.tupled)

    def keys() = m.keySet
    def values() = m.values.toSet
    def bijective() = m.values.toSet.size == m.size // source & explanation: https://stackoverflow.com/questions/34777220/scala-test-a-map-is-bijective

    def map[W](f: (K, V) => W): Map[K, W] = m map {
      case (key, value) => {
        val newValue = f(key, value)
        (key, newValue)
      }
    }

    def filter(f: (K, V) => Boolean): Map[K, V] = m.filter(f.tupled)

    def mappValues[W](f: V => W): Map[K, W] = m.view.mapValues(f).toMap

    /**
     * Zips the values of common entries of both maps.
     * The resulting map only contains keys that occur in both maps
     * and zipped their values into a tuple.
     */
    def zipp[W](m2: Map[K, W]): Map[K, (V, W)] = {
      (m.keySet intersect m2.keySet).map(k => (k, (m.get(k).get, m2.get(k).get))).toMap
    }

    /**
     * Combines two maps into a new map.
     * If a key occurs in both maps, the values are combined using the provided function.
     * If a key is contained in only one of the maps, the entry is copied as is to the new map.
     * @param m2 Map to combine with
     * @param f Function to combine two values into a new value
     * @return A map containing all keys from `m` and `m2`
     */
    def combine(m2: Map[K, V], f: (V, V) => V): Map[K, V] = {
      (m.keySet ++ m2.keySet).map {
        case k => {
          (m.get(k), m2.get(k)) match {
            case (Some(v1), Some(v2)) => (k, f(v1, v2))
            case (Some(v1), None) => (k, v1)
            case (None, Some(v2)) => (k, v2)
            case (None, None) => throw new InternalError("Key is not present in both maps.")
          }
        }
      }.toMap
    }
  }

  implicit class SetOps[V](s: Set[V]) {
    def add(elem: V) = s + elem
    def remove(elem: V) = s - elem
  }

  implicit class VectorOps[V](v: Vector[V]) {
    def write(idx: Int, value: V): Vector[V] = v.updated(idx, value)
    def append(value: V): Vector[V] = v.appended(value)
    def get(idx: Int) = v(idx)
  }

  implicit class ListOps[V](lst: List[V]) {
    def size = lst.length

    def get(idx: Int): V = lst(idx)

    def insert(idx: Int, x: V) = {
      lst.take(idx) ++ List(x) ++ lst.drop(idx)
    }

    def delete(idx: Int) = {
      lst.take(idx) ++ lst.drop(idx + 1)
    }
  }
}
