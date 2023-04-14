package org.verifx.Compiler

import org.verifx.Utilities.Interpolators._
import meta._

object BuiltInClasses {

  private val tupleClass = "class Tuple[A, B](fst: A, snd: B)".parse[Stat].get.asInstanceOf[Defn.Class]

  /**
   * Implementation of a generic Vector data type.
   */
  private val vectorClass =
    """class Vector[V](size: Int = 0, positions: Map[Int, V] = new Map[Int, V]) {
         def wellFormed(): Boolean = {
           this.size >= 0 &&
           forall(i: Int) {
             if (i >= 0 && i < this.size)
               this.positions.contains(i)
             else
               !this.positions.contains(i)
           }
         }

         def write(idx: Int, elem: V) = {
           new Vector(this.size, this.positions.add(idx, elem))
         }

         // Appends the element at the end of the vector,
         // thereby, effectively growing the array with 1 extra element.
         def append(elem: V) = {
           new Vector(this.size + 1, this.positions.add(this.size, elem))
         }

         def get(idx: Int) = {
           this.positions.get(idx)
         }

         private def min(x: Int, y: Int) = if (x < y) x else y

         def zip[W](that: Vector[W]): Vector[Tuple[V, W]] = {
           new Vector(this.min(this.size, that.size), this.positions.zip(that.positions))
         }

         def map[W](f: V => W): Vector[W] = {
           new Vector(this.size, this.positions.mapValues(f))
         }

         def forall(f: V => Boolean) = this.positions.forall((idx: Int, value: V) => f(value))
         def exists(f: V => Boolean) = this.positions.exists((idx: Int, value: V) => f(value))
       }""".parse[Stat].get.asInstanceOf[Defn.Class]

  private val listClass =
    """class LList[V](size: Int = 0, elements: Map[Int, V] = new Map[Int, V]()) {
         def wellFormed(): Boolean = {
           this.size >= 0 &&
           forall(i: Int) {
             if (i >= 0 && i < this.size)
               this.elements.contains(i)
             else
               !this.elements.contains(i)
           }
         }

         def get(pos: Int): V = this.elements.get(pos)

         def insert(pos: Int, x: V): LList[V] = {
           if (pos >= 0 && pos < this.size) {
             // - elements with idx < pos --> unchanged
             // - element with idx == pos -> x
             // - elements with idx > pos -> oldList.get(idx-1)
             // - add element at size --> oldList.get(size-1)
             val rightShiftedElems =
               this
                 .elements
                 .map((idx: Int, v: V) => {
                   if (idx < pos)
                     v
                   else {
                     if (idx == pos)
                       x
                     else {
                       // idx > pos
                       this.elements.get(idx - 1)
                     }
                   }
                 })

             // The last element got lost when shifting to the right
             // so append the old last element at the end again
             val newElems = rightShiftedElems.add(this.size, this.elements.get(this.size - 1))
             new LList(this.size + 1, newElems)
           }
           else {
             if (pos == this.size) {
               // append
               new LList(
                 this.size + 1,
                 this.elements.add(this.size, x))
             }
             else {
               this
             }
           }
         }

         def delete(pos: Int): LList[V] = {
           if (pos >= 0 && pos < this.size) {
             // - elements with idx < pos --> unchanged
             // - elements with idx >= pos && idx < size-1 --> oldList.get(idx + 1)
             // - delete element at oldSize - 1
             val leftShiftedElems =
               this
                 .elements
                 .map((idx: Int, v: V) => {
                   if (idx < pos)
                     v
                   else {
                     if (idx >= pos && idx < (this.size - 1))
                       this.elements.get(idx + 1)
                     else
                       v
                   }
                 })

             val newElems = leftShiftedElems.remove(this.size - 1)
             new LList(this.size - 1, newElems)
           }
           else {
             this
           }
         }

         private def min(x: Int, y: Int) = if (x < y) x else y

         def zip[W](that: LList[W]): LList[Tuple[V, W]] = {
           new LList(this.min(this.size, that.size), this.elements.zip(that.elements))
         }

         def map[W](f: V => W): LList[W] = {
           new LList(this.size, this.elements.mapValues(f))
         }

         def forall(f: V => Boolean) = this.elements.forall((idx: Int, value: V) => f(value))
         def exists(f: V => Boolean) = this.elements.exists((idx: Int, value: V) => f(value))
       }""".parse[Stat].get.asInstanceOf[Defn.Class]

  /**
   * Meta implementation of a generic Set data type.
   */
  private val setClass =
    ind"""class Set[V]() {
            def add(elem: V) = Dummy[Set[V]]
            def remove(elem: V) = Dummy[Set[V]]
            def contains(elem: V) = Dummy[Boolean]

            def isEmpty() = Dummy[Boolean]
            def nonEmpty() = Dummy[Boolean]

            def union(that: Set[V]) = Dummy[Set[V]]
            def diff(that: Set[V]) = Dummy[Set[V]]
            def intersect(that: Set[V]) = Dummy[Set[V]]
            def subsetOf(that: Set[V]) = Dummy[Boolean]

            def map[W](f: V => W) = Dummy[Set[W]]
            def filter(f: V => Boolean) = Dummy[Set[V]]
            def forall(f: V => Boolean) = Dummy[Boolean]
            def exists(f: V => Boolean) = Dummy[Boolean]
          }""".parse[Stat].get.asInstanceOf[Defn.Class]

  /**
   * Meta implementation of a generic Map data type.
   */
  private val mapClass =
    ind"""class Map[K, V]() {
            def add(key: K, value: V) = Dummy[Map[K, V]]
            def remove(key: K) = Dummy[Map[K, V]]
            def contains(key: K) = Dummy[Boolean]
            def get(key: K) = Dummy[V]
            def getOrElse(key: K, default: V) = Dummy[V]

            def map[W](f: (K, V) => W) = Dummy[Map[K, W]]
            def mapValues[W](f: V => W) = Dummy[Map[K, W]]
            def filter(f: (K, V) => Boolean) = Dummy[Map[K, V]]
            def zip[W](m: Map[K, W]) = Dummy[Map[K, Tuple[V, W]]]
            def combine(m: Map[K, V], f: (V, V) => V) = Dummy[Map[K, V]]
            def keys() = Dummy[Set[K]]
            def values() = Dummy[Set[V]]
            def bijective() = Dummy[Boolean]
            def toSet() = Dummy[Set[Tuple[K, V]]]

            def forall(f: (K, V) => Boolean) = Dummy[Boolean]
            def exists(f: (K, V) => Boolean) = Dummy[Boolean]
          }""".parse[Stat].get.asInstanceOf[Defn.Class]

  val builtInKinds: List[Defn.Class] = List(tupleClass, setClass, mapClass, vectorClass, listClass) // Data types that are already present in the target language
  val builtInClassNames: Set[String] = builtInKinds.map(_.name.value).toSet
  val builtInClassNamesZ3: Set[String] = builtInClassNames -- Set(tupleClass, vectorClass, listClass).map(_.name.value) // Data types that are present in the target language, except in Z3, i.e. these data types need to be compiled to Z3
}
