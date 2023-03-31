package be.vub.verifx.Utilities

import java.util.regex.Matcher

import be.vub.verifx.Compiler.Plugins.Z3CompilerPlugin

import scala.util.matching.Regex

object ExtendedString {
  implicit def stringToExtendedString(s: String) = new ExtendedString(s)
}

/**
 * Extends Scala's String class with methods to replace all occurences of one or more words.
 */
class ExtendedString(s: String) {
  import ExtendedString._

  /**
   * Takes a map of substitutions `mapping` and returns the substituted `in` string.
   */
  def replaceWords(mapping: List[(String, String)]): String =
    mapping.foldLeft(s)((str, mapping) => str.replaceWord(mapping._1, mapping._2))

  /**
   * Replaces every occurence of the `key` word by `value` in `in`.
   */
  def replaceWord(key: String, value: String): String = {
    val word = Regex.quote(key)
    val repl = Matcher.quoteReplacement(value)
    s.replaceAll(s"(?<![^,\\s()])${word}(?![^,\\s()])", repl)
  }

  def normalize(implicit className: String): String =
    Z3CompilerPlugin.z3FunNameToOriginalMethodName(s, className)
}