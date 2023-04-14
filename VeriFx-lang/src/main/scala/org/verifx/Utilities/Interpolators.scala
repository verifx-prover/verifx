package org.verifx.Utilities

// Taken from: https://stackoverflow.com/questions/11404399/indentation-preserving-string-interpolation-in-scala
object Interpolators {
  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  implicit class IndentHelper(val sc: StringContext) extends AnyVal {
    import sc._

    def process = StringContext.treatEscapes _

    def ind(args: Any*): String = {
      checkLengths(args)
      parts.zipAll(args, "", "").foldLeft("") {
        case (a, (part, arg)) =>
          val processed = process(part)

          val prefix = processed.split("\n").last match {
            case r"""([\s|]+)$d.*""" => d
            case _                   => ""
          }

          val argLn = arg.toString
            .split("\n")

          val len = argLn.length

          // Todo: Fix newline bugs
          val indented = argLn.zipWithIndex.map {
            case (s, i) =>
              val res = if (i < 1) { s } else { prefix + s }
              if (i == len - 1) { res } else { res + "\n" }
          }.mkString

          a + processed + indented
      }
    }
  }
}
