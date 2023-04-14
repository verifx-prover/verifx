package org.verifx.Utilities

import ExtendedString._

object Utils {
  implicit class RichString(val s: String) {
    def stripSuffixAfter(p: String) = {
      val idx = s.lastIndexOf(p)
      if (idx == -1)
        s
      else
        s.substring(0, idx)
    }
  }
}
