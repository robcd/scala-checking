package org.lafros.scala

sealed abstract class Checked[+O, +K] {
  def okay: Boolean
}

final case class Okay[+O, +K](o: O) extends Checked[O, K] {
  def okay = true
}

final case class KayoAsOkay[+O, +K](k: K) extends Checked[O, K] {
  def okay = true
}

final case class Kayo[+O, +K](k: K) extends Checked[O, K] {
  def okay = false
}

final case class OkayAsKayo[+O, +K](o: O) extends Checked[O, K] {
  def okay = false
}
