package basics

object DataStructures {
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = map
    .groupMap({ case (_, v) => v })({ case (k, _) => k })
    .map({case (k,v) => (v.toSet, k)})
    .toList
}