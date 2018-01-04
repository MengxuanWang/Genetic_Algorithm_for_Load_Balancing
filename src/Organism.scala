package  genetic

class Organism(val chromosome: Array[Int]){
  def genes = chromosome

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    for (gene <- chromosome) {
      sb.append(gene + " ")
    }
    sb.toString()
  }
}
