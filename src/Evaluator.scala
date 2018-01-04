package  genetic

class Evaluator(val tasksSize: Array[Long], val numExecutors: Int) {

  /*
  * Return the fittest organism in a population
  */
  def fittest(population: Population): Organism = {
    var o: Organism = population.pop(0)
    for (i <- 1 to population.size - 1) {


      val nextOrganism = population.pop(i)
      if (fitness(nextOrganism) > fitness(o)) {
        o = population.pop(i)
      }
    }
    o
  }

  /**
  * Caculate an organism's fitness
  */
  def fitness(organism: Organism): Double = {
    val loads: Array[Long] = new Array[Long](numExecutors)
    for (i <-0 to organism.genes.length - 1) {
      val gene = organism.genes(i)
      loads(gene) += tasksSize(i)
    }

    val avg: Double = loads.sum.toDouble / numExecutors
    var maxspan: Long = 0L
    for (load <- loads) {
      if (load > maxspan) {
        maxspan = load
      }
    }

    avg / maxspan.toDouble
  }
}
