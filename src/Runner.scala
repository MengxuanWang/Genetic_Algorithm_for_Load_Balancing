package genetic

trait Evolver {
  def run(evaluator: Evaluator, population: Population): Organism = {
    def run(population: Population, generation: Int): Organism = {
      val fittest = evaluator.fittest(population)
      val fitness = evaluator.fitness(fittest)

      println(s"generation: $generation, fitness: $fitness")
     // println(fittest.toString)

      if (generation >= 20) {
        fittest
      } else {
        run(population.evolve(true, evaluator), generation + 1)
      }
    }
    run(population, 1)
  }
}

object Runner extends App with Evolver {
  val tasksSize: Array[Long] = Array(20, 20, 15, 15, 15, 15, 30, 30, 30,
    40, 40, 40, 40, 40, 40, 50, 50, 50, 50, 23, 23, 23, 47, 47, 69, 69, 69, 69, 69, 120, 120, 140,77, 78, 78, 77)
  val numExecutors = 8
  val populationSize = 80
  val chromosomeSize = tasksSize.length
  val evaluator = new Evaluator(tasksSize, numExecutors)
  val population = new Population(populationSize, chromosomeSize, numExecutors)
  population.populate

  val solution = run(evaluator, population)
  val loads: Array[Long] = new Array[Long](numExecutors)
  for (i <- 0 to chromosomeSize - 1) {
    val gene = solution.genes(i)
    loads(gene) += tasksSize(i)
  }

  for (i <- 0 to numExecutors - 1) {
    val load = loads(i)
    println(s"executor $i: $load")
  }
}
