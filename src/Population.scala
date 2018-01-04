package  genetic
import util.Random

class Population(val populationSize: Int, val chromosomeSize: Int, numExecutors: Int) {
  val pop: Array[Organism] = new Array[Organism](populationSize)
  val mutationRate = 0.015

  /*
  *  Population with organism
  */
  def populate = {
    for (i <-0 to populationSize - 1) {
      val genes = new Array[Int](chromosomeSize)
      for (j <-0 to chromosomeSize - 1) {
        genes(j) = Random.nextInt(numExecutors)
      }
      val organism = new Organism(genes)
      pop(i) = organism
    }
  }

  /**
  *  Return the population size
  */
  def size: Int = {
    pop.length
  }

  /**
  * Add an organism to a particular location in a population
  */
  def addOrganism(index: Int, organism: Organism) = {
    pop(index) = organism
  }

  /*
  * Evolve the population by crossover and mutation
  * @param elitist It true, the fittest organism pass to the next generation
  * @param evaluator The evaluator to use
  */
  def evolve(elitist: Boolean, evaluator: Evaluator): Population = {
    val nextGeneration = new Population(pop.size, chromosomeSize, numExecutors)
    var offset = 0
    if (elitist) {
      val eliteOrganism = evaluator.fittest(this)
      nextGeneration.addOrganism(offset, mutate(eliteOrganism))
      offset += 1
    }

    for (index <- offset to pop.size - 1) {
      val parent1: Organism = select(evaluator)
      val parent2: Organism = select(evaluator)
      val child: Organism = crossover(parent1, parent2)
      nextGeneration.addOrganism(index, mutate(child))
    }
    nextGeneration
  }

  /*
  * Mutate an organism with a random rate of 0.015
  */
  def mutate(organism: Organism): Organism = {
    firstMutationOperator(organism)
  }

  /*
  * Random selects two points on the selected chromosome,then swapping the genes
  */
  def firstMutationOperator(organism: Organism): Organism = {
    val length = organism.genes.length
    val c: Array[Int] = organism.genes

    for (index <- 0 to c.length - 1) {
      if (Math.random() <= mutationRate) {
        val pos1 = Random.nextInt(length)
        val pos2 = Random.nextInt(length)
        val tmp = c(pos1)
        c(pos1) = c(pos2)
        c(pos2) = tmp
      }
    }
    new Organism(c)
  }

  /**
  * Create child organism from two parents
  */
  def crossover(parent1: Organism, parent2: Organism): Organism = {
    val length = parent1.genes.length
    val point = Random.nextInt(length)

    val chromosome = new Array[Int](length)

    var index = 0
    val ratio = Math.random()
    while (index < point) {
      if (ratio <= 0.5) {
        chromosome(index) = parent1.genes(index)
      } else {
        chromosome(index) = parent2.genes(index)
      }
      index += 1
    }
    while (index < length) {
      if (ratio <= 0.5) {
        chromosome(index) = parent2.genes(index)
      } else {
        chromosome(index) = parent1.genes(index)
      }
      index += 1
    }
    new Organism(chromosome)
  }

  /**
  * Select an organism from the population using stochastic universal samppling
  */
  def select(evaluator: Evaluator): Organism = {
    val numberOfRounds = 10
    val tournament = new Population(numberOfRounds, chromosomeSize, numExecutors)
    for (i <-0 to numberOfRounds - 1) {
      val randomOrganism = pop(Random.nextInt(populationSize))
      tournament.addOrganism(i, randomOrganism)
    }
    evaluator.fittest(tournament)
  }
}
