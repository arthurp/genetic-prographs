package org.singingwizard.genetics.prographs.evolution

import org.singingwizard.genetics.prographs.Graph

case class Genotype(program: Graph) {
}

case class Population(pool: Set[Genotype]) {
}

