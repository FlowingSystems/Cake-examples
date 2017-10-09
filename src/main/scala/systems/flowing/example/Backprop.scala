package systems.flowing.example

import systems.flowing.cake._
import store._
import io._

trait Error {
    this: State =>
    var error: Double = 0.0
}

trait Target extends Error {
    this: Output =>
    var target: Double = 0.0
}

trait Bias extends Input {
    state = 1
}

object Backprop extends App {
    val learningRate = 0.005

val bp = new Nodes[State with Order]
        with Hierarchy
        with Directed with AdjacencyList
        with Signal
        with Feedback
        with Flow
        with IO {

    val nodes =
        Hierarchy(2, 2, 1).nodes(
            () => new {val order = 0} with Input with Order,
            () => new {val order = 1} with State with Order with Error,
            () => new {val order = 2} with Output with Order with Target
        ) ++
        Nodes.nodes(
            ((() => new {val order = 0} with Bias with Order), 1),
            ((() => new {val order = 1} with Bias with Order), 1)
        )

        def sig(x: Double) = 1 / (1 + Math.pow(Math.E, -x))
        def dsig(x: Double) = x * (1 - x)

        def propagate(i: Int) = nodes(i).state =
            sig(this(i) map { case (i: Int, weight: Double) => nodes(i).state * weight } sum)

        def backpropagate(i: Int) = 
            nodes(i).asInstanceOf[Error].error = nodes(i) match {
                case o: Output with Target => (o.state - o.target)

                case s: State with Error => {
                    val weights = from(i)

                    (weights map { edge =>
                        weights(edge._1) * nodes(edge._1).asInstanceOf[Error].error * dsig(nodes(edge._1).state)
                    } sum)
                }
            }

        def updateWeight(i: Int) = this(i) = this(i) map { case (from: Int, weight: Double) =>
            from -> (weight - learningRate * nodes(i).asInstanceOf[Error].error * dsig(nodes(i).state) * nodes(from).state)
        }

        def input(channels: Map[String, Seq[Double]]) = {
            (channels("inputValues") zip of[Input]).
            foreach { case (x: Double, in: (Int, Input)) => in._2.state = x }

            (channels("targetValues") zip of[Target]).
            foreach { case (x: Double, in: (Int, Target)) => in._2.target = x }

            signal(propagate)
            feedback(backpropagate)
            flow(updateWeight)
        }

        def output = Map(
            "inputs" -> (nodes collect { case n: Input => n.state }),
            "outputs" -> (nodes collect { case n: Output => n.state }),
            "errors" -> (nodes collect { case n: Error => n.error })
        )
    }

    // initialize weights
    bp <= Hierarchy(2, 2, 1).random(-1, 1)

    // initialize weights from bias nodes
    bp.of[Bias with Order].
    map(in => (in._1 -> in._2.order)).
    foreach { case (i: Int, inRow: Int) =>
        bp.order(inRow+1).
        filter(!bp.nodes(_).isInstanceOf[Bias]).
        foreach { j => bp(i, j) = Some(Util.random(-1, 1)) }
    }

    val inputs = List(List(0.0, 0.0), List(0.0, 1.0), List(1.0, 0.0), List(1.0, 1.0))
    val or = List(0.0, 1.0, 1.0, 1.0) map (x=>List(x))
    val xor = List(0.0, 1.0, 1.0, 0.0) map (x=>List(x))
    def pickRandom(i: Int) = scala.util.Random.nextInt(inputs.length)
    val training = Revolve("inputValues" -> inputs,
                           "targetValues" -> xor)(pickRandom) >>
                   bp >>
                   Print(10000)

    for(i <- 1 to 1000000) training.output
}