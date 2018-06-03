package tel.schich.puzzles

import java.lang.Long.bitCount

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.io.Source

trait BitMasked[T] {
    def mask(t: T): Long
}

object SudokuSolver {

    implicit object BitMaskedFieldType extends BitMasked[FieldType] {
        override def mask(t: FieldType): Long = t.mask
    }

    type IndexGroup = Seq[Int]

    private def loadPuzzle(path: String) = {
        Loader.loadPuzzle(Source.fromFile(path)) match {
            case Right(puzzle) => puzzle
            case Left(error) => throw new Exception(error)
        }
    }

    private def loadGroups(path: String) = {
        Loader.loadGroups(Source.fromFile(path))
    }

    def main(args: Array[String]): Unit = {

        val (puzzle, reference, groups) = args match {
            case Array(puzzlePath, "none", groupPaths @ _*) =>
                (loadPuzzle(puzzlePath), None, groupPaths.flatMap(loadGroups))

            case Array(puzzlePath, referencePath, groupPaths @ _*) =>
                (loadPuzzle(puzzlePath), Some(loadPuzzle(referencePath)), groupPaths.flatMap(loadGroups))
        }


        for (_ <- 1 to sys.env.getOrElse("PROFILE", "0").toInt) {
            solve(/*scala.util.Random.shuffle(*/puzzle.field/*)*/, puzzle.domain, puzzle.undefined, groups)
        }

        println("Input:")
        printConcreteState(puzzle.width, puzzle.field, puzzle.undefined)

        val start = System.currentTimeMillis()
        val solution_? = solve(puzzle.field, puzzle.domain, puzzle.undefined, groups)
        val delta = System.currentTimeMillis() - start
        println(s"The solver took ${delta}ms!")
        solution_? match {
            case Right(solution) =>
                println("Result:")
                printConcreteState(puzzle.width, solution, puzzle.undefined)
                reference.foreach { ref =>
                    if (solution == ref.field) {
                        println(s"Matches reference!")
                    } else {
                        println("Reference:")
                        printConcreteState(ref.width, ref.field, ref.undefined)
                    }
                }
            case Left(reason) =>
                println(s"No solution found: $reason")
        }

    }

    def solve[T](field: IndexedSeq[T], domain: Set[T], undefined: T, groups: Seq[IndexGroup])(implicit ordering: Ordering[T], masked: BitMasked[T]): Either[String, Seq[T]] = {

        if (field.filter(_ != undefined).exists(!domain.contains(_))) Left("Field contains values which are not in the domain, unsolvable!")
        else {

            val groupLookup = groups.foldLeft(Map.empty[Int, List[Int]]) { (lookup, group) =>
                group.foldLeft(lookup) { (lookup, index) =>
                    val cur = lookup.getOrElse(index, Nil)

                    lookup + (index -> (cur ++ group.filter(_ != index)))
                }.mapValues(_.distinct)
            }

            println("Symbol mapping:")
            val bitStrings = domain.map(s => (s.toString, masked.mask(s).toBinaryString)).toMap
            val longest = bitStrings.values.map(_.length).max
            for ((s, mask) <- bitStrings) {
                println(s"$s -> ${mask.reverse.padTo(longest, '0').reverse}")
            }


            val unconstrained = domain.map(masked.mask).reduce(_ | _)
            val initialField = field.map {
                case `undefined` => unconstrained
                case v => masked.mask(v)
            }
            val symbolReverseLookup = domain.map(s => (masked.mask(s), s)).toMap

            val reducedField = reduceSearchSpace(initialField, groupLookup)
//            printSolverState(reducedField)

            val solution =
                if (reducedField.contains(0L)) Left("At least one field as been eliminated during constraint satisfaction.") // the puzzle is not solvable with the given constraints
                else if (reducedField.exists(bitCount(_) > 1)) searchSolution(reducedField, symbolReverseLookup.keySet.toList, groupLookup).toRight("No solution found.") // the the search space has been reduced
                else Right(reducedField) // easy puzzle will be reduced the to solution

            solution.map(_.map(symbolReverseLookup))
        }
    }

    def resolveIndexGroup(field: IndexedSeq[Long], group: List[Int]): List[Long] =
        group.map(field)

    def resolveIndexGroups(field: IndexedSeq[Long], indexGroupLookup: Map[Int, List[Int]]): Map[Int, List[Long]] =
        indexGroupLookup.mapValues(resolveIndexGroup(field, _: List[Int]))

    @tailrec
    def reduceSearchSpace[T](field: IndexedSeq[Long], groups: Map[Int, List[Int]]): IndexedSeq[Long] = {
        val domainGroups = resolveIndexGroups(field, groups)
        val nextField = field.indices.zip(field).map {
            case (_, cellDomain) if bitCount(cellDomain) <= 1 => cellDomain
            case (i, cellDomain) => solveField(cellDomain, domainGroups(i))
        }

        if (field.equals(nextField)) nextField
        else reduceSearchSpace(nextField, groups)
    }

    @tailrec
    def solveField(value: Long, groups: Seq[Long]): Long = {
        if (groups.isEmpty) value
        else {
            if (bitCount(groups.head) == 1) solveField(value & ~groups.head, groups.tail)
            else solveField(value, groups.tail)
        }
    }

    def searchSolution[T](initialField: IndexedSeq[Long], domain: List[Long], groups: Map[Int, List[Int]]): Option[Seq[Long]] = {
        def nextUnsolvedField(field: IndexedSeq[Long], from: Int): Int =
            field.indexWhere(bitCount(_) > 1, from)

        def search(currentField: IndexedSeq[Long], offset: Int): Option[Seq[Long]] = {
//            println(s"search: $offset, ${currentField(offset).toSeq.sorted}")
//            println("###############")
//            printSolverState(currentField)
//            println("###############")
//            Thread.sleep(1000)
            val next = nextUnsolvedField(currentField, offset)
            if (next == -1) Some(currentField)
            else iterateOptions(currentField, next, currentField(next), resolveIndexGroups(currentField, groups))
        }

        def isValid(value: Long, groups: Seq[Long]): Boolean =
            solveField(value, groups) == value

        @tailrec
        def iterateOptions(currentField: IndexedSeq[Long], index: Int, options: Long, domainGroups: Map[Int, Seq[Long]]): Option[Seq[Long]] = {
            println(s"options: ${options.toBinaryString.reverse.padTo(9, '0').reverse}")
            if (options != 0L) {
                val option = domain.dropWhile(m => (m & options) == 0L).head
                val rest = options & ~option
                if (isValid(option, domainGroups(index))) {
                    search(currentField.updated(index, option), index) match {
                        case None => iterateOptions(currentField, index, rest, domainGroups)
                        case r => r
                    }
                } else iterateOptions(currentField, index, rest, domainGroups)
            } else None
        }

        search(initialField, 0)
    }

    def printConcreteState[T](width: Int, sudoku: Seq[T], undef: T): Unit = {
        val rows = sudoku.grouped(width)
        val squareSize = math.sqrt(width).toInt
        println(rows.map(_.grouped(squareSize).map(_.mkString(" ").replace(undef.toString, " ")).mkString(" | ")).grouped(squareSize).map(_.mkString("\n")).mkString(s"\n${"―" * (width * 2 + squareSize)}\n"))
    }

    def printSolverState[T](field: Seq[Set[T]])(implicit ordering: Ordering[T]): Unit = {
        val strings = field.map {
            case d if d.size == 1 => d.head.toString
            case d if d.isEmpty => "X"
            case d => s"[${d.toSeq.sorted.mkString(", ")}]"
        }
        val maxLen = strings.map(_.length).max
        print(strings.map(_.padTo(maxLen, ' ')).grouped(9).map { line =>
            line.mkString("  ")
        }.mkString("", "\n", "\n"))
    }
}
