package tel.schich.puzzles

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.io.Source

object SudokuSolver {

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

        val start = System.nanoTime()
        val solution_? = solve(puzzle.field, puzzle.domain, puzzle.undefined, groups)
        val delta = System.nanoTime() - start
        println(s"The solver took ${(delta / 1000).toString.reverse.grouped(3).mkString(",").reverse}µs!")
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

    def solve[T](field: IndexedSeq[T], domain: Set[T], undefined: T, groups: Seq[IndexGroup])(implicit ordering: Ordering[T]): Either[String, Seq[T]] = {

        if (field.filter(_ != undefined).exists(!domain.contains(_))) Left("Field contains values which are not in the domain, unsolvable!")
        else {

            val groupLookup = groups.foldLeft(Map.empty[Int, Vector[Int]]) { (lookup, group) =>
                group.foldLeft(lookup) { (lookup, index) =>
                    val cur = lookup.getOrElse(index, Vector.empty)

                    lookup + (index -> (cur ++ group.filter(_ != index)))
                }.mapValues(_.distinct)
            }

            val initialField = field.map {
                case `undefined` => domain
                case v => Set(v)
            }

            val reducedField = reduceSearchSpace(initialField, groupLookup)
//            printSolverState(reducedField)

            val solution =
                if (reducedField.exists(_.isEmpty)) Left("At least one field as been eliminated during constraint satisfaction.") // the puzzle is not solvable with the given constraints
                else if (reducedField.exists(_.size > 1)) searchSolution(reducedField, groupLookup).toRight("No solution found.") // the the search space has been reduced
                else Right(reducedField) // easy puzzle will be reduced the to solution

            solution.map(_.map(_.head))
        }
    }

    def resolveIndexGroup[T](field: IndexedSeq[Set[T]], group: Seq[Int]): Seq[Set[T]] =
        group.map(field)

    def resolveIndexGroups[T](field: IndexedSeq[Set[T]], indexGroupLookup: Map[Int, Seq[Int]]): Map[Int, Seq[Set[T]]] =
        indexGroupLookup.mapValues(resolveIndexGroup(field, _: Seq[Int]))

    @tailrec
    def reduceSearchSpace[T](field: IndexedSeq[Set[T]], groups: Map[Int, Seq[Int]]): IndexedSeq[Set[T]] = {
        val domainGroups = resolveIndexGroups(field, groups)
        val nextField = field.indices.zip(field).map {
            case (_, cellDomain) if cellDomain.size <= 1 => cellDomain
            case (i, cellDomain) => cellDomain.diff(findConstrains(field, domainGroups(i), i))
        }

        if (field.equals(nextField)) nextField
        else reduceSearchSpace(nextField, groups)
    }

    def reduceCellDomain[T](domain: Set[T], domainGroups: Seq[Set[T]]): Set[T] =
        domainGroups.fold(domain)(_ -- _)

    def findConstrains[T](field: Seq[Set[T]], groups: Seq[Set[T]], i: Int): Set[T] = {
        groups.filter(_.size == 1).flatten.toSet
    }

    def searchSolution[T](initialField: IndexedSeq[Set[T]], groups: Map[Int, Seq[Int]])(implicit ordering: Ordering[T]): Option[Seq[Set[T]]] = {
        def nextIndex(field: IndexedSeq[Set[T]], from: Int): Int =
            field.indexWhere(_.size > 1, from)

        def search(currentField: IndexedSeq[Set[T]], offset: Int): Option[Seq[Set[T]]] = {
//            println(s"search: $offset, ${currentField(offset).toSeq.sorted}")
//            println("###############")
//            printSolverState(currentField)
//            println("###############")
//            Thread.sleep(1000)
            val next = nextIndex(currentField, offset)
            if (next == -1) Some(currentField)
            else iterateOptions(currentField, next, currentField(next), resolveIndexGroups(currentField, groups))
        }

        def isValid(currentField: IndexedSeq[Set[T]], value: T, index: Int, groups: Seq[Set[T]]): Boolean =
            !findConstrains(currentField, groups, index).contains(value)

        @tailrec
        def iterateOptions(currentField: IndexedSeq[Set[T]], index: Int, options: Set[T], domainGroups: Map[Int, Seq[Set[T]]]): Option[Seq[Set[T]]] = {
            if (options.nonEmpty) {
                val option = options.head
                val rest = options - option
                if (isValid(currentField, option, index, domainGroups(index))) {
                    search(currentField.updated(index, Set(option)), index) match {
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
