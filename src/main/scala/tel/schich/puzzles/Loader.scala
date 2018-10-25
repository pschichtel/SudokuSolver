package tel.schich.puzzles

import tel.schich.puzzles.SudokuSolver.IndexGroup

import scala.io.Source
import scala.util.{Failure, Success, Try}

case class Sudoku[T](field: IndexedSeq[T], width: Int, domain: Set[T], undefined: T)
case class FieldType(id: Char) {
    override def toString: String = s"$id"
}

object UndefinedFieldType extends FieldType(' ')

object FieldType {
    implicit val ordering: Ordering[FieldType] = Ordering.by(_.id)
}

object Loader {

    val UndefinedId = '_'

    def parsePuzzle(in: String): Either[String, Sudoku[FieldType]] = {
        val stripped = in.replaceAll("(\r\n|\r|\n)+", "\n").split('\n').toVector.map(_.replaceAll("\\s", ""))
        stripped.headOption.toRight("Empty spec").right.flatMap { domainSpec =>
            val domain = domainSpec.toSet.map(FieldType.apply)
            if (domain.isEmpty) Left("empty domain")
            else {
                val widths = stripped.tail.map(_.length).toSet
                if (widths.size != 1) Left("inconsistent row length")
                else {
                    val fieldSpec = stripped.tail.flatten
                    val domainLookup = domain.map(ft => (ft.id, ft)).toMap + (UndefinedId -> UndefinedFieldType)
                    if (fieldSpec.exists(!domainLookup.contains(_))) Left("Unknown char in spec")
                    else Right(Sudoku(fieldSpec.map(domainLookup), widths.head, domain, UndefinedFieldType))
                }
            }
        }
    }

    def loadPuzzle(path: String): Either[String, Sudoku[FieldType]] = {
        Try(loadPuzzle(Source.fromFile(path))) match {
            case Success(either) => either
            case Failure(e) => Left(e.getLocalizedMessage)
        }
    }

    def loadPuzzle(in: Source): Either[String, Sudoku[FieldType]] =
        parsePuzzle(in.mkString)

    def loadGroups(input: String): Seq[IndexGroup] = {
        input
            .replaceAll("(\r\n|\r|\n)", "\n")
            .split("\n{2,}")
            .map(_.replaceAll("\\s", ""))
            .flatMap(groupSpecToGroup)
    }

    def groupSpecToGroup(spec: String): Seq[IndexGroup] = {
        val specWithIndexes = spec.zipWithIndex
        val groups = specWithIndexes.groupBy(_._1) - UndefinedId

        groups.values.map(_.map(_._2)).toVector
    }

    def loadGroups(in: Source): Seq[IndexGroup] =
        loadGroups(in.mkString)
}
