package tel.schich.puzzles

import org.scalatest.FlatSpec

import scala.io.Source.fromResource

class Tests extends FlatSpec {

    val Base = getClass.getPackage.getName.replace('.', '/')

    "Loader" should "load puzzle" in {
        Loader.loadPuzzle(fromResource(s"$Base/sudokus/easy.txt"))
    }

    "Loader" should "load groups" in {
        val groups = Loader.loadGroups(fromResource(s"$Base/groups/rows-cols.txt"))

        assert(groups.forall(_.size == 9))
        assert(groups.length == 18)
        assert(groups.flatten.toSet.size == 81)
    }
}
