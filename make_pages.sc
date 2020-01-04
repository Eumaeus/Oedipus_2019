import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import java.io._
import scala.annotation.tailrec
import com.github.tototoshi.csv._
import edu.furman.classics.oedipus2019._
import edu.furman.classics.citewriter._
import edu.holycross.shot.citeobj._



// Master data for the play
val bigOT = "data/ot_all.csv"

// Data for providing short LSJ definitions
val lexColl: Vector[writer.LsjDef] = {
	val lines: Vector[String] = utilities.loadFile("data/lsj_short.txt")
	lines.map( l => {
		val urn: Cite2Urn = Cite2Urn(l.split("#").toVector(1))
		val lemma: String = l.split("#").toVector(2)
		val entry: String = l.split("#").toVector(3) 
		writer.LsjDef(urn, lemma, entry)
	})
}

// Make an Object out of play's data
val ot = new Oedipus( bigOT )

/* USER CUSTOMIZABLE SETTING!
	 default: 25

	 The script will try to put 25 poetic lines on a page, while
	 keeping speeches intact (so some pages will have more than 25 lines)
*/

val linesPerPage: Int = 25

// Gather data for the pages…
val otContent: Vector[ Vector [(Corpus, Corpus)]] = ot.readerPair(linesPerPage)

// Write the pages and their accompanying JS files…
writer.htmlPages(otContent, ot.tokens, lexColl, ot)

println(s"Done making ${otContent.size} pages.")



