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




val bigOT = "data/ot_small.csv"

val lexColl: Vector[writer.LsjDef] = {
	val lines: Vector[String] = utilities.loadFile("data/lsj_short.txt")
	lines.map( l => {
		val urn: Cite2Urn = Cite2Urn(l.split("#").toVector(1))
		val lemma: String = l.split("#").toVector(2)
		val entry: String = l.split("#").toVector(3) 
		writer.LsjDef(urn, lemma, entry)
	})
}

val ot = new Oedipus( bigOT )

val lib = utilities.loadLibrary("cex/ot_small.cex")
val tr = lib.textRepository.get
val cat = tr.catalog
val cexCorp = tr.corpus

val fuU = CtsUrn("urn:cts:greekLit:tlg0011.tlg004.fu:")
val spU = CtsUrn("urn:cts:greekLit:tlg0011.tlg004.fu.sp:")
val tokU = CtsUrn("urn:cts:greekLit:tlg0011.tlg004.fu.tokens:")


val otContent: Vector[ Vector [(Corpus, Corpus)]] = ot.readerPair(25)

writer.htmlPages(otContent, ot.tokens, lexColl, ot)




