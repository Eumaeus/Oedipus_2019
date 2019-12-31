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



val bigOT = "data/ot_small.csv"

val ot = new Oedipus( bigOT )

val lib = utilities.loadLibrary("cex/ot_small.cex")
val tr = lib.textRepository.get
val cat = tr.catalog
val cexCorp = tr.corpus

val fuU = CtsUrn("urn:cts:greekLit:tlg0011.tlg004.fu:")
val spU = CtsUrn("urn:cts:greekLit:tlg0011.tlg004.fu.sp:")
val tokU = CtsUrn("urn:cts:greekLit:tlg0011.tlg004.fu.tokens:")


val otContent: Vector[ Vector [(Corpus, Corpus)]] = ot.readerPair(25)



/*
val cexTokCorp = cexCorp >= tokU
val cexSpCorp = cexCorp >= spU

val tokCorp = ot.corpus >= tokU
val spCorp = ot.corpus >= spU
*/




