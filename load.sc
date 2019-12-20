import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec
import com.github.tototoshi.csv._
import edu.furman.classics.oedipus2019._



val bigOT = "src/resources/ot_all.csv"
val smallOT = "src/resources/ot_small.csv"

val ot = new Oedipus( bigOT )

val otExemplar: Corpus = ot.toExemplar

val otTextEdition: Corpus = ot.toTextEdition( otExemplar )

val otSpeakerEdition: Corpus = ot.toReaderExemplar(otExemplar)