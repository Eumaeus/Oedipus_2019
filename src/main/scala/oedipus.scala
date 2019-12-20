package edu.furman.classics.oedipus2019

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec
import com.github.tototoshi.csv._

case class SpeakerToken( urn: CtsUrn, text: String, speaker: String, seq: Int ) extends Ordered[SpeakerToken] {
	def compare(that: SpeakerToken) = this.seq compare that.seq
}

class Oedipus( filePath: String ) {
	
	val file: JFile = new JFile(filePath)
  val reader = CSVReader.open(file)

	case class OToken( 
		lineNo: String, 
		tokenNo: Int,
		ctsUrn: CtsUrn,
		sentenceUrn: Cite2Urn,
		sentenceSeq: Int,
		tokenUrn: Cite2Urn,
		tokenSeq: Int,
		headTokenUrn: Option[Cite2Urn],
		surfaceForm: String,
		lemma: String,
		pos: Option[Cite2Urn],
		person: Option[Cite2Urn],
		number: Option[Cite2Urn],
		tense: Option[Cite2Urn],
		mood: Option[Cite2Urn],
		voice: Option[Cite2Urn],
		gender: Option[Cite2Urn],
		grammaticalCase: Option[Cite2Urn],
		degree: Option[Cite2Urn],
		headTokenLemma: Option[String],
		relation: String,
		whoSaidIt: String
	) {
		
	}

	val tokens: Vector[OToken] = {
		val otvec: Vector[OToken] = reader.all.tail.map( r => {
			OToken(
				r(0),
				r(1).toInt,
				CtsUrn(r(2)),
				Cite2Urn(r(3)),
				r(4).toInt,
				Cite2Urn(r(5)),
				r(6).toInt,
				Some(Cite2Urn(r(7))),
				r(8),
				r(9),
				if (r(10) == "null") None else Some(Cite2Urn(r(10))),
				if (r(11) == "null") None else Some(Cite2Urn(r(11))),
				if (r(12) == "null") None else Some(Cite2Urn(r(12))),
				if (r(13) == "null") None else Some(Cite2Urn(r(13))),
				if (r(14) == "null") None else Some(Cite2Urn(r(14))),
				if (r(15) == "null") None else Some(Cite2Urn(r(15))),
				if (r(16) == "null") None else Some(Cite2Urn(r(16))),
				if (r(17) == "null") None else Some(Cite2Urn(r(17))),
				if (r(18) == "null") None else Some(Cite2Urn(r(18))),
				if (r(19) == "null") None else Some(r(19)),
				r(20),
				r(21)
			)
		}).toVector
		otvec
	}

	/** 
	  * Returns a Corpus of citable, surface-form tokens
	**/
	def toExemplar: Corpus = {
		val nodes: Vector[CitableNode] = this.tokens.map( o => {
				val urn: CtsUrn = o.ctsUrn	
				val text: String = o.surfaceForm
				CitableNode(urn, text)
		})
		Corpus(nodes)
	}

	/**
    * Returns a text citable by lines
	**/
	def toTextEdition( exemplarCorpus: Corpus = this.toExemplar) = {
		val linesCorps: Vector[Corpus] = {
			val exemplar: Corpus = this.toExemplar
			val urns: Vector[CtsUrn] = exemplar.urns.map( _.collapsePassageBy(1)).distinct
			urns.map( u => {
				exemplar ~~ u
			})
		}
		val cNodes: Vector[CitableNode] = {
			linesCorps.map( lc => {
				val u: CtsUrn = lc.nodes.head.urn.collapsePassageBy(1)
				val t: String = lc.nodes.map( _.text ).mkString(" ")
				CitableNode(u, t)
			})
		}	
		Corpus(cNodes)
	}

	/** 
	  * Returns an exemplar with speakers identified
	**/
	def toReaderExemplar( exemplarCorpus: Corpus = this.toExemplar ): Corpus = {
		val speakerCorp: Vector[SpeakerToken] = {
			exemplarCorpus.nodes.zipWithIndex.map( n => {
				val u = n._1.urn
				val t = n._1.text
				val s:String = tokens.find( _.ctsUrn == u ).get.whoSaidIt
				SpeakerToken(u, t, s, n._2)
			})
		}

		val speakerLines: Vector[(CtsUrn, Vector[SpeakerToken])] = {
			speakerCorp.groupBy(_.urn.collapsePassageBy(1)).toVector.sortBy(_._2.head.seq)
		}		

		// Test sequence!
		val validOrderTest: Boolean = {
			speakerLines.filter( l => {
				val first: Int = l._2.head.seq
				val last: Int = l._2.last.seq
				val expectedVec: Vector[Int] = (first to last).toVector
				val seqVec: Vector[Int] = l._2.map( _.seq )
				expectedVec == seqVec
			}).size == speakerLines.size
		}

		println(s"\n\nvalid sequence = ${validOrderTest}\n\n")

		/* There may be one speaker per line, so let's 
		   group tokens by speaker within a line
		*/
		val groupedBySpeaker: Vector[(CtsUrn, Vector[(String, Vector[SpeakerToken])])] = {
				speakerLines.map( sl => {
					val lineUrn = sl._1
					val tokenVec = sl._2
					val groupedBySpeaker: Vector[(String, Vector[SpeakerToken])] = {
						val unsorted: Vector[(String, Vector[SpeakerToken])] = tokenVec.groupBy(_.speaker).toVector
						val sorted: Vector[(String, Vector[SpeakerToken])] = {
							unsorted.map( us => {
								val s = us._1
								val v = us._2
								val sortedV = v.sorted
								(s, v)
							})	
						}
						sorted
					}
					(lineUrn, groupedBySpeaker)
				})
		}

		/* Turn this into a Vector[ ( urn, Vector[ (string, string) ] ) ]
			 That is… A urn for the line, and a list of speaker+text 
		 */
		val speechesForLine: Vector[ (CtsUrn, Vector[ (String, String)])] = {
			groupedBySpeaker.map( gs => {
				val lineUrn: CtsUrn = gs._1	
				val speakerVec: Vector[(String, Vector[SpeakerToken])] = gs._2
				val speakerTextVec: Vector[(String, String)] = {
					speakerVec.map( sv => {
						val speaker: String = sv._1	
						val textVec: Vector[SpeakerToken] = sv._2
						val text: String = textVec.map(_.text).mkString(" ").replaceAll(" +"," ")
						(speaker, text)
					})
				}
				( lineUrn, speakerTextVec)
			})
		}

		/* Now we make citable nodes

			We have potentially more than one speaker in a line…			 
			 		We do, e.g., 
			 				urn…:200.0.speaker#Oedipus
			 				urn…:200.0.speech#some greek
			 				urn…:200.1.speaker#Creon
			 				urn…:200.1.speech#some greek
		*/

		val citableNodesAll: Vector[CitableNode] = {
			speechesForLine.map( sfl => {
				// Looking at a: (CtsUrn, Vector[ (String, String)])
				val lineUrn = sfl._1		
				val speeches = sfl._2
				val speechesWIndex = speeches.zipWithIndex
				// Vector[( (String, String), Int )]
				speechesWIndex.map( si => {
					val linePassage = lineUrn.passageComponent
					val speaker = si._1._1
					val speech = si._1._2
					val index = si._2 + 1
					val speakerPassage = s"${linePassage}.speaker${index}"
					val speakerUrn = lineUrn.addPassage(speakerPassage)
					val speechPassage = s"${linePassage}.${index}"
					val speechUrn = lineUrn.addPassage(speechPassage)
					Vector(
						CitableNode(speakerUrn, speaker),
						CitableNode(speechUrn, speech)
					)
				}).flatten
			}).flatten
		}

		// We don't want the speaker-nodes for *every* line; just when the speaker changes…
		val filteredNodes: Vector[CitableNode] = filterSpeakers(citableNodesAll)

		Corpus(filteredNodes)

	}

	/** Filter out all speaker-identifications, except when the speaker changes */
	def filterSpeakers( allNodes: Vector[CitableNode]): Vector[CitableNode] = {
		println("doing filterSpeakers")

		// Pull out just the speaker nodes
		val justSpeakerNodes = {
			allNodes.filter( _.urn.passageComponent.contains("speaker"))
		}

		// Fancy fold-left
		val filteredSpeakerNodes: Vector[CitableNode] = {
			justSpeakerNodes.foldLeft( Vector[CitableNode]())(( nodeVec, thisNode ) => 
					// We're just starting…
					if ( nodeVec.size == 0) {
						nodeVec :+ thisNode
					} else if ( nodeVec.last.text == thisNode.text ) {
						nodeVec
					} else {
						nodeVec :+ thisNode
					}
			)
		}

		val speakerPassages: Vector[CtsUrn] = filteredSpeakerNodes.map(_.urn)
		allNodes.filter( n => {
			 val isSpeaker: Boolean = n.urn.passageComponent.contains("speaker")
			 val inList: Boolean = speakerPassages.contains(n.urn)
			 (( isSpeaker == false ) | ( inList ))
		})
	}

}

object Oedipus{
	def apply( fp: String ) = {
		val filePath:  String = fp
	}
}
	
