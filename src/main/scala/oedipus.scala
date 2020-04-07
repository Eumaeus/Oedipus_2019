package edu.furman.classics.oedipus2019

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec
import com.github.tototoshi.csv._
import scala.annotation.tailrec

case class SpeakerToken( urn: CtsUrn, text: String, speaker: String, seq: Int ) extends Ordered[SpeakerToken] {
	def compare(that: SpeakerToken) = this.seq compare that.seq
}

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
		whoSaidIt: String,
		lemmaUrn: Option[Cite2Urn]
	) {

		def morph: String = {
				val optionVec: Vector[Option[Cite2Urn]] = {
					Vector(pos, person, number, tense, mood, voice, gender, grammaticalCase, degree)
				}
				val filtVec = optionVec.filter( _ != None )
				val stringVec = filtVec.map(_.get.objectComponent)
				s"""${stringVec.mkString(", ")}."""
		}


		val speaker: String = whoSaidIt

		val dependsOn: String = {
			headTokenUrn match {
				case None => { "Is root."}
				case Some(u) => { u.toString }
			}
		}

		val csv: String = {
			s"""${lineNo},${tokenNo},${ctsUrn},${sentenceUrn},${sentenceSeq},${tokenUrn},${tokenSeq},${headTokenUrn.getOrElse("null")},${if (surfaceForm == ",") """","""" else surfaceForm},${lemma},${pos.getOrElse("null")},${person.getOrElse("null")},${number.getOrElse("null")},${tense.getOrElse("null")},${mood.getOrElse("null")},${voice.getOrElse("null")},${gender.getOrElse("null")},${grammaticalCase.getOrElse("null")},${degree.getOrElse("null")},${headTokenLemma.getOrElse("null")},${relation},${whoSaidIt},${lemmaUrn.getOrElse("null")}"""
		}

	}

class Oedipus( filePath: String ) {
	
	val file: JFile = new JFile(filePath)
  val reader = CSVReader.open(file)

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
				r(21),
				if (r(22) == "null") None else Some(Cite2Urn(r(22)))
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

	val tokenExemplarUrn: CtsUrn = this.tokens.head.ctsUrn.dropPassage
	val editionUrn: CtsUrn = tokenExemplarUrn.dropPassage.dropExemplar
	val speakerEditionUrn: CtsUrn = editionUrn.addExemplar("sp") 

	val tokenCorpus = toExemplar
	val editionCorpus = toTextEdition()
	val readerCorpus = toReaderExemplar()
	val corpus: Corpus = {
		tokenCorpus ++ editionCorpus ++  readerCorpus
	}

	/**
    * Returns a text citable by lines
	**/
	def toTextEdition( exemplarCorpus: Corpus = tokenCorpus): Corpus = {
		val linesCorps: Vector[Corpus] = {
			val exemplar: Corpus = this.toExemplar
			val urns: Vector[CtsUrn] = exemplar.urns.map( _.collapsePassageBy(1)).distinct
			urns.map( u => {
				exemplar ~~ u
			})
		}
		val cNodes: Vector[CitableNode] = {
			linesCorps.map( lc => {
				val u: CtsUrn = {
					val passage: String = lc.nodes.head.urn.collapsePassageBy(1).passageComponent
					editionUrn.addPassage(passage)
				}
				val t: String = lc.nodes.map( _.text ).mkString(" ")
				CitableNode(u, t)
			})
		}	
		Corpus(cNodes)
	}

	/** 
	  * Returns an exemplar with speakers identified
	**/
	def toReaderExemplar( exemplarCorpus: Corpus = tokenCorpus ): Corpus = {
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

		/* There may be more than one speaker per line, so let's 
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
					println(s"\n-----\n${lineUrn}\n\n${groupedBySpeaker}")
					(lineUrn, groupedBySpeaker)
				}).map( sl => {
					val u: CtsUrn = sl._1
					val v: Vector[(String, Vector[SpeakerToken])] = {
						sl._2.sortBy(_._2.head.seq)
					}
					(u, v)
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
				val lineUrn = sfl._1.dropExemplar.addExemplar("sp")		
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

	def chunkBySpeech( c: Corpus ): Vector[Corpus] = {
			case class IndexedNode( n: CitableNode, i: Int)	
			val nodes: Vector[IndexedNode] = c.nodes.zipWithIndex.map( n => IndexedNode(n._1, n._2))

			val speakerNodes: Vector[IndexedNode] = nodes.filter( n => {
				n.n.urn.passageComponent.contains("speaker")
			})

			val speechRanges: Vector[(Int, Int)] = {
				speakerNodes.zipWithIndex.map( sni => {
					val sn = sni._1
					val idx = sni._2
					val first = sn.i
					val last = {
						if (sn.i == speakerNodes.last.i) nodes.last.i
						else speakerNodes(idx + 1).i - 1
					}
					(first, last)
				})
			}

			speechRanges.map( sr => {
				Corpus(nodes.filter( n => {
					( (n.i >= sr._1) & (n.i <= sr._2) )
				}).map( _.n ))
			})
	}

	def sentenceUrnForPassage( otoks: Vector[OToken], passage: CtsUrn ): Option[Cite2Urn] = {
			val filtered: Vector[OToken] = otoks.filter( _.ctsUrn == passage)
			if (filtered.size > 0) Some(filtered.head.sentenceUrn)
			else None
	}	

	def getSyntax( otoks: Vector[OToken], sentenceUrn: Cite2Urn ) : syntax.SyntaxSentence = {
		val tokens: Vector[OToken] = otoks.filter(_.sentenceUrn == sentenceUrn) 	
		val syntaxNodes: Vector[syntax.SyntaxNode] = {
			tokens.map( t => {
				val su: Cite2Urn = sentenceUrn
				val tu: Cite2Urn = t.tokenUrn
				val ctsUrn: Option[CtsUrn] = Some(t.ctsUrn)
				val sf: String = t.surfaceForm
				val rel: Option[Cite2Urn] = None
				val relLabel: String = t.relation
				val hUrn: Option[Cite2Urn] = {
					val u = t.headTokenUrn.get
					if (u.objectComponent.contains("_0")) None
					else Some(u)
				}
				val otoken: OToken = t
				syntax.SyntaxNode(su, tu, ctsUrn, sf, rel, relLabel, hUrn, t)
			})
		}
		syntax.SyntaxSentence(sentenceUrn, syntaxNodes )
	}

	def chunkBySpeechesAndLines( c: Corpus, targetLines: Int = 25 ): Vector[Vector[Corpus]] = {
		val vc: Vector[Corpus] = chunkBySpeech(c)
		// 'resultCorpusVecVec' is the "accumulator"
		// 'whatsLeft' is the unprocessed Vector of speech-corpora
		// 'target' is the number of lines we want to aim for
		@tailrec def recurseEqualSize( 
			resultCorpusVecVec: Vector[Vector[Corpus]], 
			whatsLeft: Vector[Corpus], 
			targetLines: Int): Vector[Vector[Corpus]] = {
			// First, we see the size of the latest Corpus in the list
			// We start with an empty accumulator, so we need to check for that possibility
			val workingCorpusVecSize: Int = {
				if (resultCorpusVecVec.size == 0) 0
				else {
					// Take the last Corpus in the list; count its lines.
					resultCorpusVecVec.last.flatMap( c => {
						c.nodes
					}).size
				}
			}
			println(s"\nworkingCorpusVecSize == ${workingCorpusVecSize}")
			println(s"whatsLeft == ${whatsLeft.size}")

			/* Three possibilities…
		 		 Case 1. There is only one Corpus left in whatsLeft
		 		 Case 2. We've just met the target
		 		 Case 3. We haven't met the target
			*/
			if ( whatsLeft.size == 0) { 
				// Case 1: Add it and recurse
				//val newResultVecVec: Vector[Vector[Corpus]] = resultCorpusVecVec :+ whatsLeft
				//println(s"last one.")
				//newResultVecVec
				resultCorpusVecVec
			} else if (workingCorpusVecSize >= targetLines) { 
				// Case 2: Recurse with an empty final Corpus as the '.lates' in results
				println(s"limit reached: ${workingCorpusVecSize}")
				val emptyNewCorpusVec: Vector[Corpus] = Vector[Corpus]()
				val newResultVecVec: Vector[Vector[Corpus]] = resultCorpusVecVec :+ emptyNewCorpusVec
				recurseEqualSize( newResultVecVec, whatsLeft, targetLines)
			} else {
				// Case 3: Add one more Corpus to the latest Vector of Corpora, recurse
				println(s"add one and recurse: ${workingCorpusVecSize}")
				val workingCorpusVec: Vector[Corpus] = {
					// The very first time through, we'll have an empty Corpus, so check for this
					if ( resultCorpusVecVec.size == 0 ) {
						Vector[Corpus]()
					} else {
						resultCorpusVecVec.last
					}
				}
				// All the untreated corpora…
				val poolCorpora: Vector[Corpus] = whatsLeft
				println(s"the next speech: ${poolCorpora.head.size}")
				// Add the next node to our working corpus
				val expandedCorpVec: Vector[Corpus] = workingCorpusVec ++ Vector(poolCorpora.head)
				println(s"expanded to: ${expandedCorpVec.size}")
				// Remove that node from whatsLeft
				val newWhatsLeft: Vector[Corpus] = poolCorpora.tail
				// Add the new version of the working corpus to results
				val newResultCorpusVecVec: Vector[Vector[Corpus]] = resultCorpusVecVec.dropRight(1) :+ expandedCorpVec
				// Recurse!
				recurseEqualSize( newResultCorpusVecVec, newWhatsLeft, targetLines)
			}
		}
		// Invoke the recursive function for the first time.
		val answer: Vector[Vector[Corpus]] = recurseEqualSize( Vector[Vector[Corpus]](), vc, targetLines)
		answer

	}	

	/** Takes the Speaker-corpus, chunks by speech, and pairs
	 *  each chunk with the corresponding chunk of the token-exemplar
	 */		
	def readerPair( lines: Int = 25): Vector[ Vector[ (Corpus, Corpus )]] = {
		val chunkedReadingCorpus: Vector[ Vector[ Corpus]] = chunkBySpeechesAndLines(this.readerCorpus, lines)
		chunkedReadingCorpus.map( vc => {
			println(s"${chunkedReadingCorpus.indexOf(vc)} out of ${chunkedReadingCorpus.size}")
			vc.map( c => {
				println(s"\t${vc.indexOf(c)} of ${vc.size}")
				val thisUrn: CtsUrn = {
					val first: String = c.nodes.head.urn.collapsePassageTo(1).passageComponent
					val last: String = c.nodes.last.urn.collapsePassageTo(1).passageComponent
					tokenExemplarUrn.addPassage(s"${first}-${last}")
				}
				val newCorp = this.corpus >= thisUrn
				(c, newCorp) 
			} )
		})	
	}

	

}

object Oedipus{
	def apply( fp: String ) = {
		val filePath:  String = fp
	}
}
	
