package edu.furman.classics.oedipus2019
import scala.io.Source
import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec
import edu.furman.classics.citewriter._


object writer {
	
	def writeTextBlock( vcc: Vector[(Corpus,Corpus)] ): String = {
		val readerWrapper = """<div class="ohco2_versionCorpus"><div class="ohco2_passageGroup  ohco2_stanza ">"""
		val readerWrapperClose = """</div></div>"""
		val tokenWrapper = """<div class="ohco2_passageGroup  ohco2_tokenized">"""
		val tokenWrapperClose = """</div>"""
		vcc.map( cc => {
			val readerText: Corpus = cc._1
			val passages: Vector[String] = cc._1.nodes.map( n => {
				if (n.urn.passageComponent.contains("speaker")) {
					val thisPassage: String = HtmlWriter.writeCitableNode(n)
					thisPassage
				} else {
					val bespokeNode: CitableNode = getBespokeNode(n)
					val thisPassage: String = {
						s"""<div class="cite_showhide_header cite_showhide_closed">
						${HtmlWriter.writeCitableNode(bespokeNode)}
						</div>"""
					}
					val tokenPassage: String = {
						val twiddleUrn = n.urn.dropExemplar.collapsePassageTo(1)
						val tokenCorp: Corpus = cc._2 ~~ twiddleUrn
						val nodesHtml = tokenCorp.nodes.map( tn => {
							HtmlWriter.writeCitableNode(tn)
						}).mkString("\n")
						s"${tokenWrapper}\n${nodesHtml}\n${tokenWrapperClose}"	
					}
					thisPassage + "\n" + tokenPassage
				}
			})
			readerWrapper + "\n" + passages.mkString("\n") + "\n" + readerWrapperClose
		}).mkString("\n")
	}

	def htmlPage( p: otPage, path: String): Unit = {
		val template: String = getTemplate
		// Write text-content
		val textBlock: String = writeTextBlock(p.vcc)
		// Swap in text-content
		val withText: String = template.replaceAll("TEXT_GOES_HERE", textBlock)
		utilities.saveString(withText, path, p.fileName)
	}

	def getTemplate: String = {
		val path = "templates/page.html"
		utilities.loadFile(path).mkString("\n")
	}

	case class otPage( vcc: Vector[(Corpus, Corpus)], fileName: String, prevFn: Option[String], nextFn: Option[String], index: Int, howMany: Int)

	def htmlPages( vvcc: Vector[Vector[(Corpus, Corpus)]], htmlDirectory: String = "html/pages/"): Unit = {
		// clear directory 
		val htmlDir: File = File(htmlDirectory)
		htmlDir.clear()
		// Each item in vvcc will be one page…
		val otPages: Vector[otPage] = vvcc.zipWithIndex.map( vcci => {
			val vcc = vcci._1
			val index = vcci._2 
			val fileName = generateFileName(vcc)
			val prevFn: Option[String] = {
				if (index == 0) None
				else {
					Some(generateFileName(vvcc(index - 1)))
				}
			}
			val nextFn: Option[String] = {
				if ( (index + 1) >= vvcc.size) None
				else {
					Some(generateFileName(vvcc(index + 1)))
				}
			}
			val howMany = vvcc.size
			otPage(vcc, fileName, prevFn, nextFn, index, howMany)
		})
		// Make a page for each and save it!
	  for ( p <- otPages ) {
	  	htmlPage( p, htmlDirectory )
	  }


		/*
		for ( hv <- htmlVec) {
			hv.save( filePath = htmlDirectory)
		}
		*/
	}

	def generateFileName( vcc: Vector[(Corpus, Corpus)], suffix: String = ".html" ): String = {
		val baseUrn: CtsUrn = vcc.head._1.urns.head.dropExemplar.dropVersion
		val fromPsg: String = vcc.head._1.urns.head.collapsePassageTo(1).passageComponent
		val toPsg: String = vcc.last._1.urns.last.collapsePassageTo(1).passageComponent
		val newUrn = baseUrn.addPassage(s"${fromPsg}-${toPsg}")
		val fn: String = utilities.urnToHtmlId(newUrn) + suffix
		fn
	}		

	def writeSyntaxJS( c: Corpus, fileName: String ): Unit = {
		val syntaxJson: String = ""
		val indexJS: String = ""
		syntaxJson + "\n\n" + indexJS
	}

	def sequenceString( index: Int, howMany: Int): String = {
		s"""<div class="cts_progress"><progress class="cts_progress" max="${howMany}" value="${index + 1}"/></div>"""
	}

	def navString( prevFileName: Option[String], nextFileName: Option[String]): String = {
		val prevStr: String = prevFileName match {
			case Some(fn) => {
				s"""<span class="cts_prev"><a href="${fn}"> ⇽ </a></span>"""
			}
			case None => ""
		}

		val nextStr: String = nextFileName match {
			case Some(fn) => {
				s"""<span class="cts_next"><a href="${fn}"> ⇾ </a></span>"""
			}
			case None => ""
		}

		val middleStr: String = {
			if ( (prevFileName != None) && (nextFileName != None) ) " | " else ""
		}

		s"""<div class="cts_nav">${prevStr}${middleStr}${nextStr}</div>"""

	}

	def getBespokeNode(n: CitableNode): CitableNode = {
		val u = n.urn
		val preText = n.text
		val fixedText = preText.replaceAll(" ,",",")
												   .replaceAll(" :",":")
												   .replaceAll(" \\.",".")
		val lineNo: String = u.collapsePassageTo(1).passageComponent
		val newText = s"""<span class="ot_lineNo">${lineNo}</span>${fixedText}"""
		CitableNode(u, newText)
	}

	/*	
	def syntaxIndex( c: Corpus ): Vector[(CtsUrn, Cite2Urn)] = {
		val uu: Vector[CtsUrn] = c.urns
		uu.map( u => {
			val matchingOT: OToken = tokens.find( t => {
				(t.ctsUrn == u)
			}).get
			(u, matchingOT.sentenceUrn )
		})
	}
	*/



}