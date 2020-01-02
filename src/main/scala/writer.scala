package edu.furman.classics.oedipus2019
import scala.io.Source
import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.annotation.tailrec
import edu.furman.classics.citewriter._


object writer {

	case class LsjDef( urn: Cite2Urn, lemma: String, entry: String)

	def shortDefHtml( urnOption: Option[Cite2Urn], lsj: Vector[LsjDef] ): String = {
		val lexUrl = "http://folio2.furman.edu/lsj/?urn="	
		urnOption match {
			case None => s"""<p class="ot_lsj_link">No LSJ entry found.</p>"""
			case Some(u) => {
				val foundEntry: Option[LsjDef] = lsj.find( _.urn.dropVersion == u.dropVersion)
				foundEntry match {
					case None => s"""<p class="ot_lsj_link">No LSJ entry found for ${u}.</p>"""
					case Some(e) => {
						s"""<p class="ot_lsj_link">
						<a href="${lexUrl}${u.dropVersion}"><span class="ot_lsj_lemma">${e.lemma}</a>
						<span class="ot_lsj_def">${e.entry}</span>
						</p>"""
					}
				}
			}
		}	
	}
	
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

	def writeMorphBlock(vcc: Vector[ (Corpus, Corpus) ], oTokens: Vector[OToken], lexColl: Vector[LsjDef]): String = {
			vcc.map( cc => {
				val corp: Corpus = cc._2
				corp.nodes.map(n => {
					val nodeUrn = n.urn
					val opener = s"""<div class="cite_rollover_commentary_comment" data-commentsOn="${nodeUrn}">"""
					val thisToken: Option[OToken] = {
						oTokens.find( _.ctsUrn == nodeUrn )
					}
					thisToken match {
						case Some(tok) => {
							val morph = s"""<span class="ot_morph">${tok.morph}</span>"""
							val lemmaUrn = tok.lemmaUrn
							val sentence = tok.sentenceUrn
							val lsjLink = shortDefHtml(lemmaUrn, lexColl)
							s"""${opener}<p>${morph}</p>
							${lsjLink}
							<p><a class="open-syntax-modal" id="load-syntax-${utilities.urnToHtmlId(sentence)}" data-target="syntax-${utilities.urnToHtmlId(sentence)}">Show Syntax.</a></p>
							</div>"""
						}
						case None => ""
					}
				}).mkString("\n")
			}).mkString("\n")
	}

	def makeSyntaxFile(p: otPage, oTokens: Vector[OToken], ot: Oedipus): String = {
		val opener = s"""var configArray = Array(\n"""
		val closer = s""");\n\nvar tokenIndex = new Map();"""

		val tokens: Vector[CtsUrn] = {
			p.vcc.map( cc => {
				cc._2.urns
			}).flatten
		}
		val sentences: Vector[Cite2Urn] = {
			tokens.view.map( t => {
				oTokens.find( _.ctsUrn == t)
			}).filter( _ != None).map( _.get).map(_.sentenceUrn).toVector.distinct	
		}
		val syntaxes: Vector[syntax.SyntaxSentence] = {
			sentences.map( s => {
				ot.getSyntax(oTokens, s)
			})
		}

		val jsonString = syntaxes.map( s => {
			val containerId = s"syntax-${utilities.urnToHtmlId(s.sentenceUrn)}"
			s"""{ containerId: "${containerId}", json: ${s.toJson} }, """
		}).mkString("\n")

		opener + jsonString + closer
	}

	def htmlPage( p: otPage, oTokens: Vector[OToken], lexColl: Vector[LsjDef], path: String, ot: Oedipus): Unit = {
		val template: String = getTemplate
		// Write text-content
		val textBlock: String = writeTextBlock(p.vcc)
		// Write morphology
		val morphologyBlock: String = writeMorphBlock(p.vcc, oTokens, lexColl)
		// Write syntax
		val syntaxFileName: String = s"""syntax-${p.fileName.replaceAll("\\.html","")}.js"""
		val syntaxData: String = makeSyntaxFile(p, oTokens, ot)
		utilities.saveString(syntaxData, path, syntaxFileName)
		// Get navigation
		val navString: String = {
			getNavString(p.prevFn, p.nextFn)
		}
		// Get basic line-numbers
		val lineNumbers: String = {
			val fromLine: String = p.vcc.head._1.urns.head.collapsePassageTo(1).passageComponent
			val toLine: String = p.vcc.last._1.urns.last.collapsePassageTo(1).passageComponent 
			s"Lines ${fromLine} to ${toLine}."
		}

		// Swap in text-content
		val withText: String = template.replaceAll("TEXT_GOES_HERE", textBlock).replaceAll("MORPHOLOGY_GOES_HERE",morphologyBlock).replaceAll("SYNTAX_DATA_FILE_NAME",syntaxFileName).replaceAll("LINE_NUMBERS_HERE", lineNumbers).replaceAll("NAVIGATION_HERE", navString)
		utilities.saveString(withText, path, p.fileName)
	}

	def getTemplate: String = {
		val path = "templates/page.html"
		utilities.loadFile(path).mkString("\n")
	}

	def getIndexTemplate: String = {
		val path = "templates/index.html"
		utilities.loadFile(path).mkString("\n")
	}

	case class otPage( vcc: Vector[(Corpus, Corpus)], fileName: String, prevFn: Option[String], nextFn: Option[String], index: Int, howMany: Int)

	def htmlPages( vvcc: Vector[Vector[(Corpus, Corpus)]], oTokens: Vector[OToken], lexColl: Vector[LsjDef], ot: Oedipus, htmlDirectory: String = "html/pages/"): Unit = {
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
	  for ( pi <- otPages.zipWithIndex ) {
	  	val p = pi._1
	  	val i = pi._2
	  	println(s"Writing page ${i + 1} of ${otPages.size}")
	  	//htmlPage( p, oTokens, lexColl, htmlDirectory, ot )
	  }
	  // Make index page
	  makeIndexPage(otPages)


		/*
		for ( hv <- htmlVec) {
			hv.save( filePath = htmlDirectory)
		}
		*/
	}

	def makeIndexPage(otPages: Vector[otPage], htmlDirectory: String = "html/pages/"): Unit = {
			val templatePage: String = getIndexTemplate
			utilities.saveString(templatePage, htmlDirectory, "index.html")
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

	def getNavString( prevFileName: Option[String], nextFileName: Option[String]): String = {
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