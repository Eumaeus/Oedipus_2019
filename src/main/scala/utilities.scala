package edu.furman.classics.oedipus2019
import scala.io.Source
import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec


object utilities {
	def showMe(v:Any):Unit = {
	  v match {
	  	case _:Corpus => {
	  		for ( n <- v.asInstanceOf[Corpus].nodes) {
	  			println(s"${n.urn}\t\t${n.text}")
	  		}	
	  	}
	    case _:Vector[Any] => println(s"""\n----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----\n""")
	    case _:Iterable[Any] => println(s"""\n----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----\n""")
	    case _ => println(s"\n-----\n${v}\n----\n")
	  }
	}

	def loadLibrary(fp:String):CiteLibrary = {
		val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"))
		library
	}

	def loadFile( fp: String ): Vector[String] = {
		Source.fromFile(fp).getLines.toVector
	}

	def saveStringVec(sv:Vector[String], filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
		val file: File = File(filePath + fileName)
		for (s <- sv){
			file.appendLine().append(s)
		}
	}

	def urnToHtmlId( u: Urn ): String = {
		u.toString.replaceAll("[.:]","_")
	}

	def saveString(s:String, filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
		val file: File = File(filePath + fileName)
		file.overwrite(s)
	}

	def splitWithSplitter(text: String, puncs: String =  """[()\[\]·⸁.,; "?·!–—⸂⸃]"""): Vector[String] = {
		val regexWithSplitter = s"((?<=${puncs})|(?=${puncs}))"
		text.split(regexWithSplitter).toVector.filter(_.size > 0)
	}

	val punctuation: String = """[“”“‘()‘’'\[\]·_…⸁.,:; "?·!⸂⸃–—-]"""
	val alphabet: String = """[A-Za-z]"""

	def tokenHisto( tv: Vector[String]): Vector[(String, Int)] = {
			// assemble words into groups…
			val grouped: Vector[( String, Vector[String])] = tv.groupBy( t => t).toVector

			// Map the _._2 part of tuple _out of_ a Vector[String] and _into_ its size…	
			val remapped: Vector[ ( String, Int) ] = grouped.map( t => (t._1, t._2.size))

			// Sort…
			val sorted: Vector[ ( String, Int) ] =  remapped.sortBy( _._2 )

			// Return the histogram…
			sorted
			
		}

	def makeNGrams( n: Int, textVec: Vector[String] ): Vector[(String, Int)] = {
		// Using .sliding, let's get all possible combinations of N tokens
		val slid: Vector[String] = {
			val slid: Vector[Vector[String]] = textVec.sliding(n,1).toVector
			val toStrings: Vector[String] = slid.map( _.mkString(" ") )
			toStrings
		}

		val nGramTuples: Vector[(String, Int)] = {
			slid.groupBy( s => s).toVector.map( n => {
				( n._1, n._2.size )
			})
		}

	nGramTuples.sortBy(_._2).reverse

}


	// source: https://gist.github.com/sebleier/554280
	val stopWords: Vector[String] = Vector("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "you're", "you've", "you'll", "you'd", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "she's", "her", "hers", "herself", "it", "it's", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "that'll", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "don't", "should", "should've", "now", "d", "ll", "m", "o", "re", "ve", "y", "ain", "aren", "aren't", "couldn", "couldn't", "didn", "didn't", "doesn", "doesn't", "hadn", "hadn't", "hasn", "hasn't", "haven", "haven't", "isn", "isn't", "ma", "mightn", "mightn't", "mustn", "mustn't", "needn", "needn't", "shan", "shan't", "shouldn", "shouldn't", "wasn", "wasn't", "weren", "weren't", "won", "won't", "wouldn", "wouldn't")

}
