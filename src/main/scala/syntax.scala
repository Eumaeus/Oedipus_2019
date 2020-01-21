package edu.furman.classics.oedipus2019
import scala.io.Source
import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec


object syntax {
	
	case class SyntaxNode( sentenceUrn: Cite2Urn, tokenUrn: Cite2Urn, ctsUrn: Option[CtsUrn], surfaceForm: String, relation: Option[Cite2Urn], relationLabel: String, head: Option[Cite2Urn], otoken: OToken)

	case class GraphedNode( node: SyntaxNode, children: Vector[GraphedNode])

	case class GenericNode( marker: Option[Cite2Urn], relation: Option[Cite2Urn] )
	case class GenericGraphedNode( node: GenericNode, children: Vector[GenericGraphedNode])

	case class SyntaxSentence( sentenceUrn: Cite2Urn, nodes: Vector[SyntaxNode]) {

		def toGenericGraph( marker: Option[Cite2Urn] = None, relation: Option[Cite2Urn] = None): Map[ Option[Cite2Urn], Vector[SyntaxNode]] = {
			// Group by headTokenUrn

			val grouped: Map[ Option[Cite2Urn], Vector[SyntaxNode]] = {
				nodes.groupBy( _.head)
			}

			grouped
						
		}

		def toJson: String = {

			// Group by headTokenUrn
			val grouped: Map[ Option[Cite2Urn], Vector[SyntaxNode]] = {
				nodes.groupBy( _.head)
			}

			// Start with those elements that have None as a head;
			// …these are dependents on Root.	
			val nodeStructure: String = {
				s"""
				nodeStructure: {
		        text: { name: "Root" },
		        children: [
		        	${ syntaxRecurseJson( grouped, None )}
		        ]
				  }
				""".replaceAll("\n"," ").replaceAll("\t"," ").replaceAll(" +"," ")
			}

			val divId: String = s"syntax-${utilities.urnToHtmlId(sentenceUrn)}"

			val template: String = s""" {
		    chart: {
		        container: "#${divId}",
		        rootOrientation: "NORTH",
		        scrollbar: "native",
		        levelSeparation: 60,
		        connectors: {
		        	type: "curve",
	            style: {
	                "stroke-width": 2,
	                "stroke": "navy",
	                "opacity": 0.3
	            }
		        }
		    },
		    SYNTAX_CONTENT_HERE
		 }"""

		  template.replaceAll("SYNTAX_CONTENT_HERE", nodeStructure )
		}

	}


	def syntaxRecurseJson( grouped: Map[ Option[Cite2Urn], Vector[SyntaxNode]], urnOpt: Option[Cite2Urn]): String = {
		grouped(urnOpt).map( n => {
			s"""
			{
				text: { 
					name: "${n.surfaceForm}",
					desc: "${n.relationLabel}"
				},
				children: [
				${
						val newU = n.tokenUrn
						if (grouped.contains( Some(newU)) ) syntaxRecurseJson(grouped, Some(newU))
						else ""
				 }
				]
			},
			"""
		}).mkString(" ")
	}


	


}