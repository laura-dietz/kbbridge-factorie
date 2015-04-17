package cc.factorie.app.nlp.el

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.variable.CategoricalVar
import edu.umass.ciir.smartir.MergeBuffers
import edu.umass.ciir.smartir.MergeBuffers.Span

import scala.collection.mutable.ListBuffer


/**
 * User: dietz
 * Date: 4/16/15
 * Time: 2:34 PM
 */
object Document2DoubleTabRenderer {


  def doubleTab(doc: Document):Seq[String] = {
    val mergedTokensAnnotations = {
      val tokens =   for (section <- doc.sections; token <- section.tokens) yield Span(token.stringStart, token.stringEnd, token)
      val emty = tokens.head.entry
      val mentions =
        if(doc.attr[WikiEntityMentions] != null){
          for(linkedMention <- doc.attr[WikiEntityMentions]) yield Span(linkedMention.phrase.head.stringStart, linkedMention.phrase.last.stringEnd, linkedMention)
        } else Seq.empty

      MergeBuffers.mergeBuffers(emty, tokens,mentions.sortBy(_.spanBegin), Seq[Null]())
    }

    val result = new ListBuffer[String]()

    for(Tuple3(token, mentions, _) <- mergedTokensAnnotations) yield {
      val tokenId = token.position + 1
      val surface = token.position+"_"+token.string
      val partofspeech = getAttr(token, OntonotesForwardPosTagger.tokenAnnotationString)
      val charBegin = token.stringStart
      val charEnd = token.stringEnd
      val isStartOfSentence = token.isSentenceStart
      val nerType = getAttr(token, NoEmbeddingsConllStackedChainNer.tokenAnnotationString)
      val parseTree = getAttr(token, OntonotesTransitionBasedParser.tokenAnnotationString)


      val entityphrase = mentions.map(x => x.phrase.string+"_"+x.phrase.head.position+"_"+x.phrase.head.stringStart+"_"+x.phrase.last.stringEnd).mkString("\t")
      val mentionLen = mentions.map(_.phrase.tokens.size).mkString("\t")
      val wikititlesScore = mentions.flatMap(_.entityLinks.take(1).map(x => x.wikipediaTitle -> x.score))
      val (wikititles, entityScores) = wikititlesScore.unzip

      // todo categories

      if(isStartOfSentence) result += ""

      val line = Seq(surface, partofspeech,parseTree, nerType, s"$charBegin\t$charEnd", entityphrase, mentionLen, wikititles.mkString("\t"), entityScores.mkString("\t"))
      result += line.mkString("\t\t")
    }

    result
  }

  def getAttr(token: Token, af: (Token) => Any): String = {
    af(token) match {
      case cv: CategoricalVar[String @unchecked] => cv.categoryValue.toString
      case null => ""
      case v: Any => v.toString
    }
  }


}
