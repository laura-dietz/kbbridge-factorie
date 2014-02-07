package cc.factorie.app.nlp.el

import cc.factorie.app.nlp.{Token, Document, DocumentAnnotator}
import cc.factorie.app.nlp.ner.{ BilouConllNerTag}
import scala.Predef._
import edu.umass.ciir.kbbridge.text2kb.KnowledgeBaseCandidateGenerator
import edu.umass.ciir.kbbridge.RankLibReranker
import edu.umass.ciir.kbbridge.util.{NameDictionary, ConfInfo, KbBridgeProperties}
import edu.umass.ciir.kbbridge.data.SimpleEntityMention
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.nlp.TextNormalizer
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.slf4j.Logging
import cc.factorie.app.nlp.coref.mention.{MentionEntityType, Mention, MentionType, MentionList}

/**
 * User: jdalton
 * Date: 6/12/13
 */



object KbBridgeEntityLinking extends DocumentAnnotator with Logging {

  lazy val candidateGenerator = KnowledgeBaseCandidateGenerator()
  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

  override def tokenAnnotationString(token:Token): String = {

    val matchingingMention = token.document.attr[WikiEntityMentions].filter(e => e.mention.contains(token))

    matchingingMention match {
      case wi:Seq[WikiEntity] if wi.length > 0 => wi.map(m => (m.entityLinks take 3).map(e => e.wikipediaTitle + ":" + "%3f".format(e.score)).mkString(",")).mkString(",")
      case _ => " "
    }
//      m.attr[MentionType].categoryValue+":"+m.span.indexOf(token)).mkString(","); case _ => "_" }
//
//
//    val entities = token.attr[WikiEntityMentions]
//    val entString = if (entities != null) {
//      entities.head.entityLinks.head.wikipediaTitle
//    } else {
//      "NIL"
//    }
//    entString
  }

  def prereqAttrs: Iterable[Class[_]] = List(classOf[BilouConllNerTag], classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[WikiEntityMentions])
  def process(doc:Document): Document = {

    val mentionsToReturn = new WikiEntityMentions

    val neighbors = doc.attr[MentionList]

    val namedMentions = neighbors.filter(m => {
      val mType = m.attr[MentionType].categoryValue
      (mType equals "NAM")
    })


    val allNers = namedMentions.map(m =>  {
      val eTypeAttr = m.attr[MentionEntityType]
      val eType = if (eTypeAttr != null) {
        eTypeAttr.categoryValue
      } else {
        "UNK"
      }

      val charStart =   m.tokens.head.stringStart
      val charEnd = m.tokens.last.stringEnd
      val tokenStart = m.tokens.head.position
      val tokenEnd = tokenStart + m.tokens.length

      new NlpXmlNerMention(m.string, Seq(), -1, false, tokenStart, tokenEnd, charStart, charEnd, eType)
    }
    )

    val text = doc.tokens.map(t => t.string).mkString(" ")

    val groupedMentions = namedMentions.groupBy(m => cleanMentionString(m))
    val filtered = groupedMentions.filterKeys(m => (m.length > 1) && (m.length < 100)).filterKeys(!NameDictionary.DAY_MONTH_NAMES.contains(_))
    println("Num mentions: " + groupedMentions.size + " filtered: " + filtered.size + " limit: 250.")
    val limited = filtered take 250
    mentionsToReturn ++= limited.map(m => linkEntity(m._2, doc, text, allNers)).flatten
//    mentionsToReturn ++= neighbors.map(m => linkEntity(Seq(m), doc, text, allNers)).flatten

    doc.attr +=  mentionsToReturn

    //doc.attr[WikiEntityMentions]
    doc
  }

  def cleanMentionString(mention : Mention) = {
    val cleanTokens = new ListBuffer[String]

    val mType = mention.attr[MentionType].categoryValue
    val tokens = mType match {
      case "NOM" => mention.tokens.filter(t => t.posTag.isNoun)
     // case "NAM" => mention.span.tokens.filter(t => t.posLabel.categoryValue.toUpperCase.startsWith("NN"))
      case _ => mention.tokens
    }

   for (token <- tokens) {
      val normalToken = TextNormalizer.normalizeText(token.string).replace(" ", "")
      if (normalToken.length() > 0) {
        cleanTokens += normalToken
      }
  }

  val query = cleanTokens.mkString(" ")
  query.replace(".","")
  }

  def linkEntity(mentions : Seq[Mention], d: Document, text:String, allNers: Seq[NlpXmlNerMention]) : Seq[WikiEntity] = {

    val mention = mentions.head
    val eTypeAttr = mention.attr[MentionEntityType]
    val eType = if (eTypeAttr != null) {
      eTypeAttr.categoryValue
    } else {
      "UNK"
    }

    val start =   mention.tokens.head.stringStart
    val end = mention.tokens.last.stringEnd


    val bridgeMention = new SimpleEntityMention(d.name, eType, (d.name +"_s"+ (start) +"-" +(end)),
      entityName=cleanMentionString(mention), fullText=text, corefChain=Seq(), nerNeighbors=allNers, groundTruth="")

    println("Fetching candidates for mention: " + bridgeMention.mentionId + " d:" + bridgeMention.docId + " name:" + bridgeMention.entityName)
    val candidates = candidateGenerator.retrieveCandidates(bridgeMention, 50)

    val t0 = System.currentTimeMillis
    val rerankedResults = reranker.rerankCandidatesGenerateFeatures(bridgeMention, candidates).toSeq
    val t1 = System.currentTimeMillis()
    val diff = t1-t0
    println(s"Reranking time: $diff")
    mentions.map(m => WikiEntity(m, rerankedResults))
  }
}
