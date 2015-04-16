package cc.factorie.app.nlp.el



import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.variable.CategoricalVar


/**
 * User: dietz
 * Date: 4/16/15
 * Time: 2:34 PM
 */
object Document2XmlRenderer {

//  def sgmlString(doc: Document): String = {
//    val buf = new StringBuffer
//    for (section <- doc.sections; token <- section.tokens) {
//      if (token.isSentenceStart) buf.append("<sentence>")
//      token.startsSpans.foreach(span => buf.append("<" + span.name + ">"))
//      buf.append(token.string)
//      token.endsSpans.foreach(span => buf.append("</" + span.name + ">"))
//      if (token.isSentenceEnd) buf.append("</sentence>")
//      buf.append(" ")
//    }
//    buf.toString
//  }

  def xml(doc: Document) = {
    <root>
      <document>
        <name>
          {doc.name}
        </name>
        <tokens>
          {for (section <- doc.sections; token <- section.tokens) yield
          <token id={(token.position + 1).toString}>
            <word>
              {token.string}
            </word>
            <lemma>
              {token.string}
            </lemma>
            <POS>
              {getAttr(token, OntonotesForwardPosTagger.tokenAnnotationString)}
            </POS>
            <CharacterOffsetBegin>
              {token.stringStart}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {token.stringEnd}
            </CharacterOffsetEnd>
            <NER>
              {getAttr(token, NoEmbeddingsConllStackedChainNer.tokenAnnotationString)}
            </NER>
            <PARSE>
              {getAttr(token, OntonotesTransitionBasedParser.tokenAnnotationString)}
            </PARSE>
            <StartSentence>
              {token.isSentenceStart}
            </StartSentence>
          </token>}
        </tokens>
        <mentions>
          {for (m <- doc.attr[NounPhraseList]) yield
          <mention>
            <string>
              {m.string}
            </string>
            <type>
              {m.attr[NounPhraseType].categoryValue}
            </type>
            <CharacterOffsetBegin>
              {m.tokens.head.stringStart}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {m.tokens.last.stringEnd}
            </CharacterOffsetEnd>
            <TokenBegin>
              {m.tokens.head.position}
            </TokenBegin>
            <TokenEnd>
              {m.tokens.head.position + m.tokens.length}
            </TokenEnd>
          </mention>}
        </mentions>{if (doc.attr[WikiEntityMentions] != null) {
        <kblinks>
          {for (linkedMention <- doc.attr[WikiEntityMentions]) yield
          <entitylink>
            <name>
              {linkedMention.phrase.string}
            </name>
            <CharacterOffsetBegin>
              {linkedMention.phrase.tokens.head.stringStart}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {linkedMention.phrase.tokens.last.stringEnd}
            </CharacterOffsetEnd>
            <TokenBegin>
              {linkedMention.phrase.tokens.head.position}
            </TokenBegin>
            <TokenEnd>
              {linkedMention.phrase.tokens.head.position + linkedMention.phrase.tokens.length}
            </TokenEnd>{for (c <- linkedMention.entityLinks) yield
            <candidate>
              <id>
                {c.wikipediaTitle}
              </id>
              <rank>
                {c.rank}
              </rank>
              <score>
                {c.score}
              </score>
            </candidate>}
          </entitylink>}
        </kblinks>
      }}
      </document>
    </root>
  }

  def getAttr(token: Token, af: (Token) => Any): String = {
    af(token) match {
      case cv: CategoricalVar[String @unchecked] => cv.categoryValue.toString
      case null => ""
      case v: Any => v.toString
    }
  }


}
