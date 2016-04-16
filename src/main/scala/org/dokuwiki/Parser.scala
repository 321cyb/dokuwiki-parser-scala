package org.dokuwiki

import scala.collection.mutable
import fastparse.all._

object Parser {
  type RootNode = List[BlockASTNode]

  var emptyLines = P((" " | "\t").rep ~ "\n").rep(min=1).!.map(_ => true)
  val nonNewLineChar = CharPred(_ != '\n')
  val nonNewLineSpaceChar = CharPred(ch => ch != '\n' && ch != ' ')
  val nonDotAsciiChars = CharPred(ch => ch.toInt < 127 && ch != '.' && ch != '\n')
  val digits = P(CharIn('0' to '9')).rep(min=1,max=4).!.map(_.toInt)
  val nonNLPipeChar = CharPred(ch => ch != '\n' && ch != '|')

  // Only handles cases when str.length >= 2
  def strWithout(str: String) = {
    var p = P(!str.substring(0, 1) ~ AnyChar)
    for(ahead <- 1 until str.length) {
      p = p | P(str.substring(0, ahead) ~ (!str.substring(ahead, ahead+1) ~ AnyChar) )
    }
    p.rep.!
  }

  val code = P ("<code>"  ~ strWithout("</code>") ~ "</code>").map(CodeNode)
  val file = P ("<file>"  ~ strWithout("</file>") ~ "</file>").map(FileNode)
  val html = P ("<html>"  ~ strWithout("</html>") ~ "</html>").map(HTMLNode)
  val nowiki = P ("<nowiki>"  ~ strWithout("</nowiki>") ~ "</nowiki>").map(NowikiNode)


  val bold = P( "**" ~ ((!"*" ~ nonNewLineChar) | ("*" ~ (!"*" ~ nonNewLineChar)) ).rep.! ~ "**").map(BoldNode)
  val italic = P( "//" ~ ((!"/" ~ nonNewLineChar) | ("/" ~ (!"/" ~ nonNewLineChar)) ).rep.! ~ "//").map(ItalicNode)
  val underline = P( "__" ~ ((!"_" ~ nonNewLineChar) | ("_" ~ (!"_" ~ nonNewLineChar)) ).rep.! ~ "__").map(UnderLineNode)
  val monospace = P( "''" ~ ((!"'" ~ nonNewLineChar) | ("'" ~ (!"'" ~ nonNewLineChar)) ).rep.! ~ "''").map(MonoSpaceNode)

  val charsBeforePipe = P((!"]" ~ nonNLPipeChar) | ("]" ~ (!"]" ~ nonNLPipeChar)) ).rep(min=1).!
  val charsAfterPipe = P((!"]" ~ nonNewLineChar) | ("]" ~ (!"]" ~ nonNewLineChar)) ).rep(min=1).!

  val link = P( "[[" ~ charsBeforePipe ~ ("|" ~ charsAfterPipe).? ~ "]]").map(LinkNode.tupled)

  /* This part is for media parser */
  val imageExtension = StringIn("gif", "jpg", "png")
  val mediaFileName = P(nonDotAsciiChars.rep(min=1,max=1024) ~ "." ~ imageExtension).!
  val imageSize = P("?" ~ digits ~ ("x" ~ digits).?).?
  val leftAlign = P(" ").!.?.map(align => align.map( _ => new AlignLeft))
  val rightAligh = P(" ").!.?.map(align => align.map( _ => new AlignRight))
  def toMediaNode(value: (Option[AlignLeft], String, Option[(Int, Option[Int])], Option[AlignRight])) : MediaNode = {
    val (left, uri, dimension, right) = value
    val alignType = (left, right) match {
      case (Some(_), _) => new AlignLeft
      case _ => new AlignRight
    }
    val (height, width) = dimension match {
      case Some(v) => (Some(v._1), v._2)
      case None => (None, None)
    }

    MediaNode(new ImageMedia, uri, "", alignType, height, width)
  }
  val media = P("{{" ~ leftAlign ~ mediaFileName ~  imageSize ~ rightAligh ~ "}}").map(toMediaNode)

  val specialInlines = P(
    code | file | html | nowiki | media | link | bold | italic | underline | monospace
  )

  def singleNormalText(inList: Boolean) = {
    if(inList) {
      P((!(specialInlines) ~ nonNewLineChar))
    } else {
      P((!(specialInlines) ~ nonNewLineChar) | ("\n" ~ &(header | listItem | specialInlines) ) | ("\n" ~ !(header | listItem | specialInlines) ~ nonNewLineChar))
    }
  }

  def normalText(inList: Boolean) = P(singleNormalText(inList)).rep(min=1).!.map(NormalText)

  def paragraph(inList: Boolean) = P((specialInlines | normalText(inList)).rep(min=1) ~ "\n" ~ emptyLines.?).map(captured => ParaNode(captured._1.toVector, captured._2.nonEmpty))


  /* BLOCK Node */
  val leftHashes = P("#".rep(min=1, max=6).!)
  def remainingHeader(hashes: String) = P (CharPred(ch => ch != '\n' && ch != '#').rep(min=1).! ~ hashes.! ~ "\n" ~ emptyLines.?)
  val header = P(leftHashes.flatMap(remainingHeader)).map(captured => HeaderNode(captured._2.length, captured._1.trim))

  val listItem: Parser[ListParserNode] = P("  ".rep(min=1, max=5).! ~ ("*" | "-").! ~ paragraph(true)).map(things => ListParserNode(things._1.length/2, if(things._2 == "-") true else false, things._3))

  val root = P (header | listItem | paragraph(false) ).rep



  /* START OF PARSER UTILITIES */

  def parse(input: String): Option[RootNode] = {
    val normalizedInput = input + "\n"
    val parserRootNode = root.parse(normalizedInput) match {
      case Parsed.Success(value, _) => Some(value)
      case _ => {println("Error!"); None}
    }

    // transform list item parser nodes to AST node,
    // we could do the same for header node, but it seems unnecessary.
    parserRootNode.map(transFormListItemParserNodeToASTNode)
  }

  def transFormListItemParserNodeToASTNode(parserRootNode: Seq[BlockParserNode]):RootNode = {
    parserRootNode.foldLeft(List[BlockASTNode]())((astNodeList:RootNode, parserNode) => {
      parserNode match {
        case listItem: ListParserNode => {
          if(astNodeList.length == 0) {
            appendNewListNode(listItem, astNodeList)
          } else {
            astNodeList.last match {
              case lastListNode: ListNode => addListParserNodeToListNode(listItem, lastListNode, astNodeList)
              case _ => appendNewListNode(listItem, astNodeList)
            }
          }
        }
        case header: HeaderNode => astNodeList :+ header
        case para: ParaNode => astNodeList :+ para
      }
    })
  }

  def addListParserNodeToListNode(listParserNode: ListParserNode, lastListNode: ListNode, astNodeList: RootNode):RootNode = {
    if(listParserNode.level == lastListNode.level && listParserNode.ordered == lastListNode.ordered) {
      if(lastListNode.items.last.isInstanceOf[ParaNode] && lastListNode.items.last.asInstanceOf[ParaNode].hasFollowingEmptyLines) {
        appendNewListNode(listParserNode, astNodeList)
      } else {
        lastListNode.items += (listParserNode.para)
        astNodeList
      }
    } else if(listParserNode.level > lastListNode.level){
      // dig deeper
      lastListNode.items.last match {
        case x: ListNode => addListParserNodeToListNode(listParserNode, x, astNodeList)
        case _ => appendNewListNode(listParserNode, astNodeList)
      }
    } else {
      appendNewListNode(listParserNode, astNodeList)
    }
  }

  def appendNewListNode(listParserNode: ListParserNode, astNodeList: RootNode): RootNode = {
    astNodeList :+ ListNode(listParserNode.level, listParserNode.ordered, mutable.ArrayBuffer(listParserNode.para))
  }
}
