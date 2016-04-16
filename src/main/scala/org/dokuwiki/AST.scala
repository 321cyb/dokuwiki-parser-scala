package org.dokuwiki

import scala.collection.mutable

trait ASTNode
trait BlockParserNode extends ASTNode
trait BlockASTNode extends ASTNode
trait InlineNode extends ASTNode

trait ListItemNode extends BlockASTNode

sealed trait AlignType
final case class AlignLeft() extends AlignType
final case class AlignRight() extends AlignType


sealed trait MediaType
final case class ImageMedia() extends MediaType


// Just a normal paragraph
case class ParaNode(inlineNodes : Vector[InlineNode], hasFollowingEmptyLines: Boolean) extends BlockParserNode with ListItemNode
case class ListParserNode(level: Int, ordered: Boolean, para: ParaNode) extends BlockParserNode
case class HeaderNode(level: Int, title: String) extends BlockParserNode with BlockASTNode

// List item can be ParaNode  or another ListNode
case class ListNode(level: Int, ordered: Boolean, items: mutable.ArrayBuffer[ListItemNode]) extends ListItemNode



case class MediaNode(mediaType: MediaType, resouce: String, title: String, align: AlignType, height: Option[Int], widht: Option[Int]) extends InlineNode
case class NowikiNode(text: String) extends InlineNode
case class CodeNode(text: String) extends InlineNode
case class FileNode(text: String) extends InlineNode
case class HTMLNode(text:String) extends InlineNode // only support inline html node now.

case class BoldNode(text: String) extends InlineNode
case class ItalicNode(text: String) extends InlineNode
case class UnderLineNode(text: String) extends InlineNode
case class MonoSpaceNode(text: String) extends InlineNode

// Do not recognize auto links, because they are also supported by markdown.
case class LinkNode(url: String, title: Option[String]) extends InlineNode

case class NormalText(text: String) extends InlineNode