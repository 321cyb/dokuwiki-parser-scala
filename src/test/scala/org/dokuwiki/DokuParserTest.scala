package org.dokuwiki

import org.scalatest.FlatSpec

class DokuParserSpec extends FlatSpec {
  "Dokuwiki Parser" should "recognize section header correctly" in {
    val rootNode = Parser.parse("### def ###\n\n\n\nabc")
    val listOfNodes = rootNode.get
    assertResult(2)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[HeaderNode])
    assertResult("def")(listOfNodes(0).asInstanceOf[HeaderNode].title)
    assertResult(true)(listOfNodes(1).isInstanceOf[ParaNode])
    assertResult("abc")(listOfNodes(1).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
  }


  it should "recognize ordered list item" in {
    val rootNode = Parser.parse("  - all right, I think this is pretty great, right?")
    val listOfNodes = rootNode.get
    assertResult(1)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[ListNode])
    val listNode = listOfNodes(0).asInstanceOf[ListNode]
    assertResult(1)(listNode.level)
    assertResult(true)(listNode.ordered)
    assertResult(" all right, I think this is pretty great, right?")(listNode.items(0).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
  }

  it should "recognize bold text" in {
    val rootNode = Parser.parse("all **this** wrong thing is //wonderful//, **what do you say**?")
    val listOfNodes = rootNode.get
    assertResult(1)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[ParaNode])
    val paraNode = listOfNodes(0).asInstanceOf[ParaNode]
    val boldNodes = paraNode.inlineNodes.filter(x => x.isInstanceOf[BoldNode]).asInstanceOf[Vector[BoldNode]]
    assertResult(2)(boldNodes.length)
    assertResult("this")(boldNodes(0).text)
    assertResult("what do you say")(boldNodes(1).text)
    val italicNodes = paraNode.inlineNodes.filter(x => x.isInstanceOf[ItalicNode]).asInstanceOf[Vector[ItalicNode]]
    assertResult(1)(italicNodes.length)
    assertResult("wonderful")(italicNodes(0).text)
  }

  it should "recognize code tag" in {
    val rootNode = Parser.parse("This is a new paragraph, now, let's see the following code snippet: \n<code>\nint i = 0;\nint j = i;\nprintln(j);\n</code>\nThis is not a new paragraph.")
    val listOfNodes = rootNode.get
    assertResult(1)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[ParaNode])
    val paraNode = listOfNodes(0).asInstanceOf[ParaNode]
    val codeNodes = paraNode.inlineNodes.filter(x => x.isInstanceOf[CodeNode]).asInstanceOf[Vector[CodeNode]]
    assertResult(1)(codeNodes.length)
    assertResult("\nint i = 0;\nint j = i;\nprintln(j);\n")(codeNodes(0).text)
  }

  it should "handle newline between list items" in {
    val rootNode = Parser.parse("  - item1 \n  - item2\n  - item3")
    val listOfNodes = rootNode.get
    assertResult(1)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[ListNode])
    val listNode = listOfNodes(0).asInstanceOf[ListNode]
    assertResult(3)(listNode.items.length)
    assertResult(" item1 ")(listNode.items(0).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
    assertResult(" item2")(listNode.items(1).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
    assertResult(" item3")(listNode.items(2).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
  }

  it should "handle empty lines between list items" in {
    val rootNode = Parser.parse("  - item1 \n\n  - item2\n  - item3")
    val listOfNodes = rootNode.get
    assertResult(2)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[ListNode])
    assertResult(" item1 ")(listOfNodes(0).asInstanceOf[ListNode].items(0).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
    assertResult(true)(listOfNodes(1).isInstanceOf[ListNode])
    assertResult(" item2")(listOfNodes(1).asInstanceOf[ListNode].items(0).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
    assertResult(" item3")(listOfNodes(1).asInstanceOf[ListNode].items(1).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
  }

  it should "separate ordered and un-ordered list items" in {
    val rootNode = Parser.parse("  - item1 \n  - item2\n  * item3")
    val listOfNodes = rootNode.get
    assertResult(2)(listOfNodes.length)
    assertResult(true)(listOfNodes(0).isInstanceOf[ListNode])
    assertResult(" item1 ")(listOfNodes(0).asInstanceOf[ListNode].items(0).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
    assertResult(" item2")(listOfNodes(0).asInstanceOf[ListNode].items(1).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
    assertResult(true)(listOfNodes(1).isInstanceOf[ListNode])
    assertResult(" item3")(listOfNodes(1).asInstanceOf[ListNode].items(0).asInstanceOf[ParaNode].inlineNodes(0).asInstanceOf[NormalText].text)
  }


}
