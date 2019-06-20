package nl.simonbaars.compression.encoding

import nl.simonbaars.compression.bst.BinarySearchTree.BinarySearchTree
import nl.simonbaars.compression.bst.BinarySearchTree.ForkNode
import nl.simonbaars.compression.bst.BinarySearchTree.LeafNode
import nl.simonbaars.compression.bst.HuffmanTree
import nl.simonbaars.compression.decoding.HuffmanDecoder
import scala.collection.mutable.MutableList

object HuffmanEncoder {
  def encode(tree: BinarySearchTree)(text: List[Char]): List[Boolean] = {
    def searchCharacterCodeInBinarySearchTree(tree:  BinarySearchTree)(c: Char): List[Boolean] = tree match {
      case LeafNode(_, _) => List()
      case ForkNode(left, right, _, _) if HuffmanTree.chars(left).contains(c) => false :: searchCharacterCodeInBinarySearchTree(left)(c)
      case ForkNode(left, right, _, _) => true :: searchCharacterCodeInBinarySearchTree(right)(c)
    }
    text.flatMap(searchCharacterCodeInBinarySearchTree(tree))
  }
  
  def createBinaryRepresentation(tree: BinarySearchTree): MutableList[Boolean] = {
    binaryRepresentation(tree, MutableList[Boolean]())
  }
  
  def binaryRepresentation(tree:  BinarySearchTree, list : MutableList[Boolean]): MutableList[Boolean] =  {
    list+=false  
    tree match {
        case LeafNode(char, _) => {list+=false; list+=true;HuffmanDecoder.byte2Bools(char).reverse.foreach(f => list+=f);}
        case ForkNode(left, right, _, _) => {binaryRepresentation(left, list);binaryRepresentation(right, list);}
      }
    list+=true
      list
   }
  
  def bitsToString(bits : MutableList[Boolean]) : String = {
    bits.grouped(8)
    .map(_.foldRight(0)((b,i) => (i<<1) + (if(b) 1 else 0)).toChar)
    .mkString
  }
}