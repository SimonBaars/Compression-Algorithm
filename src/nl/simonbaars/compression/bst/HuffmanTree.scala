package nl.simonbaars.compression.bst

import nl.simonbaars.compression.bst.BinarySearchTree.BinarySearchTree
import nl.simonbaars.compression.bst.BinarySearchTree.ForkNode
import nl.simonbaars.compression.bst.BinarySearchTree.LeafNode

object HuffmanTree {
  def createBinarySearchTree(chars: List[Char]): BinarySearchTree = createForksTillOnlyOneTreeIsLeft( makeOrderedLeafList(createCharacterList(chars)) ).head
  
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[LeafNode] = {
    freqs.sortWith((f1,f2) => f1._2 < f2._2).map((f) => LeafNode (f._1, f._2))
  }


  def combineTwoCharactersIntoAFork(binarySearchTrees: List[BinarySearchTree]): List[BinarySearchTree] = binarySearchTrees match {
    case left :: right :: cs => (makeBinarySearchTree(left, right) :: cs).sortWith((t1, t2) => weight(t1) < weight(t2))
    case _ => binarySearchTrees
  }


  def createForksTillOnlyOneTreeIsLeft(binarySearchTree: List[BinarySearchTree]): List[BinarySearchTree] = {
    if (binarySearchTree.size==1) binarySearchTree
    else createForksTillOnlyOneTreeIsLeft( combineTwoCharactersIntoAFork(binarySearchTree) )
  }
  
   def weight(tree: BinarySearchTree): Int = tree match {
    case ForkNode(_,_,_,w) => w
    case LeafNode(_,w) => w
  }

  def chars(tree: BinarySearchTree): List[Char] = tree match {
    case ForkNode(_,_,cs,_) => cs
    case LeafNode(c,_) => List(c)
  }

  def makeBinarySearchTree(left: BinarySearchTree, right: BinarySearchTree) =
    ForkNode(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
  
   def createCharacterList(chars: List[Char]): List[(Char, Int)] = {
     def createCharacterMap(characterMap:Map[Char, Int], c:Char) = {
       val count = (characterMap get c).getOrElse(0) + 1
       characterMap + ((c, count))
     }

     (Map[Char,Int]() /: chars)(createCharacterMap).iterator.toList
   }
}