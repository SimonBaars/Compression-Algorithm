package nl.simonbaars.compression.bst

object BinarySearchTree {
  abstract class BinarySearchTree(weight: Int)
  case class ForkNode(left: BinarySearchTree, right: BinarySearchTree, chars: List[Char], weight: Int) extends BinarySearchTree(weight)
  case class LeafNode(char: Char, weight: Int) extends BinarySearchTree(weight)
}