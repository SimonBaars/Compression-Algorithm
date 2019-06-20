package nl.simonbaars.compression.decoding

import nl.simonbaars.compression.bst.BinarySearchTree.BinarySearchTree
import nl.simonbaars.compression.bst.BinarySearchTree.ForkNode
import nl.simonbaars.compression.bst.BinarySearchTree.LeafNode
import nl.simonbaars.compression.bst.HuffmanTree
import scala.collection.mutable.Stack

object HuffmanDecoder {
	def decode(tree: BinarySearchTree, bits: List[Boolean]): List[Char] = {
	  def searchCharactersInBinarySearchTree(t: BinarySearchTree, b: List[Boolean]): List[Char] = t match {
      case LeafNode(ch, _) => if (b.isEmpty) List(ch) else ch :: searchCharactersInBinarySearchTree(tree, b)
      case ForkNode(l, r, _, _) => if (b.isEmpty) List() else if (b.head == false) searchCharactersInBinarySearchTree(l, b.tail) else searchCharactersInBinarySearchTree(r, b.tail)
    }
	  searchCharactersInBinarySearchTree(tree, bits)
	}
	
	def boolsToBinarySearchTree(bools: List[Boolean]): (List[Boolean], BinarySearchTree) = {
	  val stack = new Stack[BinarySearchTree] 
	  var counter = 0
	  var depth = 0
	  var slicePos = 0
	  while( counter < bools.length){
	    if(bools(counter)){
	      depth-=1;
	    } else {
	      depth+=1;
	    }
	    if(!bools(counter) && bools(counter+1)){
	       stack.push(LeafNode(List(bools(counter+2),bools(counter+3),bools(counter+4),bools(counter+5),bools(counter+6),bools(counter+7),bools(counter+8),bools(counter+9)).foldLeft(0)((i,b) => (i<<1) + (if(b) 1 else 0)).toChar, 0));
	       counter+=10
	       depth-=2;
	    } else if(bools(counter)){
	      val s1 = stack.pop
	      val s2 = stack.pop
	      stack.push(ForkNode(s2, s1, List(), 0))
	    }
	    
	    if(depth == 0){
	      slicePos = counter+1
	      counter+=bools.length;
	    }
	    counter+=1;
    }
    (bools.slice(slicePos, bools.size), stack.pop)
	}

  	def byte2Bools(b: Char): Seq[Boolean] =
  			0 to 7 map isBitSet(b)
  
  	def isBitSet(byte: Char)(bit: Int): Boolean =
  			((byte >> bit) & 1) == 1
}