package nl.simonbaars.compression

import java.io.File
import java.io.PrintWriter

import scala.collection.immutable.ListMap
import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue
import scala.io.Source
import scala.collection.mutable.MutableList

import nl.simonbaars.compression.bst.BinarySearchTree.BinarySearchTree
import nl.simonbaars.compression.bst.BinarySearchTree.ForkNode
import nl.simonbaars.compression.bst.BinarySearchTree.LeafNode
import nl.simonbaars.compression.bst.HuffmanTree
import nl.simonbaars.compression.encoding.HuffmanEncoder
import nl.simonbaars.compression.decoding.HuffmanDecoder

object Main {
  def main(args: Array[String]): Unit = {
    val inputFile = "C:\\Users\\Simon\\Downloads\\test.txt"//pi1000000
    val outputFile = "E:\\School\\ASD\\APP\\Beroepsproduct\\Paradigma's\\workspace\\CompressionAlgorithm\\output.txt"
    val outputFile2 = "E:\\School\\ASD\\APP\\Beroepsproduct\\Paradigma's\\workspace\\CompressionAlgorithm\\output2.txt"
    doHoffman(inputFile, outputFile, compress)
    doHoffman(outputFile, outputFile2, uncompress);
  }
  
  def doHoffman(inputFile: String, outputFile: String, method: (String, List[Char])=>String): Unit = {
    val file = Source.fromFile(inputFile)
    var fileString =  file.mkString
    var fileChars = fileString.toList
    
    val string = method(fileString, fileChars);
    
    val writer = new PrintWriter(new File(outputFile))
    writer.write(string)
    writer.flush()
  }
  
  def compress(fileString : String, fileChars : List[Char]) : String = {
    val binarySearchTree = HuffmanTree.createBinarySearchTree(fileChars)
    val bits = HuffmanEncoder.createBinaryRepresentation(binarySearchTree) ++ HuffmanEncoder.encode(binarySearchTree)(fileChars)
    HuffmanEncoder.bitsToString(bits)
  }
  
  def uncompress(fileString : String, fileChars : List[Char]) : String = {
    val bools = fileChars.flatMap(HuffmanDecoder.byte2Bools)
    val (splicedBools, binarySearchTree) = HuffmanDecoder.boolsToBinarySearchTree(bools)
    val characterList = HuffmanDecoder.decode(binarySearchTree, splicedBools)
    characterList.mkString
  } 
}