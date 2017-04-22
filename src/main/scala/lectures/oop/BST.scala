package lectures.oop


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def getDeep(bst : Option[BST], deep: Int): Int

  def fold(aggregator: Int)(f: (Int, Int) => Int): Int
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def getDeep(bst : Option[BST], deep: Int): Int = {
    if(bst.get.left.nonEmpty && bst.get.right.nonEmpty){
      val leftDeep = getDeep(bst.get.left, deep + 1)
      val rightDeep = getDeep(bst.get.right, deep + 1)
      if(leftDeep > rightDeep) leftDeep else rightDeep
    }
    else if(bst.get.left.nonEmpty && bst.get.right.isEmpty)
      getDeep(bst.get.left, deep + 1)
    else if(bst.get.right.nonEmpty && bst.get.left.isEmpty)
      getDeep(bst.get.right, deep + 1)
    else
      deep
  }



  def add(newValue: Int): BST = {
    addNode(Some(this), newValue).get
  }

  def copyNode(bst: Option[BSTImpl]): Option[BSTImpl] = {
    if(bst.isEmpty)
      None
    else
      Some(BSTImpl(bst.get.value, bst.get.left, bst.get.right))
  }

  def addNode(bst: Option[BSTImpl], newValue: Int): Option[BSTImpl] = {
    if(newValue < bst.get.value){
      if(bst.get.left.nonEmpty)
        Some(BSTImpl(bst.get.value, addNode(bst.get.left, newValue), copyNode(bst.get.right)))
      else
        Some(BSTImpl(bst.get.value, Some(BSTImpl(newValue)), copyNode(bst.get.right)))
    }
    else if(newValue > bst.get.value){
      if(bst.get.right.nonEmpty)
        Some(BSTImpl(bst.get.value, copyNode(bst.get.left), addNode(bst.get.right, newValue)))
      else
        Some(BSTImpl(bst.get.value, copyNode(bst.get.left), Some(BSTImpl(newValue))))
    }
    else
      Some(BSTImpl(bst.get.value, copyNode(bst.get.left), copyNode(bst.get.right)))
  }

  def fold(bst: Option[BST], aggregator: Int, f:(Int, Int) => Int): Int = {
    val thisAggregator = f(aggregator, bst.get.value)
    val leftAggregator = if(bst.get.left.nonEmpty) fold(bst.get.left, thisAggregator, f) else thisAggregator
    val rightAggregator = if(bst.get.right.nonEmpty) fold(bst.get.right, leftAggregator, f) else leftAggregator
    rightAggregator
  }

  def fold(aggregator: Int)(f: (Int, Int) => Int): Int = {
    fold(Some(this), aggregator, f)
  }

  def find(value: Int): Option[BST] = {
    find(value, Some(this))
  }

  def find(value: Int, bst: Option[BST]): Option[BST] = {
    if(value < bst.get.value){
      if(bst.get.left.nonEmpty){
        find(value, bst.get.left)
      }
      else{
        None
      }
    }
    else if(value > bst.get.value){
      if(bst.get.right.nonEmpty){
        find(value, bst.get.right)
      }
      else
        None
    }
    else
      bst
  }


  def replaceCharAtString(string: String, index: Int, char: Int): String = {
    if(index + 1 < string.length){
      string.substring(0, index) + char + string.substring(index + 1)
    }
    else
      new String(string)
  }

  def nodeToString(bst: Option[BST], deep: Int, array: Array[String], leftIndex: Int, rightIndex: Int) : Unit = {
    val indexToReplace = (leftIndex + rightIndex) / 2
    array(deep) = replaceCharAtString(array(deep), indexToReplace, bst.get.value)
    if(bst.get.left.nonEmpty)
      nodeToString(bst.get.left, deep + 1, array, leftIndex, indexToReplace)
    if(bst.get.right.nonEmpty)
      nodeToString(bst.get.right, deep + 1, array, indexToReplace + 1, rightIndex)
  }



  override def toString() = { // не очень красиво, но вроде работает
  val deep = getDeep(Some(this), 0)
    val numNodesOnLastLevel: Int = Math.pow(2, deep).toInt * deep
    val stringBuilder = new StringBuilder()
    for(i <- 0 until numNodesOnLastLevel)
      stringBuilder.append(" ")
    val templateString = stringBuilder.toString()
    val arrayOfStrings: Array[String] = new Array(deep + 1)
    for(i <- arrayOfStrings.indices){
      arrayOfStrings(i) = new String(templateString)
    }
    nodeToString(Some(this), 0, arrayOfStrings, 0, templateString.length - 1)

    stringBuilder.clear()
    for(str <- arrayOfStrings){
      stringBuilder.append(str)
      stringBuilder.append("\n\r")
    }
    stringBuilder.toString()
  }

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()
  sc.close()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = {
    val list = for(i<-0 until nodesCount)yield{
      (Math.random() * maxValue).toInt
    }
    list.foldLeft(root)((acc: BST, next: Int) => acc.add(next))
  }


  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)//это ведь так?
  require(testTree.find(markerItem3).isDefined)//


  println(testTree)


  val myTree = BSTImpl(5).add(3).add(8).add(2).add(4).add(7).add(9).add(10)
  println(myTree)
  println(myTree.fold(0)((m, n) => m + n))
}