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


  def


   override def toString() = {
     val deep = getDeep(Some(this), 0)
     val numSymbols = if (deep == 0) 1 else deep * 2
     val stringBuilder = new StringBuilder()
     for(i <- 0 until numSymbols)
       stringBuilder.append(" ")
     val str = stringBuilder.toString()

     val lines = for(i <- 0 to deep) yield{
       new String(str)
     }

    val linesArray: Array[String] = lines.toArray


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


}