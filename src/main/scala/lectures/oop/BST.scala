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
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BST = {
    add(newValue, Some(this)).get
  }

  def add(newValue: Int, bst: Option[BSTImpl]): Option[BSTImpl] = {
    if(bst.get.left.isEmpty && bst.get.right.isEmpty){
      if(newValue < bst.get.value)
        Some(BSTImpl(bst.get.value, Some(BSTImpl(newValue)), None))
      else if(newValue > bst.get.value)
        Some(BSTImpl(bst.get.value, None, Some(BSTImpl(newValue))))
      else
        bst
    }
    else if(bst.get.left.isEmpty && bst.get.right.nonEmpty){
      if(newValue < bst.get.value)
        Some(BSTImpl(bst.get.value, Some(BSTImpl(newValue)), add(newValue, bst.get.right)))
      else if(newValue > bst.get.value)
        Some(BSTImpl(bst.get.value, None, add(newValue, bst.get.right)))
      else
        bst
    }
    else if(bst.get.right.isEmpty && bst.get.left.nonEmpty){
      if(newValue < bst.get.value)
        Some(BSTImpl(bst.get.value, add(newValue, bst.get.left), None))
      else if(newValue > bst.get.value)
        Some(BSTImpl(bst.get.value, add(newValue, bst.get.left), Some(BSTImpl(newValue))))
      else
        bst
    }
    else{
        Some(BSTImpl(bst.get.value, add(newValue, bst.get.left), add(newValue, bst.get.right)))
    }
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


  // override def toString() = ???

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