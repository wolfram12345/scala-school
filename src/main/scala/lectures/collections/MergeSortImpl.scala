package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val lengthLeftSeq = data.length / 2
    val lengthRightSeq = data.length - lengthLeftSeq

    var leftSeq = data.take(lengthLeftSeq)
    var rightSeq = data.takeRight(lengthRightSeq)

    if(leftSeq.length > 1)
      leftSeq = mergeSort(leftSeq)
    if(rightSeq.length > 1)
      rightSeq = mergeSort(rightSeq)

    var leftIndex:Int = 0
    var rightIndex:Int = 0
    for(i <- data.indices) yield{
      if(leftIndex < leftSeq.length && rightIndex >= rightSeq.length){
        leftIndex = leftIndex + 1
        leftSeq(leftIndex - 1)
      } else if(leftIndex >= leftSeq.length && rightIndex < rightSeq.length){
        rightIndex = rightIndex + 1
        rightSeq(rightIndex - 1)
      } else{
        val leftValue = leftSeq(leftIndex)
        val rightValue = rightSeq(rightIndex)

        if(leftValue < rightValue){
          leftIndex = leftIndex + 1
          leftValue
        }
        else{
          rightIndex = rightIndex + 1
          rightValue
        }
      }
    }
  }

  def createArrayToTest(size: Int): Seq[Int] = {
    val random = scala.util.Random
    for(i <- 0 until size) yield {
      random.nextInt() % 100
    }
  }

  val array = createArrayToTest(10)

  println(array)

  val sortedArray = mergeSort(array)

  println(sortedArray)
}
