package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  private def merge(data1: Seq[Int], data2: Seq[Int], outputData: Seq[Int]): Seq[Int] = {
    data1 match {
      case headData1 :: tailData1 => data2 match {
        case headData2 :: tailData2 => {
          if(headData1 < headData2)
            merge(tailData1, data2, outputData :+ headData1)
          else
            merge(data1, tailData2, outputData :+ headData2)
        }
        case _ => merge(tailData1, Nil, outputData :+ headData1)
      }
      case _ => data2 match{
        case headData2 :: tailData2 => merge(Nil, tailData2, outputData :+ headData2)
        case _ => outputData
      }
    }
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {

    val separateLength = data.length / 2
    val separatedLists = data.splitAt(separateLength)

    val leftSeq = if(separatedLists._1.length > 1) mergeSort(separatedLists._1) else separatedLists._1
    val rightSeq = if(separatedLists._2.length > 1) mergeSort(separatedLists._2) else separatedLists._2

    merge(leftSeq, rightSeq, Seq())
  }

  def createListToTest(size: Int) = {
    val random = scala.util.Random
    (for(i <- 0 until size) yield {
      random.nextInt() % 100
    }).toList
  }

  //работает только со списками
  //"Попробуй написать его без ручного итерирования, с применением иммутабельных списков и pattern matching."

  val array = createListToTest(10)

  println(array)

  val sortedArray = mergeSort(array)

  println(sortedArray)
}
