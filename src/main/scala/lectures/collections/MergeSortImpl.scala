package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  private def merge(data1: Seq[Int], data2: Seq[Int], outputData: Seq[Int]): Seq[Int] = {
    (data1.headOption, data2.headOption) match {
      case (Some(a), Some(b)) if a < b => merge(data1.tail, data2, outputData :+ a)
      case (Some(a), Some(b)) if a >= b => merge(data1, data2.tail, outputData :+ b)
      case (Some(a), None) => outputData ++ data1
      case (None, Some(b)) => outputData ++ data2
      case (None, None) => outputData
    }
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {

    val separatedLists = data.splitAt(data.length / 2)

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
