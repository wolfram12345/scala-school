package lectures.matching

import lectures.matching.SortingStuff._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов:
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста:
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то метод findMyKnife его отыщет.
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предыдущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen = bookGenerator.filter(_.isInteresting)
  val knifeGenerator = (needKnife: Boolean) => {
    if(needKnife) Some(Knife)
    else None
  }
  val bootsGenerator: Gen[Boots] = Gen.zip(Gen.alphaStr, Gen.choose(10, 60)).map(pair => Boots(pair._1, pair._2))

  // Override configuration if you need
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 10, maxSize = 20)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in{
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)
        goodBooks should have size books.size
        niceWatches should have size 0
        junk should have size watches.size
      }
    }
    "find knife" which {
      "was occasionally disposed" in{
        findMyKnife(sortJunk({
          val mayBeKnife = knifeGenerator(true)
          if(mayBeKnife.isDefined)
            List(mayBeKnife.get)
          else
            List()
        })) shouldBe true
        findMyKnife(sortJunk({
          val mayBeKnife = knifeGenerator(false)
          if(mayBeKnife.isDefined)
            List(mayBeKnife.get)
          else
            List()
        })) shouldBe false
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in{
        val ms = generatorDrivenConfig.minSuccessful
        val boots = (1 to ms) flatMap{_ => bootsGenerator.sample}
        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, needBoots, _) = SortingStuff.sortJunk(Random.shuffle(boots).toList)

        goodBooks.size shouldBe 0
        niceWatches should have size 0
        needBoots should have size 0

        val StuffBox(_, _, needBoots2, _) = SortingStuff.sortJunk(Random.shuffle(boots).toList :+ Boots("Converse"))

        needBoots2 should have size 1

        val StuffBox(_, _, needBoots3, _) = SortingStuff.sortJunk(Random.shuffle(boots).toList :+ Boots("Converse") :+ Boots("Adidas"))

        needBoots3 should have size 2
      }
    }
  }
}
