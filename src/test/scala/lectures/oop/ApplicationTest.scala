package lectures.oop

import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpec}

/**
  * Раскомментируйте и допишите тесты на
  * класс lectures.oop.Application
  */
class ApplicationTest extends WordSpec with Matchers{

  private val started = new AfterWord("started")

  "Application" should {
    "return correct answer" when started{
      "in a test environment" in {
        val appGeneratorTest: Gen[Application] = Gen.const(new Application(true))
        val sample = appGeneratorTest.sample
        if(sample.isDefined)
          sample.get.doTheJob() shouldBe 5
        //??? shouldBe 5
      }
      "in a production environment" in {
      //   ??? shouldBe 2
        val appGeneratorProduction: Gen[Application] = Gen.const(new Application(false))
        val sample = appGeneratorProduction.sample
        if(sample.isDefined)
          sample.get.doTheJob() shouldBe 2
      }
    }
  }
}
