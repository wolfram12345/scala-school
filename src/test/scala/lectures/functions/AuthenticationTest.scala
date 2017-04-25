package lectures.functions

import org.scalacheck.Gen
import org.scalatest.WordSpec

import scala.util.Random

/**
  * Авторизация - это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec {

  val cardUserCreds = CardCredentials(12345678)
  val cardUserCreds2 = CardCredentials(87654321)
  val authUserCreds = LPCredentials("qwerty", "qwerty")
  val authUserCreds2 = LPCredentials("qwerty2", "qwerty2")

  "authByCardTest" when{
    "noError" should {
      val p = Gen.delay()
//      val testUsersGen: Gen[User] = Gen.map(w => {
//        val randomValue = Random.nextInt(3)
//        if(randomValue == 0)
//          AnonymousUser()
//        else if(randomValue == 1)
//          LPUser()
//        else
//          CardUser()
//      }
    }

  }
}
