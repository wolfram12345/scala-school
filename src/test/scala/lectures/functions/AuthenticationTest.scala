package lectures.functions

import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

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
class AuthenticationTest extends WordSpec with PropertyChecks with Matchers{

  val cardUserGenerator: Gen[CardUser] = Gen.const(CardUser())
  val lpUserGenerator: Gen[LPUser] = Gen.const(LPUser())
  val lpAndCardUsersGenerator: Gen[User] = Gen.const({
    if(Random.nextBoolean())
      LPUser()
    else
      CardUser()
  })


  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10, maxSize = 40)

  "authByCardTest" should{
    "firstTestAuthByCard" in{
      val ms = generatorDrivenConfig.maxSize
      val cardUsersWithoutCardUserCreads = (1 to ms) flatMap(_ => cardUserGenerator.sample)
      cardUsersWithoutCardUserCreads.filter(cardUser => Authentication.authByCard.lift(cardUser).isDefined) should have size 0
    }
    "secondTestAuthByCard" in{
      val ms = generatorDrivenConfig.minSuccessful
      val cardUsersWithCardUserCreads = ((1 to ms) flatMap(_ => cardUserGenerator.sample)) :+ CardUser(1234, AuthenticationData.cardUserCreds)
      cardUsersWithCardUserCreads.filter(cardUser => Authentication.authByCard.lift(cardUser).isDefined) should have size 1
    }

  }

  "authByLPtest" should{
    "firstTestAuthByLP" in{
      val ms = generatorDrivenConfig.maxDiscarded
      val lpUsersWithoutLPCredentials = (1 to ms) flatMap(_ => lpUserGenerator.sample)
      lpUsersWithoutLPCredentials.filter(lpUser => Authentication.authByLP.lift(lpUser).isDefined) should have size 0
    }
    "secondTestAuthByLP" in{
      val ms = generatorDrivenConfig.workers
      val lpUsersWithLPCredentials = ((1 to ms) flatMap(_ => lpUserGenerator.sample)) :+ LPUser(5678, AuthenticationData.authUserCreds)
      lpUsersWithLPCredentials.filter(lpUser => Authentication.authByLP.lift(lpUser).isDefined) should have size 0
    }
  }

  "composeTest" in{
    val countUsers = Random.nextInt(100) + 10
    val users = (1 to countUsers) flatMap(_ => lpAndCardUsersGenerator.sample)
    users.filter(user => Authentication.authByLP.orElse(Authentication.authByCard).lift(user).isDefined) should have size 0
    (users :+ CardUser(1234, AuthenticationData.cardUserCreds)).filter(user => Authentication.authByLP.orElse(Authentication.authByCard).lift(user).isDefined) should have size 1
  }
}
