package lectures.collections.comprehension

/**
  * Помогите курьерам разобраться с обслуживанием адресов
  *
  * Каждый день на работу выходит 'courierCount' курьеров
  * Им нужно обслужить 'addressesCount' адресов
  * Каждый курьер может обслужить courier.canServe адресов, но только при условии, что позволит дорожная ситуация.
  * Т.е. если trafficDegree < 5, то курьер обслужит все адреса, которые может, иначе - ни одного
  *
  * Входные данные для приложения содержат 2 строки
  * В первой строке - количество адресов, которые требуется обслужить
  * Во второй - количество курьеров, вышедших на работу.
  *
  * Ваша задача:
  *  Изучить код и переписать его так,
  *  что бы в нем не было ни одного цикла for, ни одной переменной или мутабильной коллекции
  *
  * Для этого используйте функции комбинаторы: filter, withFilter, fold, map, flatMap и т.д.
  *
  */

case class Traffic(degree: Double)

object Courier {
  def couriers(courierCount: Int): List[Courier] =
    (for (i <- 1 to courierCount) yield {
      Courier(i)
    }).toList
}

case class Courier(index: Int) {
  val canServe = (Math.random() * 10).toInt
}

object Address {
  def addresses(addressesCount: Int): List[Address] =
    (for (i <- 1 to addressesCount) yield {
      Address(s"$i$i$i")
    }).toList
}

case class Address(postIndex: String)

object CouriersWithComprehension extends App {

  import Address._
  import Courier._

  val sc = new java.util.Scanner(System.in)
  val addressesCount = sc.nextInt()
  val courierCount = sc.nextInt()
  val addrs = addresses(addressesCount)
  val cours = couriers(courierCount)
  sc.close()


  // какие адреса были обслужены

  def serveAddresses(addresses: List[Address], couriers: List[Courier]) = {
<<<<<<< HEAD
    var accum = 0
    for (courier <- couriers;
         trafficDegree = traffic().degree;
         t <- 0 until courier.canServe if trafficDegree < 5 && accum < addresses.length
    ) yield {
      val addr = addresses(accum)
      accum = accum + 1
      addr
    }
=======
    val countAddresses = couriers.foldLeft(0)((count, courier) => {
      if(traffic().degree < 5)
        count + courier.canServe
      else
        count
    })
    addresses.take(countAddresses)
>>>>>>> 5964a61f372a60f1bcac2fd91438bed8342b2bee
  }




  def traffic(): Traffic = new Traffic(Math.random() * 10)

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]) = {
    serveAddresses(addresses, couriers).foreach(address => println(address.postIndex))
  }

  printServedAddresses(addrs, cours)

}
