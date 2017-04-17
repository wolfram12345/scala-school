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

case class accum(courier: Courier, indexCanServe: Int, servedAddresses: List[Address])


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
//  def serveAddresses(addresses: List[Address], couriers: List[Courier]) = {
  //    var accum = 0
  //    for (courier <- couriers;
  //         trafficDegree = traffic().degree;
  //         t <- 0 to courier.canServe if trafficDegree < 5 && accum < addresses.length
  //    ) yield {
  //      val addr = addresses(accum)
  //      accum = accum + 1
  //      addr
  //    }
  //  }

  def serveAddresses(addresses: List[Address], couriers: List[Courier]) = {
    val nullAccum = accum(null, 0, List())
    addresses.foldLeft(nullAccum)((acc, address) =>{
      val courier = acc.courier
      val indexCanServe: Int = acc.indexCanServe
      val addressesList: List[Address] = acc.servedAddresses
      if(courier == null){
        val newCourier = getCourier(couriers.iterator)
        if(newCourier == null){
          accum(null, 0, addressesList)
        }
        else{
          val newIndexCanServe = 1
          accum(newCourier, newIndexCanServe, addressesList :+ address)
        }
      }
      else{
        if(indexCanServe < courier.canServe){
          accum(courier, indexCanServe + 1, addressesList :+ address)
        }
        else{
          val newCourier = getCourier(couriers.iterator)
          if(newCourier == null){
            accum(null, 0, addressesList)
          }
          else{
            val newIndexCanServe = 1
            accum(newCourier, newIndexCanServe, addressesList :+ address)
          }
        }
      }
    })
  }

  def getCourier(iterator: Iterator[Courier]): Courier = {
    if(iterator.hasNext){
      val courier = iterator.next()
      if(traffic().degree < 5 && courier.canServe > 0)
        courier
      else
        getCourier(iterator)
    }
    else
      null
  }


  def traffic(): Traffic = new Traffic(Math.random() * 10)

//  def printServedAddresses(addresses: List[Address], couriers: List[Courier]) =
//    for (a <- serveAddresses(addresses, couriers)) {
//      println(a.postIndex)
//    }

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]) = {
    val listServedAddresses = serveAddresses(addresses, couriers).servedAddresses
    listServedAddresses.map(address => println(address.postIndex))
  }

  printServedAddresses(addrs, cours)

}
