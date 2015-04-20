/**
 * Created by apgalush on 4/14/2015.
 */


import org.cscie54.a3.{RealEstateListingsImpl, RealEstateListings}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.ConcurrentModificationException
import org.scalatest.FlatSpec
import org.scalatest.FunSuite
import org.cscie54.a3.warmup._
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer

class Testing extends FunSuite{


  // first create a listing

  // get the price and verify if it's valid


  // threads:


  // how to test for dead lock

  // give the write load

  def thread(body: =>Unit): Thread =
  {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }

  test("test Account")  {
    val a1 = new Account()

    val t1 = thread (a1.adjustBalance(1))
    val t2 = thread (a1.adjustBalance(2))
    val t3 = thread (a1.adjustBalance(3))
    val t4 = thread (a1.adjustBalance(4))
    val t5 = thread (a1.adjustBalance(5))

    t1.join();t2.join();t3.join();t4.join();t5.join()
    assert(a1.getBalance() === 15)

  }

  test("test Account2")  {
    val a1 = new Account2()

    val t1 = thread (a1.adjustBalance(1))
    val t2 = thread (a1.adjustBalance(2))
    val t3 = thread (a1.adjustBalance(3))
    val t4 = thread (a1.adjustBalance(4))
    val t5 = thread (a1.adjustBalance(5))

    t1.join();t2.join();t3.join();t4.join();t5.join()
    assert(a1.getBalance() === 15)

  }

  test("prove deadlock Account2")
  {
    for ( i <- 0 until 100)
    {
      val a1 = new Account2()
      val a2 = new Account2()

      val t1 = thread(a1.adjustBalance(100))
      val t2 = thread(a2.adjustBalance(100))
      val t3 = thread(a1.transferTo(a2, 100))
      t1.join(); t2.join(); t3.join()

      assert(a1.getBalance() === 0)
      assert(a2.getBalance() === 200)
    }
}


  test("test Join")
  {

    val join = new Join()

    val testJoin1 = (x: Unit) => println("I'm an anonymous function")
    val testJoin2 = (x: Unit) => println("I'm an anonymous function")
    val testJoin3 = (x: Unit) => println("I'm an anonymous function")

    join.join(List{testJoin1;testJoin2;testJoin3})

  }


  test("test Rectangle")
  {

    val rectangle = new Rectangle(2, 4)
    val t1 = thread (rectangle.getArea())
    val t2 = thread (rectangle.getArea())
    val t3 = thread (rectangle.getArea())

    t1.join();t2.join();t3.join()

    assert (rectangle.getArea() === 8)
  }

  test("test TotalOrder2")
  {
    val totalOrder = TotalOrder2

    val threadList: ListBuffer[Thread] = ListBuffer()
    for (i<-0 until 150)
    {
      threadList+=thread (totalOrder.next())
    }

    threadList.foreach(_.join())

    assert (totalOrder.getValue === 150)
  }
/*
  test("testing RealEstate")
  {
    val realEstate: RealEstateListings = new RealEstateListingsImpl(5)

    val t1 = thread (realEstate.addListing("165 Bridle", 300))
    val t2 = thread (realEstate.addListing("167 Bridle", 200))
    val t3 = thread (realEstate.addListing("1023 Beacon", 900))
    val t4 = thread (realEstate.addListing("1024 Beacon", 950))

    t1.join();t2.join();t3.join();t4.join()

    assert (realEstate.getTotalNumber === 4)
    assert (realEstate.getTotalValue === 2350)
  }
 */
  test("testing RealEstate 2")
  {
    val realEstate: RealEstateListings = new RealEstateListingsImpl(3)

    val t1 = thread (realEstate.addListing("165 Bridle", 300))
    val t2 = thread (realEstate.addListing("167 Bridle", 200))
    val t3 = thread (realEstate.addListing("1023 Beacon", 900))
    val t4 = thread (realEstate.addListing("1024 Beacon", 950))

    t1.join();t2.join();t3.join();t4.join()

    assert (realEstate.getTotalNumber === 3)
    assert (realEstate.getTotalValue === 2350)
  }

  /*
  def testAccountUnsafe(): Unit = {
    val a2 = new Account2()
    try {
      val range = (1 to 1000)
      range.par.foreach { i =>
        // if this method is not thread-safe it should throw an error
        a2.adjustBalance(1)
      }
      throw new Exception("This should have worked")
    }
    // catch ConcurrencyExceptions thrown in adjustBalance()
    catch {
      case e: ConcurrencyException =>
    }
  }


  def testAccount2(): Unit = {
    val a1 = new Account2()
    val range = (1 to 1000)
    range.par.foreach { i =>
      // if this method is not thread-safe this will sometimes throw an error
      a1.adjustBalance(1)
    }
  }


    def testJoin(): Unit = {
      import scala.util.Sorting

      val dumpster = new CopyOnWriteArrayList[Int]
      val tasks: Traversable[Unit => Unit]  = {
        val temp = new ListBuffer[Unit=>Unit]
        for (i<- 0 to 99){
          temp.append(_ => for (_ <- 0 to 99)dumpster.add(i) )
        }
        temp
      }

      val j = new Join()
      j.join(tasks)
      //    println(dumpster.toString())
      //    println(dumpster.size)
      assert(dumpster.toArray != dumpster.toArray.sorted)

    }

  */
  //testAcccount()
  //testAccountUnsafe()
  //testAccount2()
  //testJoin()

}
