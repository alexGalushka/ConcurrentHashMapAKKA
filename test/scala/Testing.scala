/**
 * Created by apgalush on 4/14/2015.
 */


import org.cscie54.a3.{RealEstateListingsImpl, RealEstateListings}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.ConcurrentModificationException
import org.scalatest.FlatSpec
import org.scalatest.FunSuite
import org.cscie54.a3.warmup._
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

class Testing extends FunSuite with Matchers with ScalaFutures{

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

  test("normal positive testing of RealEstate")
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

  test("in sequence edge case negative testing of RealEstate")
  {
    //for (i<- 0 until 200) {
      val realEstate: RealEstateListings = new RealEstateListingsImpl(3)

      realEstate.addListing("165 Bridle", 300)
      //println(t1.getId)
      realEstate.addListing("167 Bridle", 200)
      //println(t2.getId)
      realEstate.addListing("1023 Beacon", 900)
      //println(t3.getId)
      realEstate.addListing("1024 Beacon", 950)
      //println(t4.getId)
      //println(i)
      assert(realEstate.getTotalNumber === 3)
    //}
  }

  test("with futures: edge case negative testing of RealEstate")
  {

    val realEstate: RealEstateListings = new RealEstateListingsImpl(3)

    val tasks: Seq[Future[Try[Unit]]] = for (i <- 1 to 4) yield Future {
      realEstate.addListing("16" + i.toString + " Bridle", 100 * i)
    }

    val aggregated: Future[Seq[Try[Unit]]] = Future.sequence(tasks)

    //val squares: Seq[Try[Unit]] = Await.ready(aggregated, Duration.Inf).value.get match {
    //  case Success(result) => result
    //}

    whenReady(aggregated) { result =>
      assert(realEstate.getTotalNumber === 3)
      assert((realEstate.getAllSortedByPrice.toList(0)._2 < realEstate.getAllSortedByPrice.toList(1)._2) === true )
      assert(realEstate.getAllSortedByPrice.size === 3 )
      assert(realEstate.getCurrentPrice("161 Bridle") === Success(100))
      //assert(realEstate.getCurrentPrice("160 Bridle") === Failure(new IllegalArgumentException))
      //an [IllegalArgumentException] should be thrownBy {realEstate.getCurrentPrice("160 Bridle")}
      //val thrown = intercept[Exception] {realEstate.getCurrentPrice("160 Bridle")}
      //assert(thrown.getMessage === "Get Price Exception")

      val failureMessage = realEstate.getCurrentPrice("160 Bridle") match
      {
        case Failure (failE) => failE.toString
      }
      assert(failureMessage === "java.lang.IllegalArgumentException")

    }

  }


  test("with futures: removeListing of RealEstate")
  {
    val realEstate: RealEstateListings = new RealEstateListingsImpl(4)

    val tasks: Seq[Future[Try[Unit]]] = for (i <- 1 to 4) yield Future {
      if(i==2)
      {
        realEstate.removeListing("161 Bridle")
      }
      realEstate.addListing("16" + i.toString + " Bridle", 100 * i)
    }

    val aggregated: Future[Seq[Try[Unit]]] = Future.sequence(tasks)

    whenReady(aggregated) { result =>
      assert(realEstate.getTotalNumber === 3)
    }
  }

  test("with futures: updatePrice of RealEstate")
  {
    val realEstate: RealEstateListings = new RealEstateListingsImpl(4)

    val tasks: Seq[Future[Try[Unit]]] = for (i <- 1 to 4) yield Future {
      if(i==2)
      {
        realEstate.updatePrice("161 Bridle", 700)
      }
      realEstate.addListing("16" + i.toString + " Bridle", 100 * i)
    }

    val aggregated: Future[Seq[Try[Unit]]] = Future.sequence(tasks)

    whenReady(aggregated) { result =>
      assert(realEstate.getCurrentPrice("161 Bridle") === Success(700))
    }
  }

  test("with futures: <backlog to lisitings> of RealEstate")
  {
    val realEstate: RealEstateListings = new RealEstateListingsImpl(3)

    val testdata: ListBuffer[(String, Int)] = new ListBuffer()
    val tasks: Seq[Future[Try[Unit]]] = for (i <- 1 to 4) yield Future {
      if(i==2)
      {
        realEstate.removeListing("161 Bridle")
      }
      val tupleData = ("16" + i.toString + " Bridle", 100 * i)
      testdata.+=(tupleData)
      realEstate.addListing(tupleData._1, tupleData._2)
    }

    val aggregated: Future[Seq[Try[Unit]]] = Future.sequence(tasks)

    whenReady(aggregated) { result =>
      val listingTupleList = realEstate.getAllSortedByPrice.toList
      val listingTuple = realEstate.getAllSortedByPrice.toList(1) // listing to remove
      realEstate.removeListing(listingTuple._1) //after removing the above listing backlog should push entry to listings

      val excludedListings = testdata -- listingTupleList

      val listingTupleFinal = realEstate.getAllSortedByPrice.toList
      //for convinience:
      val pseudoTuplesList = listingTupleFinal.map { myTuple =>
        myTuple._1 + myTuple._2.toString
      }

      for (i<-0 until excludedListings.size)
      {
        var tempString = excludedListings(0)._1 + excludedListings(0)._2.toString
        assert(pseudoTuplesList.contains(tempString) == true)     //check if backlog listing has been added to listings
      }
    }
  }


}
