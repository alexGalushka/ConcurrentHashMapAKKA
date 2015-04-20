package org.cscie54.a3


import java.util.concurrent.atomic.{AtomicReference}
import java.lang.IllegalStateException
import java.lang.IllegalArgumentException
import java.util.concurrent.locks.{ReentrantReadWriteLock, ReadWriteLock}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.Queue
import java.util.NoSuchElementException
import scala.util.{Failure, Try}

trait RealEstateListings {

  /*
    Returns current price of a listing for given address.
    If a listing is not tracked, it returns java.lang.IllegalArgumentException, wrapped inside Failure projection of Try.
  */
  def getCurrentPrice(address: String): Try[Int]


  /*
    If less then N number of listings are currently tracked, adds the listing and returns Success[Unit]
    If N number of listings are already tracked, returns java.lang.IllegalStateException, wrapped inside Failure projection of Try.
    If a listing with a given address already exists, it returns java.lang.IllegalArgumentException, wrapped inside Failure projection of Try.
  */
  def tryAddListing(address: String, price: Int): Try[Unit]

  /*
    If a listing with a given address already exists, it returns java.lang.IllegalArgumentException, wrapped inside Failure projection of Try.
    If less than N number of listings are currently tracked, adds the listing.
    If N number of listings are already tracked, "saves it for later" and adds it when an open slot becomes available.
    Note: In the latter case, the service should automatically add a listing (in FIFO fashion) when some other listing is removed.
    The "saved for later" collection allows duplicate entries. However, when an item from this collection is to be added to the main collection of listings,
    but it turns out to be a duplicate, it can just silently fail - perform no operation. No need to throw an exception.
  */
  def addListing(address: String, price: Int): Try[Unit]

  /*
    Updates the price of an existing listing, determined by given address.
    If a listing does not exist, it returns java.lang.IllegalArgumentException, wrapped inside Failure projection of Try.
  */
  def updatePrice(address: String, price: Int): Try[Unit]

  /*
    Stops tracking a listing. Only listings that are actively being tracked are eligile for removal.
    That is, ones "saved for later inclusion" should not be removed.
    If a listing does not exist, it returns java.lang.IllegalArgumentException, wrapped inside Failure projection of Try.
  */
  def removeListing(address: String): Try[Unit]

  /*
    Returns the current number of listings being tracked.
  */
  def getTotalNumber: Int

  /*
    Returns total value (sum of individual prices) of all listings
  */
  def getTotalValue: Long

  /*
    Retrieves all listings, sorted by price in a descending order, as a collection of tuples,
    where the first tuple element is an address and second represents a price.
  */
  def getAllSortedByPrice: Iterable[(String, Int)]
}


class RealEstateListingsImpl (realEstListingsNum: Int) extends RealEstateListings {

  private val readWriteLockTotVal: ReadWriteLock = new ReentrantReadWriteLock()
  private val readLockTotVal = readWriteLockTotVal.readLock()
  private val writeLockTotVal = readWriteLockTotVal.writeLock()

  private val trackedListingsLimitNum = realEstListingsNum

  private val listings = new TrieMap[String, Int]()

  private val backLogListings = new AtomicReference(Queue[(String, Int)]())

  private var totalValue: Long = 0


  private def updateTotalValue(value: Int) = {
    writeLockTotVal.lock()
    try {
      totalValue += value
    }
    finally {
      writeLockTotVal.unlock()
    }
  }


  private def getPrice(address: String): Int = {
    //val tempMap = listings.get()
    //tempMap.get(address)
    listings.get(address) match {
      case Some(result) => result
      case None => -1
    }
  }

  def getCurrentPrice(address: String): Try[Int] = {

    val result = getPrice(address)
    if (-1 == result)
    {
      Failure(new IllegalArgumentException)
    }
    else
    {
      Try{result}
    }
  }

  def tryAddListing(address: String, price: Int): Try[Unit] = Try
  {
    if(listings.size < trackedListingsLimitNum)
    {
       listings.putIfAbsent(address, price) match{
         case Some(expectedPrice) => return Try {Failure{new IllegalArgumentException}}
         case None => updateTotalValue(price)
       }
    }
    else
    {
      return Try{Failure{new IllegalStateException}}
    }
  }

  def addListing(address: String, price: Int): Try[Unit] = Try
  {
    // check for tracked listing number first
    if(listings.size < trackedListingsLimitNum)
    {
      listings.putIfAbsent(address, price) match{
        case Some(expectedPrice) => return Try {Failure{new IllegalArgumentException}}
        case None => updateTotalValue(price)
      }
    }
    else
    {
      // add to back log of listings
      var tempBackLogListings = Queue.empty[(String,Int)]
      var newTempBackLogListings = Queue.empty[(String,Int)]
      do
      {
        tempBackLogListings = backLogListings.get()
        newTempBackLogListings ++= tempBackLogListings
        newTempBackLogListings.enqueue((address, price))

      }while (!backLogListings.compareAndSet(tempBackLogListings, newTempBackLogListings))
    }
  }

  def updatePrice(address: String, price: Int): Try[Unit] = Try
  {
    val oldPrice = listings.get(address) match {
      case Some(result) => result
      case None => -1
    }
    if(-1 != oldPrice) {
      listings.replace(address, oldPrice, price)
    }
  }

  def removeListing(address: String): Try[Unit] = Try {


        listings.remove(address) match
        {
          case Some(priceToRemove) =>

            updateTotalValue(-priceToRemove)
            //finally try to push backlogged listings
            if(getTotalNumber < trackedListingsLimitNum) {
              var tempBackLogListings = Queue.empty[(String, Int)]
              var newTempBackLogListings = Queue.empty[(String, Int)]
              var listingTuple = ("", 0)
              var ifFailed = false
              do {
                tempBackLogListings = backLogListings.get()
                newTempBackLogListings ++= tempBackLogListings
                try
                {
                  listingTuple = newTempBackLogListings.dequeue()
                }
                catch
                  {
                    case ense: NoSuchElementException => ifFailed = true
                  }
                finally {} //nothing to put here

              } while (!backLogListings.compareAndSet(tempBackLogListings, newTempBackLogListings))

              if (!ifFailed)
              {
                try
                {
                  tryAddListing(listingTuple._1, listingTuple._2)
                }
                catch
                  {
                    case eia: IllegalArgumentException => // do nothing

                    case eis: IllegalStateException => // do nothing
                  }
                finally {} //nothing to put here
              }
            }

          case None => Try {Failure{new IllegalArgumentException}}
        }

  }

  def getTotalNumber: Int = {
    listings.size
  }

  def getTotalValue: Long = {

    readLockTotVal.lock()
    try
    {
      totalValue
    }
    finally
    {
      readLockTotVal.unlock()
    }

  }

  def getAllSortedByPrice: Iterable[(String, Int)] = {

    val listOfTuples = listings.readOnlySnapshot().toList

    val sortedList = listOfTuples.sortBy(_._2)

    sortedList.toIterable
  }

}

