package org.cscie54.a3

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.lang.IllegalStateException
import java.lang.IllegalArgumentException

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


class RealEstateListingsImpl (realEstListingsNum: Int) extends RealEstateListings
{
  private final val trackedListingsLimitNum = realEstListingsNum

  private val currentTrackedListingsNum = new AtomicInteger(0)

  private val listings = new AtomicReference(Map[String, Int]())

  def getCurrentPrice(address: String): Try[Int] = ???

  def tryAddListing(address: String, price: Int): Try[Unit] = Try
  {
    var v: Integer = 0
    // check for tracked listing number first
    do
    {
      v = currentTrackedListingsNum.get()
    } while (!currentTrackedListingsNum.compareAndSet(v, v+1))
    v+1

    if(v+1 <= trackedListingsLimitNum)
    {
      var tempMap =  Map.empty[String, Int]
      var newTempMap = Map.empty[String, Int]
      do
      {
        tempMap = listings.get()
        if(tempMap.contains(address))
        {
          // have to roll back the currentTrackedListingsNum if failure!
          do
          {
            v = currentTrackedListingsNum.get()
          } while (!currentTrackedListingsNum.compareAndSet(v, v-1))

          Failure[IllegalArgumentException]
        }
        else
        {
           newTempMap = tempMap ++ Map(address, price)
        }
      }while (!listings.compareAndSet(tempMap, newTempMap))
    }
    else
    {
      Failure[IllegalStateException]
    }
  }

  def addListing(address: String, price: Int): Try[Unit] = ???

  def updatePrice(address: String, price: Int): Try[Unit] = ???

  def removeListing(address: String): Try[Unit] = ???

  def getTotalNumber: Int = ???

  def getTotalValue: Long = ???

  def getAllSortedByPrice: Iterable[(String, Int)] = ???


}

