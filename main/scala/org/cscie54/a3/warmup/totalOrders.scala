package org.cscie54.a3.warmup

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks._

/*
Consider the following generator of integers
with a total ordering.  This implementation
is not lock free.
*/

object TotalOrder {
  private var counter = 0
  def next() = this.synchronized {
    val newValue = counter + 1
    counter = newValue
    newValue
  }
}

/*
  Example Usage:
  TotalOrder.next()  // 1
  TotalOrder.next()  // 2
  TotalOrder.next()  // 3

Rewrite this in a lock free fashion without
using synchronized or AtomicInteger, however
AtomicReference is allowed.  Note also the
signature of the next() method.
*/


object TotalOrder2 {

  private var value= new AtomicReference[Integer](0)
  // after implementing next() method, please uncomment the annotation below:
  // @annotation.tailrec
  def next(): Integer =
  {
     var v: Integer = 0
     do
     {
       v = value.get()
     } while (!value.compareAndSet(v, v+1))
     v+1
  }

  // this is for test purposes only (with lock)
  private val lock: Lock = new ReentrantLock()
  def getValue: Int =
  {
    lock.lock()
    try
    {
      value.get()
    }
    finally
    {
      lock.unlock()
    }
  }

}

/*
Example Usage:
TotalOrder2.next()  // 1
TotalOrder2.next()  // 2
TotalOrder2.next()  // 3
*/