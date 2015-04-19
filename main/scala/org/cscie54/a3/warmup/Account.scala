package org.cscie54.a3.warmup
import java.util.concurrent.locks._

/*
Create a thread-safe Account class, that contains a
mutable balance.  Assume that the read frequency
of Accounts is much higher than the write frequency,
so you are expected to need two locks per Account
instance.  Take care to prevent deadlock.
*/

class Account
{

    private var balance: Int = 0
    private val readWriteLock : ReadWriteLock = new ReentrantReadWriteLock()
    private val readLock = readWriteLock.readLock()
    private val writeLock = readWriteLock.writeLock()


    def getBalance() =
    {
      readLock.lock()
      try
      {
        balance
      }
      finally
      {
        readLock.unlock()
      }
    }

    def adjustBalance( delta: Int) =
    {
      writeLock.lock()
      try
      {
        balance += delta
      }
      finally
      {
        writeLock.unlock()
      }
    }
}

/*
  Sample usage:
  val a = new Account()
  a.getBalance()  //   0
  a.adjustBalance(100)
  a.getBalance()  // 100
  a.adjustBalance(-10)    //  90
*/