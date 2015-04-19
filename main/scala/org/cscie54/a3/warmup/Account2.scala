package org.cscie54.a3.warmup

/*
Because any object can serve as a lock, it's possible
to pass "this" into a synchronized method call.  For
example, one of the locks in the Account implementation
could be the Account object itself.  Is this a good
or a bad idea?  Defend your response with words and
with a code example.
*/

class Account2
{

  private var balance: Int = 0

  def getBalance() = this.synchronized
  {
    balance
  }

  def adjustBalance(delta: Int) = this.synchronized
  {
    balance += delta
  }


  // The solution above is proned to the deadlock, if we try to combine 2 above methods under one, let's say,
  // transferFunds, there is a good chance that it will deadlock

  // pass the positive delta
  def transferTo(to: Account2, delta: Int) = this.synchronized
  {
    adjustBalance(-delta)
    to.adjustBalance(delta)
  }
}