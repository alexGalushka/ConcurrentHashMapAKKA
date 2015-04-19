package org.cscie54.a3.warmup

/*
Create an effectively immutable class that represents
a Rectangle with a given length and width.  The class
will have a private mutable field for area, but once
initialized, the value will never change.  The idea
is that the area calculation might be expensive, and
should only be computed on demand, and only once per
Account.  You are expected to use @volatile but not
lazy keyword in your solution.
*/


class Rectangle(val length: Int, val width: Int) {
  // you may need to add or modify code here
  private var area: Int = 0

  @volatile private var ifCalculated = false

  def getArea(): Int = {
    if(!ifCalculated)
    {
      area = length*width
      ifCalculated = true
      area
    }
    else
    {
      area
    }
  }

}

/*
  Sample usage
  val r = new Rectangle(6, 7)
  r.getArea()   // 42
*/