package org.cscie54.a3.warmup
import java.util.concurrent.CountDownLatch

/*
Use a CountdownLatch to implement a "join".
Given a Traversable of lamdas (Unit) => Unit
Execute each of them in their own threads, and use
a CountdownLatch to wait for all of them to finish.
*/


class Join {
	def join(tasks: Traversable[Unit => Unit]) =
  {
    val arrayOfTaskThreads = tasks.map{
      task =>
        val startGate = new CountDownLatch(1)
        val endGate = new CountDownLatch(tasks.size)
        val t = new Thread() {
          override def run(): Unit = {
            startGate.await()
            try{
              task()
            }finally{
              endGate.countDown()
            }
          }
        }
        t.start()
    }
  }

}
