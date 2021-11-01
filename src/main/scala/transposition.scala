import ParallelSum.separation

import java.lang.Thread.sleep
import scala.util.Random

object parallelTransposition extends App{
  def printArr(matrix:Array[Array[Double]]) = matrix.foreach(xs=>println(xs.mkString(" ")))
  def separation(arraySize:Int, threadsCount:Int):Seq[(Int,Int)] =
    for (i <- 0 to (threadsCount-1)) yield (i*arraySize/threadsCount,(i+1)*arraySize/threadsCount - 1)
  var testmtx = Array.ofDim[Double](10,10)
  var rand = new Random()
  for(i<-testmtx.indices;j<-testmtx(i).indices) testmtx(i)(j) = rand.nextDouble()
  def ParallelTransposition(a:Array[Array[Double]],n:Int):Array[Array[Double]] = {
    val l = a.length
    def segmentTransposition(left:Int,right:Int) = {
      for(i<-left to right; j <-0 to (l-1) if j>(l-1-i)) {
        val x = a(i)(j)
        a(i)(j) = a(l - i - 1)(l - j - 1)
        a(l - i - 1)(l - j - 1) = x
      }
    }
    val threads = for(part<-separation(l,n)) yield new Thread(()=>segmentTransposition(part._1,part._2))
    threads.foreach(_.start())
    sleep(10)
    while(!threads.forall(!_.isAlive)) sleep(10)
    a
  }
  val result = ParallelTransposition(testmtx,10)
  /*printArr(testmtx)
  println()
  printArr(ParallelTransposition(testmtx,10))*/
}

object naiveTransposition extends App{
  def printArr(matrix:Array[Array[Double]]) = matrix.foreach(xs=>println(xs.mkString(" ")))
  var testmtx = Array.ofDim[Double](5,5)
  var rand = new Random()
  for(i<-testmtx.indices;j<-testmtx(i).indices) testmtx(i)(j) = rand.nextDouble()
  def naiveTransposition(a:Array[Array[Double]]):Array[Array[Double]] = {
    val l = a.length
    for(i<-a.indices; j <-a.indices if j>(l-1-i)) {
      val x = a(i)(j)
      a(i)(j) = a(l - i - 1)(l - j - 1)
      a(l - i - 1)(l - j - 1) = x
    }
    a
  }
  val result = naiveTransposition(testmtx)
  /*printArr(testmtx)
  println
  printArr(naiveTransposition(testmtx))*/
}

object refere0 extends App{
  println(myTimer(parallelTransposition).workTime+" sec for parallel transposition to finish")
  println(myTimer(naiveTransposition).workTime+" sec for naive transposition to finishgit a")
}