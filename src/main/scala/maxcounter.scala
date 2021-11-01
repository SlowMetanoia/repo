import java.lang.Thread.sleep
import scala.collection.mutable
import scala.util.Random

object NaiveMaxcounter extends App{
  val testArray = new Array[Int](100000000)
  val rand = new Random
  for(i<-testArray.indices) testArray(i) = rand.nextInt(990)
  def segmentMaxes(left:Int,right:Int):(Int,Int) = {
    var result = (0,0)
    for(i<-left to right) {
      if(testArray(i)>result._1) result = (testArray(i),1)
      if(testArray(i)==result._1) result = (result._1,result._2+1)
    }
    result
  }
  println(segmentMaxes(0,testArray.length-1))
}

object ParallelMaxcounter extends App{
  val testArray = new Array[Int](100000000)
  val rand = new Random
  for(i<-testArray.indices) testArray(i) = rand.nextInt(990)
  def separation(arraySize:Int, threadsCount:Int):Seq[(Int,Int)] =
    for (i <- 0 to (threadsCount-1)) yield (i*arraySize/threadsCount,(i+1)*arraySize/threadsCount - 1)
  def parallelMaxCouter(array: Array[Int],n:Int):(Int,Int) = {
    var result = Seq[(Int,Int)]()
    def segmentMaxes(left:Int,right:Int):(Int,Int) = {
      var result = (0,0)
      for(i<-left to right) {
        if(testArray(i)>result._1) result = (testArray(i),1)
        if(testArray(i)==result._1) result = (result._1,result._2+1)
      }
      result
    }
    val threads = for(part<-separation(testArray.length,10)) yield new Thread(()=>{
      val s = segmentMaxes(part._1,part._2)
      synchronized{result = result.appended(s)}
    })
    threads.foreach(_.start())
    sleep(10)
    while(!threads.forall(!_.isAlive)) sleep(10)
      var finalResult = (0,0)
      result.foreach(r=>{
        if (r._1 > finalResult._1) finalResult = (r._1,r._2)
        if (r._1 == finalResult._1) finalResult = (finalResult._1,finalResult._2+r._2)
      })
    finalResult
  }
  println(parallelMaxCouter(testArray,10))
}

object refere1 extends App {
  println(myTimer(NaiveMaxcounter).workTime+" sec for naive max counter to finish")
  println(myTimer(ParallelMaxcounter).workTime+" sec for parallel max counter to finish")
}
