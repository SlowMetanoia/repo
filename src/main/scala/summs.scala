import java.lang.Thread.sleep
import scala.util.Random
//параллельное вычисление сумы матриц
object ParallelSum extends App {
  //разделение массива на непересекающиеся куски работы
  def separation(arraySize: Int, threadsCount: Int): Seq[(Int, Int)] =
    for (i <- 0 to (threadsCount - 1)) yield (i * arraySize / threadsCount, (i + 1) * arraySize / threadsCount - 1)
  //генерация данных
  var testmtx = Array.ofDim[Double](10000, 10000)
  var rand = new Random()
  for (i <- testmtx.indices; j <- testmtx(i).indices) testmtx(i)(j) = rand.nextDouble()
//функция параллельной суммы
  def ParallelMatrixSum: Array[Array[Double]] => Array[Array[Double]] => Int => Array[Array[Double]] = a => b => n => {
    val l = a.length
    var result = Array.ofDim[Double](l, l)
    //вычисление суммы кусков массива
    def segmentSum(left: Int, right: Int) = {
      for (i <- left to right; j <- 0 to (l - 1)) result(i)(j) = a(i)(j) + b(i)(j)
    }
    //коллекция потоков
    val threads = for (part <- separation(l, n)) yield new Thread(() => segmentSum(part._1, part._2))
    threads.foreach(_.start())
    //ожидание результатов
    sleep(10)
    while (!threads.forall(!_.isAlive)) sleep(10)
    result
  }
  val result2 = ParallelMatrixSum(testmtx)(testmtx)(10)
}
//поднопоточная сумма матриц
object NaiveSum extends App {
  //генерация данных
  var testmtx = Array.ofDim[Double](10000, 10000)
  var rand = new Random()
  for (i <- testmtx.indices; j <- testmtx(i).indices) testmtx(i)(j) = rand.nextDouble()

  def naiveSum: Array[Array[Double]] => Array[Array[Double]] => Array[Array[Double]] = a => b => {
    val l = a.length
    val result = Array.ofDim[Double](l, l)
    for (i <- result.indices; j <- result(i).indices) result(i)(j) = a(i)(j) + b(i)(j)
    result
  }

  val result1 = naiveSum(testmtx)(testmtx)
}
//объект для замеров и вывода
object refere extends App {
  println(myTimer(NaiveSum).workTime + "sec for naive sum")
  println(myTimer(ParallelSum).workTime + "sec for parallel sum")
}
