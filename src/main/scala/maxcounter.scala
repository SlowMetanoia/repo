import java.lang.Thread.sleep
import scala.util.Random

//Однопоточный подсчёт количества максимаьных значений
object NaiveMaxcounter extends App {
  //генерация данных
  val testArray = new Array[Int](100000000)
  val rand = new Random
  for (i <- testArray.indices) testArray(i) = rand.nextInt(990)

  //считает количество максимальных значений промеждутка от left до right из testArray
  def segmentMaxes(left: Int, right: Int): (Int, Int) = {
    var result = (0, 0)
    for (i <- left to right) {
      if (testArray(i) > result._1) result = (testArray(i), 1)
      if (testArray(i) == result._1) result = (result._1, result._2 + 1)
    }
    result
  }

  println(segmentMaxes(0, testArray.length - 1))
}

//Многопоточный подсчёт количества максимаьных значений
object ParallelMaxcounter extends App {
  //генерация данных
  val testArray = new Array[Int](100000000)
  val rand = new Random
  for (i <- testArray.indices) testArray(i) = rand.nextInt(990)

  //наивное деление пространства вычислений для вычисления на n потоках
  //threadsCount доолжен быть кратен arraySize
  def separation(arraySize: Int, threadsCount: Int): Seq[(Int, Int)] =
    for (i <- 0 to (threadsCount - 1)) yield (i * arraySize / threadsCount, (i + 1) * arraySize / threadsCount - 1)

  //собственно функция параллельного вычисления
  def parallelMaxCouter(array: Array[Int], n: Int): (Int, Int) = {
    var result = Seq[(Int, Int)]()

    //считает количество максимальных значений промеждутка от left до right из testArray
    def segmentMaxes(left: Int, right: Int): (Int, Int) = {
      var result = (0, 0)
      for (i <- left to right) {
        if (testArray(i) > result._1) result = (testArray(i), 1)
        if (testArray(i) == result._1) result = (result._1, result._2 + 1)
      }
      result
    }

    //формирование коллекции с потоками
    val threads = for (part <- separation(testArray.length, n)) yield new Thread(() => {
      val s = segmentMaxes(part._1, part._2)
      synchronized {
        result = result.appended(s)
      }
    })
    //заупск потоков на исполнение
    threads.foreach(_.start())
    //ждём пока все не завершатся
    sleep(10)
    while (!threads.forall(!_.isAlive)) sleep(10)
    //считаем итоговый результат в главном потоке
    var finalResult = (0, 0)
    result.foreach(r => {
      if (r._1 > finalResult._1) finalResult = (r._1, r._2)
      if (r._1 == finalResult._1) finalResult = (finalResult._1, finalResult._2 + r._2)
    })
    finalResult
  }

  println(parallelMaxCouter(testArray, 10))
}

//объект для замеров и вывода
object refere1 extends App {
  println(myTimer(NaiveMaxcounter).workTime + " sec for naive max counter to finish")
  println(myTimer(ParallelMaxcounter).workTime + " sec for parallel max counter to finish")
}
