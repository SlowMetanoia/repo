import java.lang.Thread.sleep

class myTimer private(val a:App){
  val workStart = System.currentTimeMillis
  a.main(Array())
  val workEnd = System.currentTimeMillis
  def workTime = (workEnd - workStart)/1000.0
}
object myTimer{
  def apply(a:App): myTimer = new myTimer(a)
}