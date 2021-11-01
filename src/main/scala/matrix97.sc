import java.lang.Thread.sleep
import scala.language.postfixOps
import scala.util.Random

def printArr(matrix:Array[Array[Double]]) = matrix.foreach(xs=>println(xs.mkString(" ")))
def revT(matrix:Array[Array[Double]]):Array[Array[Double]] = {
  val n = matrix.length
  var result = Array.ofDim[Double](n, n)
  for (i<-matrix.indices; j<-matrix(i).indices)
    if (i!=n-j-1)
      result(n-i-1)(n-j-1) = matrix(i)(j)
    else
      result(i)(j) = matrix(i)(j)
  result
}
def revT0(a:Array[Array[Double]]):Array[Array[Double]] = {
  val l = a.length
  for(i<-a.indices; j <-a.indices if j>(l-1-i)) {
    val x = a(i)(j)
    a(i)(j) = a(l - i - 1)(l - j - 1)
    a(l - i - 1)(l - j - 1) = x
  }
  a
}
var array = Array(Array(1.0,2.0,3),Array(4.0,5.0,6),Array(7,8,9.0))
printArr(array)
println()
printArr(revT(array))
println()
printArr(revT0(array))





