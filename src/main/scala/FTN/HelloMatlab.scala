package FTN

import com.mathworks.engine.MatlabEngine

object HelloMatlab {
  def main(args: Array[String]): Unit = {
    val eng = MatlabEngine.startMatlab
    // execute statements
    // cannot define function here, write functions in a file in /home/ltr
    eng.eval("cd '/home/ltr'")
    // evaluate function
    val result: Double = eng.feval("Add", Array(1.0, 2.0))
    println(result)
    eng.close()
  }
}
