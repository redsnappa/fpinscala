

def failingFn(i:Int): Int = {
  val y = throw new Exception("Error!")
  try{
    val x = 24 + 23
    x+y
  }
  catch {
    case e:Exception => 42
  }
}

failingFn(1)