
def fa (a: Int): Int = a
def fb (a: Int): Int = a+1
def fc (a: Int): Int = a+2


val obA = fa _
val obB = fb _
val obC = fc _


def myMatch (f: Int => Int): String = f match {
  case `obA` => "f is fa"
  case `obB` => "f is fb"
  case _ => "I dont know f"
}

myMatch(obA)
myMatch(obB)
myMatch(obC)

