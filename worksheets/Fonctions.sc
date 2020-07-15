def square(x: Double): Double = x * x


square(4)
square(square(4))
square(square(square(4)))

def sumOfSquares(x: Double, y: Double): Double =
  square(x) + square(y)

sumOfSquares(10, 4)

sumOfSquares(square(4), square(2))

def abs(x: Int): Int = if(x>=0) x else -x

abs(-5)
abs(5)

def abs2(x:Int): Int = {
  if(x>=0)x
  else -x
}

abs(-5)
abs(5)

def remain(x: Int, y: Int): Int = x % y

def max(x: Int, y: Int): Int = if(x>=y)x else y

remain(16,4)
remain(17,4)

max(5,9)