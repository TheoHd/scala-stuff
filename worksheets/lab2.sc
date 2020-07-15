def pgcd(a: Int, b: Int): Int = {
  b match {
    case 0 => a
    case _ => pgcd(b,a % b)
  }
}

pgcd(96,64)