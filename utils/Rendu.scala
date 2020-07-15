import Rendu.{encodeList, ourList, ourSecondList}

object Rendu extends App {

  // 1. Trouver le 1er élément d’une liste - (2 points)
  @scala.annotation.tailrec
  def last[T](list: List[T]): T = list match {
    case head :: Nil => head
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  // 2. Trouver le Nème élément d’une liste. Par convention, le premier élément dans la liste est l’élément 0 - (2 points)
  @scala.annotation.tailrec
  def nth[T](n: Int, list: List[T]): T = (n,list) match {
    case (0, head :: _) => head
    case (n, _ :: tail) => nth(n - 1, tail)
    case _ => throw new NoSuchElementException
  }

  // 3. Trouver le pénultième (avant dernier) élément d’une liste - (2 points)
  def penultimate[A](list: List[A]) = list match {
    case _ :+ x :+ _ => Some(x)
    case y +: Nil => Some(y)
    case _ => None
  }

  // 4. Calculer la taille (nombre d’éléments) d’une liste - (2 points)
  def length(list: List[Int]): Int = list match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  // 5. Ecrire la méthode de Compression RLE (Run-length Encoding) d’une liste - (2 points)
  def encode(list: List[Symbol]): List[(Int, Symbol)] = list match {
    case first_head :: first_tail => {
      encode(first_tail) match {
        case (second_head, `first_head`) :: second_tail => (1 + second_head, first_head) :: second_tail
        case tail => (1, first_head) :: tail
      }
    }
    case _ => Nil
  }

  // 6. Dupliquer les éléments d’une liste en utilisant flatMap - (2 points)
  def duplicate(list : List[Int]): List[Int] = list.flatMap(x => List(x,x))

  def assert(test : Boolean, text : String): String = if (test) {
    "SUCCESS:" + text
  } else {
    "FAILED:" + text
  }

  def testExercice1(ourList: List[Int], ourSecondList: List[String]) : Unit = {
    println("Tests exercises 1")
    println(assert(last(ourList) == 4,"testOurListExo1"))
    println(assert(last(ourSecondList) == "StringFour","testOurSecondListExo1"))
  }

  def testExercice2(ourList: List[Int], ourSecondList: List[String]) : Unit = {
    println("Tests exercises 2")
    println(assert(nth(2,ourList) == 3,"testOurListExo2"))
    println(assert(nth(2,ourSecondList) == "StringThree","testOurSecondListExo2"))
  }

  def testExercice3(ourList: List[Int], ourSecondList: List[String]) : Unit = {
    println("Tests exercises 3")
    println(assert(penultimate(ourList) == 3,"testOurListExo3"))
    println(assert(penultimate(ourSecondList) == "StringThree", "testOurSecondListExo4"))
  }

  def testExercice4(ourList: List[Int], ourSecondList: List[String]) : Unit = {
    println("Tests exercises 4")
    println(assert(length(ourList) == 4, "testOurListExo4"))
    println(assert(length(ourEmptyList) == 0, "testOurEmptyListExo4"))
  }

  def testExercice5(encodeList: List[Symbol]) : Unit = {
    println("Tests exercises 5")
    println(
      assert(
        encode(encodeList) == List((4,Symbol("a")), (1,Symbol("b")), (2,Symbol("c")), (2,Symbol("a")), (1,Symbol("d")), (4,Symbol("e"))),
        "testEncodeList"
      )
    )
  }

  def testExercice6(ourList: List[Int]) : Unit = {
    println("Tests exercises 6")
    println(assert(duplicate(ourList) == List(1,1,2,2,3,3,4,4), "testOurListExo6"))
  }

  val ourEmptyList : List[Nothing] = List()
  val ourList : List[Int] = List(1,2,3,4)
  val ourSecondList : List[String] = List("StringOne","StringTwo","StringThree","StringFour")
  val encodeList : List[Symbol] = List(Symbol("a"), Symbol("a"), Symbol("a"), Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"), Symbol("a"), Symbol("a"), Symbol("d"), Symbol("e"), Symbol("e"), Symbol("e"), Symbol("e"))

  testExercice1(ourList, ourSecondList)
  testExercice2(ourList, ourSecondList)
  testExercice3(ourList, ourSecondList)
  testExercice4(ourList, ourSecondList)
  testExercice5(encodeList)
  testExercice6(ourList)

}
