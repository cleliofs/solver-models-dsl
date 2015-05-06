"2.0 z_100_endings".split(" ").toList(0)
"2.0 z_100_endings".split(" ").toList(1)

"x".split(" ").toList.length

val VariablePattern = """(\d)+ (\.)+""".r
"2 z_100_endings" match {
  case VariablePattern(d, name) => println(s"Var = $d $name")
  case _ => println("not matched")// do nothing
}

val t: Tuple2[Int, Int] = Tuple2(0, 1)
t._1
t _2

var d: Option[Double] = None
d = Some(0)
d != None
d == Some
d.get == 0
d.getOrElse(null)