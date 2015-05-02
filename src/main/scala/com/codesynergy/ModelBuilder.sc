val VariablePattern = """(\d+)*\s?(\w)+""".r
"10name" matches VariablePattern.toString()

"10 name" match {
  case VariablePattern(d, name) => println(s"Var = $d$name")
  case _ => println("not matched")// do nothing
}