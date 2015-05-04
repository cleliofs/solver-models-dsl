val VariablePattern = """(\d+)*\s?(\w)+""".r
"2 z" matches VariablePattern.toString()

"2z" match {
  case VariablePattern(d, name) => println(s"Var = $d $name")
  case _ => println("not matched")// do nothing
}