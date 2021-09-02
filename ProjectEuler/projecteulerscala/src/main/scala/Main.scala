@main def main: Unit =
  println(s"""
  ******************** Problems Results 1-10 ********************
  Es1:  ${pad(ProjectEuler.es1)} Es2: ${pad(ProjectEuler.es2)} Es3: ${pad(ProjectEuler.es3)}
  Es4:  ${pad(ProjectEuler.es4)} Es5: ${pad(ProjectEuler.es5)} Es6: ${pad(ProjectEuler.es6)}
  Es7:  ${pad(ProjectEuler.es7)} Es8: ${pad(ProjectEuler.es8)} Es9: ${pad(ProjectEuler.es9)}
  Es10: ${pad(ProjectEuler.es10)}

  ******************** Problems Results 11-20 ********************
  Es11:  ${pad(ProjectEuler2.es11)} Es12: ${pad(ProjectEuler2.es12)} Es13: ${ProjectEuler2.es13}
  Es14:  ${pad(ProjectEuler2.es14)}
  """)

def pad[A: Numeric](x: A): String = x.toString.padTo(15, " ").mkString
