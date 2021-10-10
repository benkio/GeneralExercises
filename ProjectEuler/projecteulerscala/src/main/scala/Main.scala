@main def main: Unit =
  println(s"""
 ******************** Problems Results 1-10 ********************
 Es1: ${pad(ProjectEuler.es1)} Es2: ${pad(ProjectEuler.es2)} Es3: ${pad(ProjectEuler.es3)}
 Es4: ${pad(ProjectEuler.es4)} Es5: ${pad(ProjectEuler.es5)} Es6: ${pad(ProjectEuler.es6)}
 Es7: ${pad(ProjectEuler.es7)} Es8: ${pad(ProjectEuler.es8)} Es9: ${pad(ProjectEuler.es9)}
 Es10: ${pad(ProjectEuler.es10)}""")

  println(s"""
 ******************** Problems Results 11-20 ********************
 Es11: ${pad(ProjectEuler2.es11)} Es12: ${pad(ProjectEuler2.es12)} Es13: ${ProjectEuler2.es13}
 Es14: ${pad(ProjectEuler2.es14)} Es15: ${pad(ProjectEuler2.es15)} Es16: ${pad(ProjectEuler2.es16)}
 Es17: ${pad(ProjectEuler2.es17)} Es18: ${pad(ProjectEuler2.es18)} Es19: ${pad(ProjectEuler2.es19)}
 Es20: ${pad(ProjectEuler2.es20)}""")

  println(s"""
 ******************** Problems Results 21-30 ********************
 Es21: ${pad(ProjectEuler3.es21)} Es22: ${pad(ProjectEuler3.es22)} Es23: ${pad(ProjectEuler3.es23)}
""")

def pad[A: Numeric](x: A): String = x.toString.padTo(15, " ").mkString
