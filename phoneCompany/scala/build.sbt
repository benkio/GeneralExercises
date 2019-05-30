lazy val phoneCompany = (project in file(".")).settings(
  Seq(
    name := "disco-test-phone-company",
    version := "1.0",
    scalaVersion := "2.12.8",
    scalacOptions += "-Ypartial-unification",
    resolvers += Resolver.bintrayRepo("wolfendale", "maven"),
    libraryDependencies ++= Seq("com.github.nscala-money" %% "nscala-money" % "0.13.0",
                                "org.typelevel" %% "cats-effect" % "1.3.0",
                                "org.typelevel" %% "cats-core" % "1.6.0",
                                "org.scalatest" %% "scalatest" % "3.0.5" % "test",
                                "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
                                "wolfendale" %% "scalacheck-gen-regexp" % "0.1.1" % "test")
  )
)