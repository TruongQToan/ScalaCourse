import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT",
        name := "merge_sort",
        resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases",
        libraryDependencies += scalaTest % Test,
        libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2",
        testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
        parallelExecution in Test := false
    ))
  )
