name := "comp332-funjs"

version := "0.1"

organization := "comp.mq.edu.au"

// Scala compiler settings

scalaVersion := "2.11.2"

scalacOptions ++= Seq ("-deprecation", "-feature", "-unchecked")

// Interactive settings

logLevel := Level.Info

shellPrompt <<= (name, version) { (n, v) => _ => n + " " + v + "> " }

// Execution

parallelExecution in Test := false

// Dependencies

libraryDependencies ++=
    Seq (
        "com.googlecode.kiama" %% "kiama" % "1.6.0",
        "com.googlecode.kiama" %% "kiama" % "1.6.0" % "test" classifier ("tests"),
        "junit" % "junit" % "4.10" % "test",
        "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
        "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )

resolvers += "Sonatype OSS Snapshots Repository" at
    "https://oss.sonatype.org/content/repositories/snapshots"

// Source code locations

// Specify how to find source and test files.  Main sources are
//    - in src directory
//    - all .scala files, except
// Test sources, which are
//    - files whose names end in Tests.scala, which are actual test sources

scalaSource <<= baseDirectory { _ / "src" }

unmanagedSources in Test <<= scalaSource map { s => {
    (s ** "*Tests.scala").get
}}

unmanagedSources in Compile <<= (scalaSource, unmanagedSources in Test) map { (s, tests) =>
    ((s ** "*.scala") --- tests).get
}

// Resources

unmanagedResourceDirectories <<= scalaSource { Seq (_) }

unmanagedResourceDirectories in Test <<= unmanagedResourceDirectories

// Test resources are the non-Scala files in the source that are not hidden
unmanagedResources in Test <<= scalaSource map { s => {
    (s ** (-"*.scala" && -HiddenFileFilter)).get
}}

