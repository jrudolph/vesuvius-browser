val scalaV = "3.3.1"
val pekkoV = "1.0.1"
val pekkoHttpV = "1.0.0"
val scalaTestV = "3.2.16"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-stream" % pekkoV,
  "org.apache.pekko" %% "pekko-http" % pekkoHttpV,
  "org.apache.pekko" %% "pekko-http-spray-json" % pekkoHttpV,
  "org.apache.pekko" %% "pekko-http-caching" % pekkoHttpV,
  "io.spray" %% "spray-json" % "1.3.6",
  "com.typesafe.play" %% "twirl-api" % "1.6.0-RC4",

  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

enablePlugins(SbtTwirl)

scalaVersion := scalaV

// docs

enablePlugins(ParadoxMaterialThemePlugin)

Compile / paradoxMaterialTheme := {
  ParadoxMaterialTheme()
    // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#changing-the-color-palette
    .withColor("light-green", "amber")
    // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#adding-a-logo
    .withLogoIcon("cloud")
    .withCopyright("Copyleft © Johannes Rudolph")
    .withRepository(uri("https://github.com/jrudolph/xyz"))
    .withSocial(
      uri("https://github.com/jrudolph"),
      uri("https://twitter.com/virtualvoid")
    )
}

paradoxProperties ++= Map(
  "github.base_url" -> (Compile / paradoxMaterialTheme).value.properties.getOrElse("repo", "")
)

// setup docker build
// use separate dependency and app jars
assembly / assemblyOption := (assembly / assemblyOption).value.withIncludeScala(false).withIncludeDependency(false)
assembly / assemblyJarName := "app.jar" // contract with Dockerfile
assemblyPackageDependency / assemblyJarName := "deps.jar" // contract with Dockerfile
assemblyMergeStrategy := {
  // merging pekko protobuf and protobuf from google ortools (not sure why they are different, but doesn't matter here)
  case PathList(ps @ _*) if ps.last endsWith ".proto" => MergeStrategy.first
  case x => assemblyMergeStrategy.value(x)
}
