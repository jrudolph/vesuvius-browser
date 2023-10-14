val scalaV = "3.3.0"
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
