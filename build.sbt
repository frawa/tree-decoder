import Dependencies._

addCommandAlias("lint", "headerCheckAll;fmtCheck;fixCheck;npmAll")
addCommandAlias("lintFix", "headerCreateAll;fixFix;fmtFix")
addCommandAlias("fmtCheck", "all scalafmtCheck scalafmtSbtCheck")
addCommandAlias("fmtFix", "all scalafmt scalafmtSbt")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fixFix", "scalafixAll")

lazy val scalaVersion3 = "3.2.0"

lazy val sharedSettings = Seq(
  scalaVersion     := scalaVersion3,
  organization     := "io.github.frawa",
  organizationName := "Frank Wagner",
  startYear        := Some(2022),
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  versionScheme := Some("semver-spec")
)

lazy val sharedScalacSettings = Seq(
  scalacOptions ++= {
    Seq(
      "-deprecation",
      "-feature"
      // "-version",
      // "-help",
      // "-encoding",
      // "UTF-8"
      // "-language:implicitConversions"
      // disabled during the migration
      // "-Xfatal-warnings"
    ) ++
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq(
            "-unchecked",
            "-Xmigration",
            "-new-syntax",
            "-indent"
            // "-Ywarn-unused",
            // "-source:future",
            // "-source:future-migration",
            // "-source:3.2-migration",
            // "-source:3.0-migration",
            // "-rewrite"
            // "-explain"
          )
        case _ =>
          Seq(
            "-Xfatal-warnings",
            "-Wunused:imports,privates,locals",
            "-Wvalue-discard"
          )
      })
  },
  ThisBuild / semanticdbEnabled := true,
  ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
)

lazy val sharedTestSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
  // Test / testOptions += Tests.Argument("+l", "-q", "--summary=0")
  // perferred to copy&paste expectations into tests:
  Test / testOptions += Tests.Argument("-q", "--summary=0")
)

lazy val root = project
  .in(file("."))
  .settings(sharedSettings)
  .settings(sharedSettings)
  .settings(sharedTestSettings)
  .settings(
    name           := "tree-decoder",
    publish / skip := true
  )
