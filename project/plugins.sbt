resolvers += Resolver.url(
  "bintray-laika-sbt-plugin-releases",
  url("http://dl.bintray.com/content/jenshalm/sbt-plugins/"))(
  Resolver.ivyStylePatterns)

resolvers += Resolver.url(
  "bintray-scala.js-sbt-plugin-releases",
  url("https://dl.bintray.com/content/scala-js/scala-js-releases"))(
  Resolver.ivyStylePatterns)

resolvers += Resolver.url(
  "bintray-tut-sbt-plugin-releases",
  url("https://dl.bintray.com/content/tpolecat/sbt-plugins"))(
  Resolver.ivyStylePatterns)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.15")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.4.8")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")

addSbtPlugin("org.planet42" % "laika-sbt" % "0.6.0")