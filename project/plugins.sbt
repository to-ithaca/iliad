resolvers += Resolver.url(
  "bintray-zainab-ali-sbt-plugins",
  url("http://dl.bintray.com/zainab-ali/sbt-plugins"))(
  Resolver.ivyStylePatterns)

addSbtPlugin("org.directive" %% "sbt-directive" % "0.1.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.15")
