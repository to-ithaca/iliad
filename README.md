# Iliad
[![Build Status](https://travis-ci.org/to-ithaca/iliad.svg?branch=master)](https://travis-ci.org/to-ithaca/iliad) [![codecov](https://codecov.io/gh/to-ithaca/iliad/branch/master/graph/badge.svg)](https://codecov.io/gh/to-ithaca/iliad)

# Still in progress

Iliad is a commercial mobile game toolkit written completely in functional Scala.
The toolkit renders 3D graphics using OpenGL ES 3.0.

Apps can be deployed to both Android and iOS using the companion [plugin](https://github.com/to-ithaca/sbt-iliad).
The plugin also comes with hardware emulation for X11 (MacOS and Linux) and Win32 (Windows) based windowing systems.

The library depends heavily on,

- [cats](https://github.com/typelevel/cats) for functional abstractions
- [freek](https://github.com/ProjectSeptemberInc/freek) for developing a `Free` OpenGL DSL
- [fs2](https://github.com/functional-streams-for-scala/fs2) for reactive streams
- [spire](https://github.com/non/spire) for mathematical typeclasses and datatypes
- [scodec](https://github.com/scodec/scodec) for working with binary data
- [shapeless](https://github.com/milessabin/shapeless) for working with heterogenous lists
- and more...
