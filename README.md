# iliad
[![Build Status](https://travis-ci.org/to-ithaca/iliad.svg?branch=master)](https://travis-ci.org/to-ithaca/iliad) [![codecov](https://codecov.io/gh/to-ithaca/iliad/branch/master/graph/badge.svg)](https://codecov.io/gh/to-ithaca/iliad)

# Still in progress

Iliad is a commercial mobile game library written completely in scala using functional paradigms. The rendering part of the library is written with an interpreter for OpenGL ES 3.0.

Using the companion [plugin](https://github.com/to-ithaca/sbt-iliad) allows applications to be deployed to iOS (using RoboVM compilation) and Android. The plugin also comes with emulation for the X11 (MacOS and Linux) and Win32 (Windows) based windowing systems.

The library depends heavily on,

-[cats](https://github.com/typelevel/cats) for general functional typeclasses and datatypes
-[freek](https://github.com/ProjectSeptemberInc/freek) for enrichment of the `Free` model
-[fs2](https://github.com/functional-streams-for-scala/fs2) for reactive streams
-[spire](https://github.com/non/spire) for mathematical typeclasses and datatypes
-[scodec](https://github.com/scodec/scodec) for working with binary data
-[shapeless](https://github.com/milessabin/shapeless) for working with heterogenous lists
-and more...
