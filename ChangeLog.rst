=======================
 Quickdist's ChangeLog
=======================

0.2.0
=====

* Fixed collection of the systems from asd files.
  Previously, all systems loaded during asd file loading were considered
  as systems to be included into the distribution. This caused problems
  because systems.txt file contained systems which are not included into
  the distribution.

  Here is an issue, caused by this problem:
  https://github.com/ultralisp/ultralisp/issues/1

0.1.0
=====

* Semver versioning was added.
* Fixed issue when ``uiop`` system considered missing when collecting
  dependencies for a system, which depends on ``uiop`` (like ``woo``
  does, for example.

