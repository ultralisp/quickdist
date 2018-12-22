=======================
 Quickdist's ChangeLog
=======================

0.4.0 (2018-12-22)
==================

New variable was added: ``quickdist:*project-path*``.
During building of the distribution, this special variable will point to
a currently processed project.

Also, a symbol ``quickdist:skip-project`` was exported to make it
possible to find a restart and to skip some project in case of errors
during the build.

0.3.0 (2018-12-20)
==================

* Changed a way how do system and release files are filled.

  Previously, if some error was raised during loading of asd
  file there may be information about some project's systems already
  written into the release.txt and systems.txt files.

  Now this behaviour is fixed and information will be written
  only if all project's asd files were loaded correctly.

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

