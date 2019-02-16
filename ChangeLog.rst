=======================
 Quickdist's ChangeLog
=======================

0.6.0 (2019-02-17)
==================

Code was refactored to make it possible to reuse parts responsible for
system's info extraction and archive creation.

Also, this made the main function ``create-dist`` more readable.

0.5.0 (2019-02-16)
==================

Format of the ``black-alist`` was changed.

Now it is possible to ignore not only systems, but also their
dependencies. This could be configured per-project or globally
via ``*blacklisted-systems*`` and ``*blacklisted-dependencies*``
variables.

By default, all SBCL's contrib systems are ignored because they can
cause issues for other implementation.

Format for ``black-alist`` is following::

  ("the-project" :systems ("system-to-ignore" "other-system")
                 :dependencies ("dep-system" "other-depsystem"))


0.4.1 (2019-01-27)
==================

Fixed issue with loading of asd files having a ``:defsystem-depends-on``
argument in the ``defsystem`` form. Before this fix, quickdist wasn't
able to process such asd files. Here is an example:

https://github.com/40ants/log4cl-json/blob/25f5f93032c95423cdacbd7205a8647f47297c41/log4cl-json-test.asd#L13

Error it was throwing was a ``asdf:missing-dependency``.


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

