=======================
 Quickdist's ChangeLog
=======================

0.16.1 (2021-03-04)
===================

* Fixed processing of the `require <https://common-lisp.net/project/asdf/asdf.html#Require>`_
  dependencies like this ``(:require :extensible-sequences)``. Now they are ignored.

  This should fix issue https://github.com/ultralisp/ultralisp/issues/101

0.16.0 (2019-09-15)
===================

* Fixed a bug with system filenames in releases.txt metadata.
* Roswell script now accepts a new option ``--any-asd`` to
  process not only toplevel asd files.

0.15.0 (2019-09-15)
===================

* Now an error is generated if gnu tar binary is absent.
* Fixed issue https://github.com/ultralisp/ultralisp/issues/51 when systems with asd files in nested directories
  were not installable because of wrong system's file in metadata.

0.14.0 (2019-07-07)
===================

* Fixed ``.distignore`` file processing. Previosly, if it contains
  an empty line, any ``.asd`` file was ignored and this caused
  a few Ultralisp distribution version breakage.

0.13.0 (2019-05-17)
===================

* Allow quickdist to read ``.distignore`` file from user configuration folder to define globally ignored files.

0.12.0 (2019-03-13)
===================

* Fixed problem with duplicating systems in the output.
* Fixed a crash on trying to load dependencies for ``iolib``.

0.11.0 (2019-03-09)
===================

Changed a way to ignore asd files and system dependencies.

Now only toplevel ASD files of each project are processed. This was
changed, because some projects usually have something like
``example.asd`` or ``skeleton.asd`` which should not be included into
the distribution.

If some of your projects have asd files in nested directories and you
want to include them, you need to specify a predicate which will allow
them. This should looks like this:

.. code:: common-lisp

   (quickdist:quickdist :name "foo"
                        :projects-dir #P"~/projects/"
                        :dists-dir #P"~/dist/"
                        :get-ignore-filename-p (lambda (project)
                                                 (if (string-equal project "my-cool-project")
                                                     (constantly nil)
                                                     'quickdist:not-toplevel-filename-p)))                            


0.10.1 (2019-03-08)
===================

* Fixed function ``get-system-files``. Now it returns unique filenames.

0.10.0 (2019-03-08)
===================

* ASDF's package inferred systems are supported. A separate line with
  dependencies for each of subsystem will be created in the systems.txt
  file. This gives ability to use ``(ql:quickload :weblocks/server)``
  and to load only a part of package inferred system.
* Also, there is another incompatible change. Now only direct
  dependencies are collected. However this change makes ``quickdist``
  behaviour similar to original Quicklisp distribution's.

0.9.0 (2019-03-04)
==================

* Now function ``get-external-dependencies`` caches it's results
  and it shouldn't lead to crach on ``cl-git`` anymore.
* Added more logging in the code which collects dependencies.

0.8.0 (2019-02-24)
==================

Now ``system-info`` has a relative path in the ``filename`` slot.
Also, ``system-files`` slot of the ``release-info`` object contains
a list of relative paths. And ``get-system-files`` called on the list of
``system-info`` objects returns relative paths.

0.7.0 (2019-02-23)
==================

Class ``release-info`` now contains additional slot ``archive-path``.
It points to a ``.tgz`` file on the filesystem.
Also it's accessor ``get-archive-path`` was exported from the
``quickdist`` package.

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

