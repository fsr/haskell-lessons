Haskell tooling
===============

Haskell is a compiled language. 
As such you do not require any special tooling at runtime.
However do develop and build Haskell projects you will require a compiler to generate an executable binary file from your code.
Furthermore you will most likely require a library management tool, since the Haskell "base" library, which is bundeled with the compile will most likely not be sufficient for most tasks [#base-sucks]_.

.. _GHC:

The compiler (GHC)
------------------

There are many Haskell compilers out there, however very few are well maintained.
As such the Glasgow Haskell Compiler, or GHC for short, has developed as the de-facto standard Haskell compiler.
It is by far the most mature, stable and feature rich Haskell compiler.
In this course we will use optional extensions of the Haskell language which not every compiler implements.
I therefore highly recommend using the GHC as your Haskell compiler.

For more information about the various parts of the GHC see the `compiler reference pages <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`__.
There you will find information on `compiler flags <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html>`__, the `interactive prompt GHCi <ghci reference pages>`_, including the `debugger <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger>`__, `profiling <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html>`__, and the `GHC Haskell extensions <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#syntactic-extensions>`__.
We will discuss all these topics in the future.

We will rarely interact directly with the compiler, as there are very nice :ref:`build tools` out there which we will make use of instead.

.. _ghci reference pages: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html

.. _GHCi:

The interactive Interpreter (GHCi)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GHC also supports the interpreted execution of code.
For one this allows you to directly run a Haskell source file with the ``runghc`` or ``runhaskell`` program.
Furthermore any standard installation of GHC includes a program in which you can interactively type Haskell code, inspect it and run it.
The program is called GHCi (``ghci`` is the executable name) which stands for "GHC interpreter". (`ghci reference pages`_)
GHCi is very simlar to programs like the python or ruby interpreter with the notable difference that the code you type is type checked, like normal Haskell programs, before it is executed.
The GHCi also includes a debugger for Haskell code (like `gdb <https://www.sourceware.org/gdb/>`__) which we will :ref:`study in a later chapter <ghci-debugger>`.

We first use GHCi to explore some Haskell code before we get started with source files.

Some notable ways to interact with GHCi are:

``expr``
    Simply submitting a Haskell expression evaluates the code and tries to print the result value.

``let name = expr``
    Binding a name (we will learn about this :ref:`later <bindings>`) to the value of an expression.

``:browse <module>``
    Displays the contents of the module with the entered name.

``:type <expr>``
    Prints the type of the expression ``expr``.

``:info <name>``
    Displays information about the name, such as the source module of type if it is a function.

Some, but not all, of these commands also work in a shortened form (``:t`` for ``:type`` for instance)

.. _build tools:

Build tools
-----------

There are many ways to manage your Haskell builds.
Bare bones tools such as ``make``, or, if you want to use haskell for the makefile, a library called `shake <https://hackage.haskell.org/package/shake>`__.

I would however recommend to use one of the Haskell specific tools, which make it very easy to manage libraries and build, package and distribute your projects and libraries.
Two major tools are available here, where the second one uses the first and makes certain improvements on the first.

.. _cabal-tool:

Cabal
^^^^^

`The Cabal <cabal>`_ is the older of the two tools.
It is 

    "a system for building and packaging Haskell libraries and programs." 
    [what-is-cabal]_

meaning it manages the compilation of your code as well as any library that code depends on.
If you are familiar with tools such as `cargo`_, `maven`_, `leinigen`_ etc, cabal is the Haskell version of those tools.

Cabal works on a unit of code called a **"package"**.
As with the aforementioned tools this package is configured in a special file, called the **"cabal file"**, after its extension **".cabal"**.

The documentation for the contents of a cabal file can be found `here <https://www.haskell.org/cabal/users-guide/developing-packages.html#package-descriptions>`__.
See an example cabal file `here <https://github.com/JustusAdam/marvin/blob/master/marvin.cabal>`__.
The cabal file contains package metadata, most of it optional, such as the package name, version, a description, the author, license etc.
Furthermore each package may define one "library", a collection of modules which can be imported in other packages, and several "executables" and "test suites".
The sections for "executable", "library" and "test-suite" then define the modules they, include and/or export, which other packages they depend on, what the main file is etc.

Following are the most important command line commands for cabal (all prefixed with ``cabal <command> [options] [args]``).

.. note:: 
    This is only a very quick overview, and does not mention many of the commands and options available. 
    To see all commands use ``cabal --help`` and to see options for a specific command use ``cabal <command> --help``.


``init [dir]``
    Creates a new cabal package in the directory ``[dir]``.
    This command interactively asks for the required and most common fields in the cabal file and creates it for you.

``configure``
    Configure the build environment. 
    Ususally not necessary since both ``build`` and ``install`` already run ``configure`` themselves.

``build``
    Compile the modules and executables from the package. 
    Note that this requires all dependencies to be present.
    If you have not installed and registered the dependencies yet run ``cabal install --only-dependencies`` first

``install``
    Compiles the modules and executables from the package.
    Registers the library (if any) and copies the executables to the users configured Haskell binary directory.
    This also installs any missing dependencies.

    Note that since this copies the executables to the Haskell binary directory it will overwrite any previously installed program with the same name.
    If you do not wish this behaviour instead run ``cabal install --dependencies-only`` to install only the dependencies and then use ``cabal build`` to compile the project, or alternatively use a :ref:`sandbox <cabal-sandbox>`.

``repl``
    Opens :ref:`GHCi` in the context of the dependencies and sources of the current package.

``run [executable]``
    If your package defines executables and you build them using ``cabal build`` to avoid polluting cabals user binary directory you can use this command to conveniently invoke the locally installed executable.
    Should your package contain only one executable you can omit the ``[executable]`` name.

.. _cabal-sandbox:

Sandboxes
"""""""""

Per default cabal installs the dependencies of your packages in a user-global directory (``$HOME/.cabal/lib`` on UNIX) and for each library it installs only one version.
This means all packages you develop (or install) share the same pool of libraries and versions.
Since these libraries get updated over time you will find yourself with a high likelyhood of dependency conflicts after a while, especially if you are developing multiple packages on the same system.
Furthermore since the libraries are global you need to reinstall all of them every time you update the compiler.
A relatively recent solution to this problem are **sandboxes**.

A sandbox is a separate library pool against which your packages can be built.
This is the same approach `cargo`_ uses.
Usually one sandbox is used for only one package or project, but it is possible to reuse a sandbox for multiple packages, although that may defeat the original purpose of separation.

Sanboxes are created by running ``cabal sandbox init``. 
This creates a directory for the libraries and a new package database against which you can build your package.
Commands such as ``cabal build|install|run|repl`` will automatically search for a sandbox in the directory in which the command is invoked, no additional configuration required.
The next ``cabal install`` after creating a sandbox will pull all dependencies from the repository again and install them into the sandbox.
Also all libraries and executables created by ``cabal install``, be those from dependencies or the package itself will now be installed into the sandbox.
You can use ``cabal run`` to run executables which were installed into the sandbox.

.. admonition:: Caveats
    
    Despite the usefulness of sandboxes there are some drawbacks.

    #. Redundancy and Space

        Having many Haskell projects means a full set of compiled dependencies for each of them.
        This can use up quite a large amount of disk space, and many sandboxes will contain the the same core set libraries.
    
    #. Build times

        Many (real world) Haskell packages have a large stock of dependencies and the GHC is quite slow (compared to, for instance the go compiler).
        The first ``cabal install`` after setting up the sandbox takes a very long time.
        This may be tolerable for projects on which you work for a while, but is very annoying if you just want to quickly check out someone else's project.
    
    Because of these issues I recommend using :ref:`stack` instead.
    It uses the same project configuration (the cabal file), has very similar(ly working) commands and avoids the aforementioned issues to a large degree.


.. _stack:

Stack
-----

Stack is a build tool very similar to cabal.
In fact, under the hood, stack uses cabal as a library to read the project configuration, calculate build order etc.


Aside: Recommended Editors and development environments
-------------------------------------------------------



.. [what-is-cabal] The Haskell Cabal, Introduction. https://www.haskell.org/cabal/
.. _cabal: https://www.haskell.org/cabal/
.. _cargo: https://doc.rust-lang.org/1.5.0/book/hello-cargo.html 
.. _maven: https://maven.apache.org/
.. _leinigen: https://leiningen.org/


.. rubric:: Footnotes

.. [#base-sucks] This is one of the unfortunate things about Haskell, that the base library lacks many desirable things.
