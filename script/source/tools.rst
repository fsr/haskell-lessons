The tools we will need
======================

This first lesson is all about the various tools we will use to develop Haskell code.

.. _GHC:

The compiler (GHC)
------------------

Haskell is a compiled language. 
As such you do not require any special tooling at runtime.
However to develop and build Haskell projects you will require a compiler to generate an executable binary file from your code.
Furthermore you will most likely require a library management tool, since the Haskell "base" library, which is bundeled with the compiler will most likely not be sufficient for most tasks [#base-sucks]_.

There are several Haskell compilers out there, however very few are well maintained.
As such the Glasgow Haskell Compiler, or GHC for short, has developed as the de-facto standard Haskell compiler.
It is by far the most mature, stable and feature rich Haskell compiler.
In this course we will use optional extensions of the Haskell language which not every compiler implements.
I therefore highly recommend using the GHC as your Haskell compiler.

For more information about the various parts of the GHC see the `compiler reference pages <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`__.
There you will find information on `compiler flags <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html>`__, the `interactive prompt GHCi <ghci-reference-pages>`_, including the `debugger <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger>`__, `profiling <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html>`__, and the `GHC Haskell extensions <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#syntactic-extensions>`__.
We will discuss all these topics in the future.

We will rarely interact directly with the compiler, as there are very nice :ref:`build tools` out there which we will make use of instead.

.. _ghci-reference-pages: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html

.. _GHCi:

The interactive Interpreter (GHCi)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GHC also supports the interpreted execution of code.
For one this allows you to directly run a Haskell source file with the ``runghc`` or ``runhaskell`` program.
Furthermore any standard installation of GHC includes a program in which you can interactively type Haskell code, inspect it and run it.
The program is called GHCi (``ghci`` is the executable name) which stands for "GHC interpreter". (`ghci reference pages <ghci-reference-pages>`_)
GHCi is very simlar to programs like the python or ruby interpreter with the notable difference that the code you type is type checked, like normal Haskell programs, before it is executed.
The GHCi also includes a debugger for Haskell code (similar to `gdb <https://www.sourceware.org/gdb/>`__) which we will :ref:`study in a later chapter <ghci-debugger>`.

We first use GHCi to explore some Haskell code before we get started with source files.

Some notable ways to interact with GHCi are:

``<expr>``
    Simply submitting a Haskell expression evaluates the code and tries to print the result value.

``let <name> = <expr>``
    Binding the value of an expression to a name (we will learn about this :ref:`later <bindings>`).

``:?``
    Probably the most important command.
    Displays the help menu.
    The help menu lists available commands (a selection of which will follow here) and what they do.

``:browse <module>``
    Displays the contents of the module with the entered name.

``:type <expr>``
    Prints the type of the expression ``expr``.

``:info <name>``
    Displays information about the name, such as the source module, or the type if it is a function.

Some, but not all, of these commands also work in a shortened form (``:t`` for ``:type`` for instance)


.. _documentation:


Documentation resources
-----------------------

The standard Haskell tool suite includes a tool called "haddock" which can be used to generate documentation for your source code from special in line comments.
This documentation is available online for published packages.

Hackage
^^^^^^^

`Hackage <hackage>`_ is the de-facto standard online Haskell package database. 
Anyone can make an account and start uploading their own Haskell packages. 
My username for instance is `justus <hackage.haskell.org/user/justus>`__.

On hackage you may search for packages, browse the different versions available for each package, see a packages dependencies and also browse the generated haddock documentation.

Hoogle
^^^^^^

`Hoogle <hoogle>`_ is a search engine for the hackage documentation.
Whereas on hackage you may only search the database by package name on hoogle you can search the contents more directly by searching for function names, module names, package names and even *type signatures*.

Stackage
^^^^^^^^

The `stackage`_ site, which hosts resources to be used with the tool ``stack`` functions similarly to a combination of Hackage and Hoogle.
It hosts the documentation, including a Hoogle search, for each package snapshot.
I therefore recommend to use stackage to browse haskell packages and documentation, unless the package you want information on is not on Stackage.


Editors
-------

Any text editor is fine for Haskell development.
Though it is desirable to have at least some Haskell source code highlighting.
Many editors also offer extra features via one of the Haskell ide servers ``ghc-mod`` and ``intero``.

I personally use *visual studio code*, becuase it is clean and fast and becuase I maintain its Haskell highlighting plugin and constantly improve it.
However I have heard that the editor best supporting Haskell is supposedly emacs.
Atom also has good Haskell support because of the `atom-haskell`_ group on github.

For those who wish to go hard on Haskell, there is a graphical editor written in Haskell itself, called `leksah`_ as well as a command line editor called `yi`_.
Also a special mention is to be given to `Haskell for Mac`_ a particularly beautiful graphical Haskell IDE with native stack support for OSX and tailored towards learning Haskell.

.. _leksah: http://leksah.org
.. _yi: http://yi-editor.github.io/
.. _Haskell for Mac: http://haskellformac.com
.. _atom-haskell: https://atom-haskell.github.io/

And lastly I want to mention `ghcid <https://www.stackage.org/lts-8.9/package/ghcid>`__. 
Its a very simple, command line based program which simply attempts to load your code into the interpreter and shows you the errors it encouters.
It automatically refreshes whenever you save a source file.
This gives you some very bare bones ide features.
The big advantage is that, unlike the other ide programs, ``ghcid`` is incredibly reliable.

Also for those who like their code to be a bit prettier I recommend using *font ligatures* with a font that supports it, for instance my favourite is `github.com/tonsky/FiraCode <https://github.com/tonsky/FiraCode>`__. In this font there are some multi character symbols which in my opinion make the code a bit more readable.

.. rubric:: Footnotes		
 
.. [#base-sucks] 
    This is one of the unfortunate things about Haskell, that the base library lacks many desirable things. 
    Examples of good standard libraries would be those of python and go.
