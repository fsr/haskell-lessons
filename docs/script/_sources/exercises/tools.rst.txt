Tools
=====

Install the mentioned Haskell tools.

Installing the compiler
-----------------------

- Method 1 - platform first

    #. Install the correct Haskell platform distribution for your system (for instance the package ``cabal-install``).
    #. Install stack. Either from source with ``cabal install stack`` or by downloading it from the `website <https://docs.haskellstack.org>`__.

    Advantages:
    
    With this approach you'll have the ``ghc`` and ``ghci`` as well as ``cabal`` and ``haddock`` commands directly available to you on the command line after installation.

    Disadvantages:

    You'll have to keep your installation of these tools up to date.
    If you later run ``stack install haddock`` to get a newer version of haddock it will shadow the previously globally installed tool.

- Method 2 - stack first

    #. Install stack from the `website <https://docs.haskellstack.org>`__.
    #. Use the ``stack setup`` command on the command line to have stack install ghc for you.

    Advantages:

    No stray ``ghc`` etc. executables.

    Disadvantages:

    You have to separately install haddock with ``stack install haddock`` if you need haddock directly.

- Make sure the stack executable is on your ``$PATH``. 

    Stack can update itself (if you want to). 
    In this case it'll always install the new version in some user-local binary directory (``~/.local/bin`` on UNIX).
    Therefore you also have to ensure this directory is on your ``$PATH``.
    This is also important as all other executables which you install with stack are placed in this directory (including the ones you wrote yourself).

Install some Haskell support for your favourite editor
------------------------------------------------------

You may ask for help for finding said support.

Also consider the website https://wiki.haskell.org/IDEs and https://wiki.haskell.org/Editors.


