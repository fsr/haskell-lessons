Simple I/O
==========

Reverser
--------

Write an application which reads a line from stdin and prints the same string back, but reversed.

Modify it so it keeps repeating this process forever.

Bulk rename
-----------

You will need the `filepath`_ and `directory`_ library.

Implement a function which:

- takes as argument a string
- then scans the current directory and finds all files
- renames each file by prepending the string argument to the filename

.. _filepath: https://www.stackage.org/lts-8.13/package/filepath-1.4.1.1
.. _directory: https://www.stackage.org/lts-8.13/package/directory-1.3.0.

Advanced
^^^^^^^^

Get the input string from the application arguments [#getArgs]_.

Combine files
-------------

You will need the `filepath`_ and `directory`_ library.

Implement a function which:

- Scans the current directory
- finds all files
- reads each file and collects the contents in a list
- writes a combined output file with the contents of each of the files concatenated.

Advanced
^^^^^^^^

Instead of collecting to a list open the output file first and write each files contents to the output handle right away.

.. rubric:: footnotes

.. [#getArgs] Use the `getArgs <https://www.stackage.org/haddock/lts-8.13/base-4.9.1.0/System-Environment.html#v:getArgs>`__ function.

