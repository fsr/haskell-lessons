A custom boolean
================

We want to define a boolean type. 
The standard library already has a type ``Bool`` but we will make your own.

*Note:* this exercise is intended to be solved using both a Haskell source file and ghci.
My recommendation is to implement the code in a file (``Something.hs``) then open ``ghci`` and load the file with the ``:load FileName.hs`` (this has autocompletion for the file name as well) command.
After that the types and functions you defined in the file will be in scope and you can play around with them.

*Note:* In ghci bindings must be created with ``let binsing = expr``.

Defining the type
-----------------

Define a new type called ``Boolean`` with two constructors ``Yes`` and ``No``. [#defining]_

``if``
------

Booleans are used for ``if`` expressions.
Therefore we will define our own ``if``.
Since ``if`` is a keyword in Haskell you can use a name like ``if_`` or ``if'`` or something else.

Your ``if`` function should take three arguments.

#. A ``Boolean`` (your custom boolean type) as condition.
#. A value which to return when the boolean is ``Yes``.
#. A value which to return when the boolean is ``No``.

1. Implement the function [#implementing_if]_
2. Add a signature to your ``if`` [#if_signature]_

Boolean operations
------------------

We also need to be able to define more complex interactions.
Implement a ``not'``, ``or'`` and ``and'`` operation which, as the names suggest, do boolean *not*, *and* and *or*. [#boolean_operations]_

Finally play around some with the operations you have defined.
Make sure they are indeed correct.

.. rubric:: footnotes

.. [#defining] Use the ``data`` keyword
.. [#implementing_if] Use a ``case`` construct to match on the two ``Boolean`` constructors.
.. [#if_signature] 
    Use a type variable for the two values.
    If you're stuck think about what you know about the two values (do they perhaps have the same type? Aka the type variable needs to be the same) and what type would the return be?

.. [#boolean_operations] 
    A nested ``case`` match should be useful here.
    Alternatively you can match on both booleans simultaneously if you wrap them in a tuple *or* if you match them with the special function syntax on the arguments.



