
.. _syntax:

Fundamentals of the Haskell syntax
==================================

This lesson gets us started with the basics of Haskell syntax.
Haskell is an old language (older than Java) and also one people like to experiment with.
As a result a lot of extra syntax has accumulated in over the years.
Some of it in regular use, some of it more obscure and not well known.
Most of this extra syntax is hidden behind language extensions.
We may come to learn some of it in future lessons, however for now we will simply start with the ML style core of the Haskell syntax.


.. _comments:

Comments
--------

There are two types of comments in Haskell.
**Line comments** start with a double dash ``--`` (must be followed by a space or word character) and extends to the end of the current line.
**Block comments** start with the sequence ``{-`` and extend until the end sequence ``-}``.

.. _types:

Type literals
-------------

In Haskell type literals always start with an uppercase letter.
Examples from Haskell's base library are:

``Int``
    A fixed size integer

``Integer``
    An unbounded integer.

``Float``
    A floating point number.

``Char``
    A character.

``String``
    A string of characters.

``Bool``
    A boolean.

After that the allowed characters are word characters, digits, the underscore ``_`` and the apostrophe ``'`` (often called *"prime"*). [#type-operators]_
Therefore a name such as ``Isn't_4_bool`` is a valid type name.

In general Haskell is a type inferred language, meaning you rarley have to specify the type of a value or expression (although it is common practice to annotate top level types and values with type signatures).
You can however annotate any value and expression you want with a type signature.
The special operator ``::`` can be used to achieve this (see also next section for examples).
This is particularly useful when chasing down the source of type errors as you can fix expressions to a certain type you expect them to be.

Value literals
--------------

Supported literals are:

Numbers 
    ``1``, ``3.0`` etc. 
    These are however overloaded, meaning depending on the inferred type a literal ``3`` can be an ``Int`` or ``Integer`` for instance.
    If you wish to specify the type you can annotate the literal like so ``(3 :: Int)``, ``(3 :: Integer)`` or ``(3.5 :: Float)``, ``(3.5 :: Double)``.

Characters
    Character literals are constructed by surrounding a character or escape sequence with single quotes.

    ``'a'``, ``'H'``, ``'5'``, ``'.'``.

    The escape character ``\`` is used to produce special values, such as the newline character (``'\n'``)
    For a literal ``\`` character use ``'\\'``.
    For a literal ``'`` character use ``'\''``.

Strings
    String literals are constructed by surrounding a sequence of characters or escape sequences with double quotes.

    ``"Hello World"``, ``"Foo\nBar"``

    The same escape sequences as for characters apply with addition of the escaped double quotes ``\"``.
    [#overloaded-strings]_

Lists
    List literals are a sequence of comma separated values surrounded by square brackets.

    ``[1,2,3,4]``


    All elements of a list must have the same type.

    More on the list type in the next section.

Tuples
    Tuples are a sequence of comma separated values surrounded by parentheses.

    ``(5, "A string", 'c')``

    Unlike lists the elements of a tuple can have different types.

    More on the tuple type in the next section.

Note that there are not *special* literals for booleans in Haskell as they are just a regular :ref:`data structure <user defined types>`.
The literals for ``Bool`` are ``True`` and ``False``.

.. _bindings:

Bindings
--------

Bindings (often also called variables) are names referring to pieces of data.
It is similar to the concept of variables in other languages, however in Haskell bindings are **always** immutable.
Since these ergo they are **not** "variable" (they cannot vary).
This is why I prefer the name "binding" as it **binds** a value to an identifier, not a variable, as it cannot "vary".

Bindings must always start with a lowercase letter.
Then, like the types, it may contain word characters, digits, the underscore and the apostrophe. [#naming-convention]_

There are several ways to bind a value.
The first one we will learn (because it is the way to bind values in GHCi) is called ``let`` with the concrete syntax ``let name = value``.

::

    let myInt = 5
    let aBool = False
    let someString = "Hello World"

You can use ``let`` in GHCi to bind a value and then print it by simply entering the name again and pressing enter. [#rebinding]_
In the ``let`` construct you may also, optionally, specify a type signature for the binding.

::

    let myInt :: Int
        myInt = 5

Note that the second occurrence of ``myInt`` must be properly indented.
We will explore the indentation rules in more detail later.


.. _if:

``if`` expressions
------------------

In Haskell ``if`` is not a statement, but an expression, meaning that it returns a value.
Therefore ``if`` always has a type, and also always has an ``else`` case, which must return a value of the same type.
For instance we can assign the result of ``if`` to a binding.

::

    let aBool = False

    let anInt = if aBool then 8 else 9

Parentheses are not required and one may write any expression on the branches and for the condition of an ``if``.

Function application
--------------------

The syntax for applying functions to arguments in Haskell is the simplest imaginable.
Its called *juxtaposition* or somethimes *prefix notation*.
Meaning we simply write the function and follow it up by the arguments separated by whitespace.
Optionally we can surround the whole construct with parentheses.
This is especially useful when we need the result of a function call as an argument.

::

    succ 5 == 6
    takeDirectory "/etc/hosts" == "/etc"
    elem (pred 6) [1..10]
    not True == False

Haskell also supports binary operators.
For instance the addition operator ``(+)`` and the equality operator ``(==)``.
Note that to apply the operator we use its bare form ``+``, however if we mean a reference to the function we surround it *directly* with parentheses.

::

    4 + 5 == 9
    [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]

    map (uncurry (+)) [(1,2), (4,5)]

Infix operators can also be used in the prefix notation by surrounding them with parentheses ``(+)``.
And prefix functions can also be used like infix operators by surrounding them with backticks ``4 `elem` [1..10]``.

Function application *always binds stronger* to its arguments than operator application.
For operators users may define a prescedence in which they are applied.
Thus ``(+)`` for instance is applied before ``(==)``.


.. rubric:: footnotes

.. [#overloaded-strings] 
    There is a language extension in GHC which allows overloading of strings (much like the numeric literals), see :ref:`overloaded strings`.

.. [#overloaded-lists]
    There is a language extension in GHC which allows overloading of lists (much like the numeric literals), see :ref:`overloaded lists`.

.. [#naming-convention]
    The naming convention in Haskell is camel case. 
    Meaning in each identifier (type variable, type or binding) all words composing the name are chained directly, with each new word starting with an upper case letter, except for the first word, who's case is determined by the syntax contstraints (upper case for types, lower case for type variables and bindings).

.. [#rebinding]
    Note that in GHCi, as in many Haskell constructs you may also **rebind** a binding.
    This may look like you have altered the binding, however this is not the case. 
    It creates a wholly new binding, which simply shadows the older binding in the current scope.
    When the scope is exited the value stored for this name remains the old value.
    You will also know that it is a new binding by the fact that the new binding can have a different type than the old one.

.. [#type-operators]
    GHC also allows you to define `data constructors and types <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#infix-type-constructors-classes-and-type-variables>`__ as operators.
