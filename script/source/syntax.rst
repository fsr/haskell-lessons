
.. _syntax:

Haskell Syntax
==============

.. _comments:

Comments
--------

There are two types of comments in Haskell.
**Line comments** start with a double dash ``--`` (must be followed by a space or word character) and extends to the end of the current line.
**Block comments** start with the sequence ``{-`` and extend until the end sequence ``-}``.

.. _types:

Types
-----

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

After that the allowed characters are word character, digits, the underscore ``_`` and the apostrope ``'`` (often called *"prime"*).
Therefore a name such as ``Isn't_4_bool`` is a valid type name.

In general Haskell is a type inferred language, meaning you rarley have to specify the type of a value or expression (although it is common practice to annotate top level types and values with type signatures).
You can hovever annotate any value and expression you want with a type signature.
The special operator ``::`` can be used to achieve this (see also next section for examples).
This is particularly useful when chasing down the source of type errors as you can fix expressions to a certain type you expect them to be.

Literal values
--------------

Supported literals are:

Numbers 
    ``1``, ``3.0`` etc. 
    These are however overloaded, meaning depending on the inferred type a literal ``3`` can be an ``Int`` or ``Integer`` for instance.
    If you wish to specify the type you can annotate the literal like so ``(3 :: Int)``, ``(3 :: Integer)`` or ``(3.5 :: Float)``, ``(3.5 :: Double)``.

Characters
    Character literals are constructed by surrounding a character or excape sequence with single quotes.

    ``'a'``, ``'H'``, ``'5'``, ``'.'``.

    The escape character ``\`` is used to produce special values, such as the newline character (``\n``).

Strings
    String literals are constructed by surrounding a sequence of characters or escape sequences with double quotes.

    ``"Hello World"``, ``"Foo\nBar"``

    The same escape sequences as for characters apply with addition of the excaped double quotes ``\"``.
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

Data literals
    For instance the boolean literals ``True`` and ``False``.

    We will come back to this in more depth when we learn about :ref:`user defined types`.

.. _bindings:

Bindings
--------

Bindings (often also called variables) are names referring to pieces of data.
It is similar to the concept of variables in other languages, however in Haskell bindings are **always** immutable.
Since these ergo they are **not** "variable" (they cannot vary).
This is why I prefer the name "binding" as it **binds** a value to an identifier, not a variable, as it cannot "vary".

Bindings must always start with a lowercase letter.
Then, like the types, it may contain word characters, digits, the underscore and the apostrope. [#naming-convention]_

There are several ways to bind a value.
The first one we will learn is called ``let`` with the concrete syntax ``let name = value``.

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

.. _type variables:

3. Type variables and special types
-----------------------------------

Types in Haskell may be parameterized over another type, which is not known at the time of defining the former type.
This system is very similar to generics in many languages, but much more powerful as the type information is fully preserved.

The naming rules for type variables are the same as for :ref:`bindings`. [#naming-convention]_

The whole type is then written as first the type name followed by a space and then followed by the parameters, also space separated.
This is also called juxtaposition.

As an example for a parameterized type is the ``Either a b`` type. 
The name of the type is ``Either`` and it is parameterized by a type variable ``a`` and a type variable ``b``.
Note that there is no special significance to the name of the type variables themselves. 
It would be semantically equivalent to call the type ``Either one the_other``.
Only if we were to name both variables the same would we change the meaning, because ``Either a a`` would mean **both** types ``Either`` is parameterized over are the **same** type.

We have now seen the type in its generic form.
By instantiating the type variables we can create a concrete form.
For instance ``Either Int String`` or ``Either Bool Char``.
Note that ``Either a b`` does not mean that ``a`` and ``b`` **have** to be distinct, but they are allowed to.
``Either Int Int`` is also a perfectly valid concrete form of ``Either a b``.

At compile time all of the type parameters must be known, i.e. only concrete form of types are allowed.
The compiler will infer the concrete values of the type variables for you.

.. _special types:

Special types
^^^^^^^^^^^^^

There are some notable exceptions to the type naming rule above.
Those are the **list type**, which is ``[]`` or ``[a]`` which means "a list containing elements of type ``a``" and the **tuple type** ``(a,b)`` for "a 2-tuple containing a value of type ``a``and a value of type ``b``".
There are also larger tuples ``(a,b,c)``, ``(a,b,c,d)`` etc. [#tuple-size]_
These tuples are simply grouped data and very common in mathematics for instance.
Should you not be familiar with the mathematical notion of tuples it may help to think of it as an unnamed struct where the fields are accessed by "index".
And the last special type is the **function type** ``a -> b``, which reads "a function taking as input a value of type ``a`` and producing a value of type ``b``.

Some examples for concrete instances of special types:

::

    let myIntBoolTriple :: (Int, Int, Bool)
        myIntBoolTriple = (5, 9, False)
    
    let aWordList = ["Hello", "Foo", "bar"] :: [String] -- Note: A different way to annotate the type

    -- Note: we can also nest these types
    let listOfTuples :: [(Int, String)]
        listOfTuples = 
            [ (1, "Marco")
            , (9, "Janine")
            ]

.. _functions:

Functions
---------

Function literals in Haskell are also often called **lambda functions**.
The syntax is a slash ``\`` followed by a list of space separated paramters, follwed by an ASCII arrow ``->`` upon which follows the body of the function.
Function bodies in Haskell are always an expression, and as such require no ``return`` keyword.

::

    \ param -> param

Here for instance we have a function which takes one parameter as input and return it.
This function is also known as ``id``.

::

    -- we often call an unspecified parameter 'x'
    let id = \x -> x

Haskell is a functional language. 
As such functions may be used just like any other value including being assigned to bindings.
The type of our binding is now the function type.

:: 

    let id :: a -> a
        id = \x -> x

.. todo:: Function application

Lets look at another example:

::

    let const :: a -> b -> a
        const = \x _ -> x
    
The const function takes a first parameter ``x`` and a second parameter, which we ignore.
The underscore ``_`` as a parameter or binding name is used to indicate that we ignore the value.
And finally the function returns the first parameter.

Note that the type of the function is now ``a -> b -> a``.
We see here that the function type ``->`` occurs twice and this is deliberate because we may rewrite our function as follows:

::

    let const :: a -> (b -> a)
        const = \x -> \_ -> x

Now we can see the analogy. 
We first consume the first parameter and return a function.
This second function is then applied to the second parameter returning the final value.
The two versions ``\x _ -> x`` and ``\x -> \_ -> x`` and their type signatures are equivalent in Haskell, hence the same type.

.. rubric:: footnotes

.. [#tuple-size] 
    The `source file for tuples in GHC <https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/src/GHC.Tuple.html#%28%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%29>`__ defined tuples with up to 62 elements.
    Below the last declaration is a large block of perhaps 20 more declarations which is commented out, with a note above saying "Manuel says: Including one more declaration gives a segmentation fault."

.. [#overloaded-strings] 
    There is a language extension in GHC which allows overloading of strings (much like the numeric literals), see :ref:`overloaded strings`.

.. [#overloaded-lists]
    There is a language extension in GHC which allows overloading of lists (much like the numeric literals), see :ref:`overloaded lists`.

.. [#naming-convention]
    The naming convention in Haskell is camel case. 
    Meaning in each identifier (type variable, type or binding) all words composing the name are chained directly, with each new word starting with an upper case letter, except for the first word, who's case is determined by the syntax contstraints (upper case for types, lower case for type variables and bindings).

.. [#rebinding]
    Note that in GHCi, as in many Haskell constructs you may also **rebind** and a binding.
    This may look like you have altered the binding, however this is not the case. It creates a wholly new binding, which simply shadows the older binding in the current scope.
    When the scope is exited the value stored for this name remains the old value.
    You will also know that it is a new binding by the fact that the new binding can have a different type than the old one.
