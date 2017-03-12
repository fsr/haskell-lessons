.. _functions:

Functions
=========


.. _lambdas:

Function literals
-----------------

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

When we have a value of the function type we may apply it to an argument of the type *left* of the arrow to obtain a value of the type *right* of the arrow.
Ergo ``Int -> Bool`` applied to ``Int`` gives a ``Bool``.
Similarly ``a -> a`` applied to ``Int`` gives an ``Int`` again.
An ``a -> a`` applied to a ``Bool`` gives a ``Bool``.

To apply a function we ise the simplest synta of all, juxtaposition.
Also called *postfix notation* or "the function followed by the arguments, all space separated".

::

    let id :: a -> a
        id = \x -> x

        myBool = id True
        myBool2 = (\x -> x) True
        myInt = id 5

    myBool == myBool2 == True && myInt == 5

Lets look at another example fuction:

::

    let const :: a -> b -> a
        const = \x _ -> x
    
The ``const`` function takes a first parameter ``x`` and a second parameter, which we ignore.
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

The practical upshot of this is that haskell makes it extremely easy to do what is often called "partially applied functions".
This means supplying fewer arguments to a function than would be required to produce its final value.
Technically this is not even possible in Haskell, since, as we have seen above, every Haskell function only takes one argument but may return a curried function to simulate being given a second argument.
To fully grasp the possibilities that partial application offers it is instrumental to internalise this aforementioned concept.

Partial application is best described using examples:

::

    let const :: a -> b -> a
        const x _ = x

        alwaysFive = const 5
    
    alwaysFive "a string" == alwaysFive 6 == alwaysFive () == 5

    let plusThree = (+ 3)

    plusThree 5 == 8
    plusThree 10 == 13

.. admonition:: Aside

    This is particularly useful when combined with :ref:`higher order functions`.

    For instance we can increment a whole list of integers using the partial application of ``+`` to ``1``.

    ::

        map (+ 1) [4,5,8] == [5,6,9]
    
    Or to find the index of a particular element in a list: (partial application of ``==``)

    ::

        find (== 6) [3,6,8] == Just 2

    Note that these are advanced examples, there is no need to undestand them yet, we will cover those in detail later.


Syntactic sugar for function definitions
----------------------------------------

There are a few common patterns in Haskell when defining functions.
The first is for creating function values.

::

    myFunction = \a b -> doSomthing

    let anotherFunction = \x -> expr

This pattern is very common.
Therefore we have some syntactic sugar in the Haskell laguage which allows us to omit both the backslash ``\`` and the arrow ``->`` by moving the function arguments before the equal sign.

::

    myFunction a b = doSomthing

    let anotherFunction x = expr

Another common pattern is to take arguments to a function and immediately perform a ``case`` match on them.

::

    myFunction a = 
        case a of  
            Constr1 val -> ...
            Constr2 v2 -> ...

Instead we may write

::

    myFunction (Constr1 val) = ...
    myFunction (Constr2 v2) = ...

.. note:: 
    Here we must use parentheses around the match clauses to distinguish the clauses for several arguments.

