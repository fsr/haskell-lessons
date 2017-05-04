I/O and ``do`` notation
=======================

The Haskell language is very self contained due to its pure nature.
Consecutive calls to a function with the same input *has* to produce the same result.
This does not allow for interactions with the stateful environment, like accessing the hard disk, network or database.

To separate these stateful actions from the pure Haskell has a type called ``IO``.
The ``IO`` type is used to tag functions and values.
For instance ``IO Int`` means that we can obtain an ``Int`` from this value if we let it execute some interaction with the environment.

A very common type is ``IO ()`` this means the function does I/O and then returns the ``()`` (unit) value.
This value contains no information (similar to ``null``) and ``IO ()`` is basically equivalent to ``void``.
It marks a function which we only want for its *effect*, not its returned *value*.

An nice example of this is the ``getLine`` function.
As you may imagine it reads a single line from stdin and gives us back what was entered.
Its type ``IO String`` then means that it returns a string after doing some interactions with the environment, in this case reading from the ``stdin`` handle.

``do`` ing IO
-------------

IO actions can be chained using the ``do`` syntax.
``do`` syntax is basically what every function body in an imperative language is, a series of statemens and assignments.
One important thing to note is that all statements in a ``do`` block are executed in sequence.

::
    
    main = do
        putStrLn "Starting work"
        writeFile "Output" "work work work"
        putStrLn "Finished work"

As you can see we can use ``do`` to execute several ``IO`` actions in sequence.
We can also obtain the values in from inside those tagged with ``IO``.

::

    main = do
        l <- getLine
        putStrLn ("You entered the line: " ++ l)

The ``binding <- ioExpr`` syntax means "execute the I/O from ``ioExpr`` and bind the result to ``binding``".
Since ``<-`` is only for ``IO`` tagged values you cannot use it for pure ones.
To handle pure values use the statement for of ``let``: ``let binding = expr`` (notice no ``in``).

::

    action :: IO ()
    actions = do
        l <- getLine
        let computed = computeStruff l
        return computed

The ``do`` syntax does however not actualy execute the ``IO``.
It merely combines several ``IO`` actions into a larger ``IO`` action.
The value from the last statement in a ``do`` block is what the whole thing returns.
For instance if the last statement was `putStrLn "some string"` the type of the whole block would be ``IO ()`` (void).
If it was ``getLine`` the type would be ``IO String``.
You can also return non-I/O values from within ``do`` by tagging them with ``IO`` using the ``return`` funciton.



Running ``IO``
--------------

To execute the action there are two ways.

#. GHCi
   If you type an ``IO`` action into ghci it will execute it for you and print the returned value.

#. The ``main`` function.
   
   When you compile and run a Haskell program or interactively run a Haskell source file the compiler will search for a ``main`` function of type ``IO ()`` and execute all the I/O inside it.
   This means you must tie all the I/O you want to do somehow back to the main function.
   This is similar to a C program for instance where the ``int main()`` function is the only one automatically executed and all other routines have to be called from within it.

``do`` Overload
---------------

There are more container and tag types which can be used similar to ``IO``.
To be more precise they can be used with the ``do`` notation, just like ``IO`` can.

Examples of such structures are ``[a]``, ``Maybe a``, ``State s a`` and ``Reader e a``.
Like ``IO`` all these structures represent some kind of context for the contained value ``a``.

We will explore this in more detail later.

For now it suffices that in Haskell these structures are generalized with a typeclass called ``Monad``.
The ``Monad m`` typeclass requires two capabilities: ``return :: a -> m a`` to wrap a value ``a`` into the monad ``m`` and bind ``(>>=) :: m a -> (a -> m b) -> m b`` which basically states that the computations with context (the *Monad*) can be chained.

This is all that is necessary to enable them to use the ``do`` notation.

There is a nice library called `monad-loops`_ which implements many of the control structures one is used to from imperative languages in terms of ``Monad``.

.. _monad-loops: https://www.stackage.org/haddock/lts-8.13/monad-loops-0.4.3/Control-Monad-Loops.html

Also of interest should be the `Control.Monad <base_monad>`_ module from the base library which also contains some generic interactions for monads.
For now it is enough to know that functions with the ``:: Monad m =>`` requirement can be used with ``IO``.

.. _base_monad: https://www.stackage.org/haddock/lts-8.13/base-4.9.1.0/Control-Monad.html
