Typeclasses
===========

In addition to the *ad-hoc* polymorphism of type variables Haskell offers another type of polymorphism via a concept called *type classes*.
Conceptually a *type class* groups a set of types for which there exists a common behaviour.

Practically a typeclass is the same as an interface in Java or C#.
It defines a set of methods which a must be implemented for a certain type.

Defininig classes
-----------------

The class is defined with the keyword ``class`` (think ``interface`` in Java) followed by a name for the class. [#class_names]_
Following this is a type variable [#multi_param_classes]_ which is a reference for the actual type.
This variable is subsequently used in the method signatures to reference the type.

:: 

    class MyTypeClass typeReference where

In the body of the definition follows a number of declaraions for whats called *methods*.
Methods are functions which must be implemented for a type to be member of this class.

::

    class MyTypeClass typeReference where

        theFirstMethod :: typeReference -> String

        theSecondMethod :: String -> typeReference

Classes can have a so called *superclasses*. 
This essentially just defines another class to be a dependency for the declaration of an instance of this class.
In short: A class can depend on another class.

::

    class TheSuperclass typeReference => MyTypeClass typeReference where

        theFirstMethod :: typeReference -> String

        theSecondMethod :: String -> typeReference

Lastly methods of the class can have default implementation in which may use both other methods of the class or methods of the superclasses.

::

    class Monad m => MonadState m where
        type State m
        get :: m (State m)
        get = state (\s -> (s, s))

        put :: State m -> m ()
        put s = state (\_ -> ((), s))

        state :: (State m -> (a, State m)) -> m a
        state f = do
            s <- get
            let ~(a, s') = f s
            put s'
            return a

Constraining types
------------------

Unlike in Java where, if we wish to use an interface, we simply declare the type to *be* the interface in Haskell we *constrain* the type to *implement* the class.
The advantage of this is that we can require *multiple* classes for a single type.

Implementing classes
--------------------

Implementations of the class are done using the ``instance`` keyword otherwise are very similar to the class declaration.
The ``instance`` keyword is followed by the class name ant then the type name for which the instance is to be declared.

::

    instance MyTypeClass AType where

In the body of the declaration follow definitions for each of the methods of the class.

::

    instance MyTypeClass AType where




.. rubric:: footnotes

.. [#class_names] The naming schema for class names is the same as for types and constructors.

.. [#multi_param_classes] Using the ``MultiParamTypeClasses`` language extensions allows one to define type classes over multiple parameters.
