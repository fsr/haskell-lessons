Typeclasses
===========

In addition to the *parametric* polymorphism of type variables Haskell offers *ad-hoc* polymorphism via a concept called *type classes*.
Conceptually a *type class* groups a set of types for which there exists a common behaviour.

Practically a typeclass is the same as an interface in Java or C#.
It defines a set of methods which a must be implemented for a certain type.

Defininig classes
-----------------

The class is defined with the keyword ``class`` (think ``interface`` in Java) followed by a name for the class. [#class_names]_
Following this is a type variable [#multi_param_classes]_ which is a reference for the actual type.
This variable is subsequently used in the method signatures to reference the type.
For instance ``Ord`` is used to implement ordering for values.

:: 

    class Ord a where

Member functions
^^^^^^^^^^^^^^^^

In the body of the definition follows a number of declaraions for whats called *methods*.
Methods are functions which must be implemented for a type to be member of this class.
Below is an excerpt of the ``Ord`` typeclass as an example.

::

    class Ord a where

        compare :: a -> a -> Ordering
        (<=) :: a -> a -> Bool
        max :: a -> a -> a

Superclasses
^^^^^^^^^^^^

Classes can have a so called *superclasses*. 
This essentially just defines another class to be a dependency for the declaration of an instance of this class.
In short: A class can depend on another class.

::

    class Eq a => Ord a where

        compare :: a -> a -> Ordering
        (<=) :: a -> a -> Bool
        max :: a -> a -> a

Default implementations
^^^^^^^^^^^^^^^^^^^^^^^

Lastly methods of the class can have default implementation in which may use both other methods of the class or methods of the superclasses.

::

    class Eq a => Ord a where

        compare :: a -> a -> Ordering
        compare x y = if x == y then EQ
                    else if x <= y then LT
                    else GT

        (<=) :: a -> a -> Bool
        x <= y = case compare x y of { GT -> False; _ -> True }

        max :: a -> a -> a
        max x y = if x <= y then y else x


Constraining types
------------------

Unlike in Java where, if we wish to use an interface, we simply declare the type to *be* the interface in Haskell we *constrain* the type to *implement* the class.
Constraints precede the arguments and return type in a type signature.
Constraints are always placed on *type variables*.
An ascii double arrow separates the constraints and the rest of the type signature.

::

    max3 :: Ord a => a -> a -> a -> a
    max3 a1 a2 a3 = a1 `max` a2 `max` a3

The advantage of this is that we can require *multiple* classes for a single type.
In this case the constraints are listed comma separated and surrounded by parentheses.

::

    showMax3 :: (Show a, Ord a) => a -> a -> a -> String
    showMax3 a1 a2 a3 = show (max3 a1 a2 a3)

Implementing classes
--------------------

The Haskell model of implementing classes is similar to that of `Rust`_ and `Swift`_.
Like in those languages an instance of the class (interface) can be declared anywhere.
It does not have to happen at the place where the type is defined.
The only constraint is that there must not be an existing instance to the class in scope.
Typically the instances of the class are either defined where the type is defined or where the class is defined.
This prevents situations where two instances of the same class for the same type are in scope.

Implementations of the class are done using the ``instance`` keyword, otherwise they are very similar to the class declaration.
The ``instance`` keyword is followed by the class name and then the type name for which the instance is to be declared.

.. _Rust: https://www.rust-lang.org
.. _Scala: https://www.scala-lang.org
.. _Swift: https://swift.org

::

    data MyType = TheSmallest | TheMiddle | TheLargest

    instance Eq MyType where

    instance Ord MyType where
        

In the body of the declaration follow definitions for each of the methods of the class.

::

    data MyType = TheSmallest | TheMiddle | TheLargest

    instance Eq MyType where
        TheSmallest == TheSmallest = True
        TheMiddle   == TheMiddle   = True
        TheLargest  == TheLargest  = True
        _           == _           = False

    instance Ord MyType where

        compare TheSmallest TheSmallest = EQ -- EQual
        compare TheLargest TheLargest = EQ 
        compare TheMiddle TheMiddle = EQ
        compare TheSmallest _ = LT -- Less Then
        compare TheLargest  _ = GT -- Greater Then
        compare _ TheLargest = LT
        compare _ TheSmallest = GT

        TheSmallest <= _ = True
        _ <= TheLargest = True
        TheLargest <= _ = False
        _ <= TheSmallest = False


Deriving classes
^^^^^^^^^^^^^^^^

For some classes like ``Eq``, ``Ord``, ``Show`` and ``Read`` you may let GHC automatically define an instance for you.
This is done using the ``deriving`` keyword after the type definition.
The exact semantics of those derived classes can be found in the `ghc manual <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/>`__.

::

    data T = A | B Int deriving (Show, Eq, Ord)



.. rubric:: footnotes

.. [#class_names] The naming schema for class names is the same as for types and constructors.

.. [#multi_param_classes] Using the ``MultiParamTypeClasses`` language extensions allows one to define type classes over multiple parameters.
