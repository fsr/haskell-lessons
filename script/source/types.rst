
Types
=====

.. _user defined types:


User defined types
------------------

Defining types in Haskell takes three forms.

.. _type alases:

Aliases
^^^^^^^

The ``type`` keyword allows us to define a new name for an existing type.
This can have two different purposes:

#. It allows us to define shorter names for long type.
    For instance

    ::

        type MakerM a = StateT (ALongStateName String Bool (HashMap Text Int)) (LoggingT IO) a

#. We can abstract our API from the concrete type.
    If our program uses a Map like structure for instance, but we are not sure yet that we want to stick with a concrete Map type we might write the following:

    ::

        type MyMap key value = HashMap key value
        -- or (omitting the `value` variable)
        type MyMap key = HashMap key
        -- or (omitting both the `value` and `key` variable)
        type MyMap = HashMap
    
    We can then later replace it with a different map type if we like and we do not need to change all of our type signatures.

    ::

        type MyMap = Map

As you can see from these examples like in function signatures type aliases support polymorphism via type variables and the type varables support partial application like functions.

.. _algebraic datatypes:

Algebraic datatypes
^^^^^^^^^^^^^^^^^^^

Algebraic datatypes are the "normal" user defined datatypes in Haskell.
They are richer than datatypes from other laguages such as Java classes or C structs in that each type can have more (or less) than one representation.
Some modern languages such as Rust and Swift also support those types of data.
They call them Enums.

A type is defined using the ``data`` keyword, followed by the name of the type, which must begin with an upper case letter (see also :ref:`here <types>`), followed by an equal sign.
This is followed by any number of ``|`` separated *constructor definitions*.

::

    data Coordinates = LongAndLat Int Int

    data Maybe a = Nothing | Just a

A constructor definition takes the form of first the constructor itself, followed by any number of type arguments, which are the types of the fields in the constructor.
The naming constraints for the constructor are the same as for :ref:`types`.[#type-operators]

Constructors serve two purposes.

#. They are used, through normal function application, to *construct* a value of their type.
    You can think of any constructor (like ``Coordinates``) as a function, wich takes arguments according to the number and type of its fields and produces a value of its type.

    ::

        LongAndLat :: Int -> Int -> Coordinates
    
    These constructors can be used just just like any other function, which includes partial application and being arguments to higher order functions.

    ::

        LongAndLat 8 :: Int -> Coordinates

        map (LongAndLat 9) [0,9,15] == [LongAndLat 9 0, LongAndLat 9 9, LongAndLat 9 15]

#. They are used in a pattern match to *deconstruct* a value of their type and gain access to its fields. (See :ref:`next section <case>`)

It is very important to know the difference between a *type(name)* and a *constructor* in Haskell.
Also not that it is allowed for a type and a constructor with the same name to be in scope, as the distinction between the two can be made from the context in which they are used.
Type names only ever occur in a place where a type can occur, such as in the definition of another type and type signatures.

.. admonition:: Aside
    
    There are more ways to control type variables in Haskell using a :ref:`generalised concept of algebraic datatypes <GADTs>`.

.. _newtypes:

Newtypes
^^^^^^^^

Newtypes are basically a stricter version of the ``type`` alias.
To be more concrete a ``newtype`` is a wrapper for another type which completely hides the wrapped type.

The syntax is very similar to a ``data`` definition, with two important restrictions.

#. The newtype must have exactly *one* constructor.
#. The constructor must have exactly *one* field.

What is so special about the newtype is that even though it may look like a ``data`` definition the newtype does not exist at runtime and thus has no runtime overhead.
It is typically used to impose some restrictions on the creation of a type.

Whereas aliases created with ``type`` may be used in just the same way that the type they alias can be used a ``newtype`` creates a completely new type and the functions which work on the inner type *do not* work on the new type.

In the following example for instance we force the user to go through the ``createEmail`` function to construct an ``Email`` type.
if we used a ``type`` alias the user could simply pass a ``String`` to the ``sendEmail`` function, becuase it is just an alias, but types created with ``newtype`` are distinct from the type they wrap and thus ths would cause a type error.

::

    newtype Email = Email String

    createEmail :: String -> Either String Email
    createEmail str =
        if conformsToEmailStandard str
            then Right (Email str)
            else Left "This is not a valid email"
    
    sendEmail :: Email -> String -> IO ()


.. _case:

The ``case`` construct
----------------------

The ``case`` construct together with function application basically comprise everything which you can do with Haskell at runtime.
The ``case`` construct is used to deconstruct a type and gain access to the data contained withtin.


This is easiest to see with a user defined type

::

    data MyType = Constr1 Int

    let aValue = Constr1 5 :: MyType
        theIntWithin =
            case aValue of
                Constr1 i -> i

    theIntWithin == 5

Any Haskell expression is allowed in the ``case <expr> of`` head of the construct.
The body of the case statement is a number of ``matchclause -> expr`` pairs.

Each match clause is a combination of constructors and bindings for values.
The expression to the right of the arrow may then use the values bound by these bindings.

A very simple case match (which does absolutely nothing) would be

::

    case expr of
        x -> doSomething x
    

Which is the same as ``doSomething expr``.
We simply bind the expression to ``x``.

However this is often used to create a default clause for a case match.

::

    data MyType = Constr1 Int | Constr2 String

    let aValue = Constr1 5 :: MyType
        theIntWithin =
            case aValue of
                Constr1 i -> i
                x -> 0

Match clauses are alwas matched in sequence, from top to bottom until a matching clause is found.
A clause like ``x``, which does not contain a constructor will always match.
Therefore it is usually found as the last clause, often serving as a kind of default clause.
If the default clause does not need the value we often use ``_`` as binding to indicate that we do not use the value.

The ``case`` is an immensely powerful control structure as all other control structures can be defined in terms of ``case`` and function application.
For instance we can define an ``if`` using case.

::

    let if cond a b =
            case cond of
                True -> a
                False -> b

You can also pattern match on primitive, built-in types.

::

    let isC char = case char of
                        'c' -> True
                        _ -> False
    
    isC 'l' == False
    isC 'c' == True

    let is4 n = case n of 
                    4 -> True
                    _ -> False

    is4 4 == True 
    is4 0 == False

Different ways to write a case expression
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Case expressions can either be written using indentation, or semicolons and braces.
The following definitions are equivalent

::
    
    case expr of
        -- note the indent of the match clauses
        Constr1 field1 field2 -> resultExpr 
        Constr2 f -> resultExpr2

    case expr of
        Constr1 field1 field2 -> 
            -- note the deeper indent for the result expression
            resultExpr
        Constr2 f -> resultExpr2 

    -- indent is replaced with semicolons and braces
    case expr of { Constr1 field1 field2 -> resultExpr; Constr2 f -> resultExpr2 }


.. _type variables:

Type variables and special types
--------------------------------

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
Those are the **list type**, which is ``[]`` or ``[a]`` which means "a list containing elements of type ``a``" and the **tuple type** ``(a,b)`` for "a 2-tuple containing a value of type ``a`` and a value of type ``b``".
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


.. _record syntax:

Record syntax
-------------



