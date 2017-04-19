.. _types:

Types
=====

.. _type variables:

Type variables
--------------

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

Note that if you wish to annotate a type which uses type variables you will have to fill in the concrete types for those variables *unless* they are unused.
An example:

As you can see from the definition of ``Either`` each type variable is used in one of the constructors.
If you now create one of theses values and whish to annotate it with a type you have to fill in the respective typ variable.
However you do not have to fill in the second variable.
For instance if you create a ``Left`` value, lets say containing a ``String`` it does not matter what type ``b`` is in the resulting ``Either``, becuase the ``Left`` constructor only uses the ``a`` variable and therefore the compiler will allow you to write anything for ``a`` including a type variable (which means it can be anything).
If however you have an expression like the ``if`` which may either return ``Left`` or ``Right`` you have to fill in both types properly.

::

    data Either a b = Left a | Right b

    x :: Either String b
    x = Left "A String" 
    y :: Either a Int
    y = Right 1 

    x_and_y :: Either String Int
    x_and_y = if someBool then x else y 

We could also have annotated ``x`` and ``y`` with concrete types for the respective other variable, however in that case we must make it the type the ``if`` expression expects it to be or we get a type error.
Therefore is is usually advisable to leave the type unspecified unless necessary.

::

    data Either a b = Left a | Right b

    -- these definitions are ok 
    -- because the type lines up with the if expression

    x :: Either String Int
    x = Left "A String" 
    y :: Either String Int
    y = Right 1 

    -- these definitions are problematic
    -- they would cause a type error

    x :: Either String Bool
    x = Left "A String" 
    y :: Either (Either String String) Int
    y = Right 1 

    x_and_y :: Either String Int
    x_and_y = if someBool then x else y


If you don't know the type of an expression but wish to annotate it or you dont know the value of one of the type variables you can use a so called "type hole" to have the compiler figure it out for you.
If you annotate an expression with ``_`` the compiler will throw an error and tell you what it infers the type for ``_`` to be.
You can use multiple ``_`` at the same time each of which will cause a compile error with information about the inferred type.
This can be used for full type signatures or even just parts of it, including type variables.
GHC generally tries to infer the most general type for you.

::

    -- infer a full type signature
    x :: _
    x = Left "A String"

    -- Infer a variable
    y :: Either a _
    y = Right 1


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

    data File = TextFile String | Binary Bytes

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
Type names only ever occur in a place where a type can occur, such as in the definition of another type and type signatures whereas a *Constructor* can occur in any epression.

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

The ``case`` construct together with function application basically comprises everything which you can do in Haskell.
The ``case`` construct is used to deconstruct a type and gain access to the data contained withtin.


This is easiest to see with a user defined type

::

    data MyType = Constr1 Int

    aValue = Constr1 5 :: MyType
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

    aValue = Constr1 5 :: MyType
    theIntWithin =
        case aValue of
            Constr1 i -> i
            x -> 0

Match clauses are always matched in sequence, from top to bottom until a matching clause is found.
A clause like ``x``, which does not contain a constructor will always match.
Therefore it is usually found as the last clause, often serving as a kind of default clause.
If the default clause does not need the value we often use ``_`` as binding to indicate that we do not use the value.

The ``case`` is an immensely powerful control structure as all other control structures can be defined in terms of ``case`` and function application.
For instance we can define an ``if`` using case.

::

    if cond a b =
        case cond of
            True -> a
            False -> b

You can also pattern match on all primitive, built-in types such as ``Char``, ``[]``, ``String``, ``Int``, ``Float`` and so on. Anything you can write as a literal you may use in a case pattern.

::

    isC char = case char of
                    'c' -> True
                    _ -> False
    
    isC 'l' == False
    isC 'c' == True

    is4 n = case n of 
                4 -> True
                _ -> False

    is4 4 == True 
    is4 0 == False

Different ways to write a case expression
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Case expressions can either be written using indentation, or semicolons and braces in the same way we can do with ``let``. 
Thereby we can use ``;`` to omit newlines and ``{}`` to omit the indentation.
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
        Constr2 f -> 
            resultExpr2 

    -- indent is replaced with semicolons and braces
    case expr of { Constr1 field1 field2 -> resultExpr; Constr2 f -> resultExpr2 }

.. _special types:

Special types
-------------

There are some notable exceptions to the type naming rule.
Those are the **list type**, which is ``[]`` or ``[a]`` which means "a list containing elements of type ``a``" and the **tuple type** ``(a,b)`` for "a 2-tuple containing a value of type ``a`` and a value of type ``b``".
There are also larger tuples ``(a,b,c)``, ``(a,b,c,d)`` etc. [#tuple-size]_
These tuples are simply grouped data and very common in mathematics for instance.
Should you not be familiar with the mathematical notion of tuples it may help to think of it as an unnamed struct where the fields are accessed by "index".
And the last special type is the **function type** ``a -> b``, which reads "a function taking as input a value of type ``a`` and producing a value of type ``b``.

Some examples for concrete instances of special types:

::

    myIntBoolTriple :: (Int, Int, Bool)
    myIntBoolTriple = (5, 9, False)
    
    aWordList = ["Hello", "Foo", "bar"] :: [String] -- Note: A different way to annotate the type

    -- Note: we can also nest these types
    listOfTuples :: [(Int, String)]
    listOfTuples = 
        [ (1, "Marco")
        , (9, "Janine")
        ]


.. _record syntax:

Record syntax
-------------

For convenience reasons there is some extra syntax for defining data types which also automatically creates some field accessor functions.

We can write the following:

::

    data MyType = 
        Constructor { field1 :: Int
                    , field2 :: String
                    }

This defines the type the same way as the other ``data`` construct.
Meaning we can pattern match as usual on the constructor.


::

    theData = Constructor 9 "hello" :: MyType
    theInt = case theData of
                Constructor i _ -> i
    
    theInt == 9

But additionally it also defines two functions ``field1`` and ``field2`` for accessing the fields.

Aka it generates code similar to the following:

::

    data MyType = Constructor Int String

    field1 :: MyType -> Int
    field1 (Constructor i _) = i

    field2 :: MyType -> String
    field2 (Constructor _ s) = s

Also the two accessor functions ``field1`` and ``field2`` may be used in a special *record update syntax* to create a new record from an old one with altered field contents.
Additionally the record may be created with a special record creation syntax.

::

    data MyType = 
        Constructor { field1 :: Int
                    , field2 :: String
                    }

    v1 = Constructor 9 "Hello" :: MyType
    
    -- record creation syntax
    v2 = Constructor { field2 = "World", field1 = 4 } :: MyType

    -- update syntax
    v3 = v2 { field1 = 9 }
    -- updating multiple fields at once
    v4 = v2 { field1 = 9, field2 "Hello" }

    v1 == v4

    -- old records are unchanged
    v2 /= v3 /= v4

And finally it also enables a special record pattern match using the fields.

::

    theData = Constructor 9 "hello" :: MyType

    theInt = case theData of
                Constructor{ field1 = i } -> i

    theInt == 9

.. rubric:: footnotes

.. [#naming-convention]
    The naming convention in Haskell is camel case. 
    Meaning in each identifier (type variable, type or binding) all words composing the name are chained directly, with each new word starting with an upper case letter, except for the first word, who's case is determined by the syntax contstraints (upper case for types, lower case for type variables and bindings).

.. [#tuple-size] 
    The `source file for tuples in GHC <https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/src/GHC.Tuple.html#%28%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C%29>`__ defined tuples with up to 62 elements.
    Below the last declaration is a large block of perhaps 20 more declarations which is commented out, with a note above saying "Manuel says: Including one more declaration gives a segmentation fault."
