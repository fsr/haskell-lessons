Implementing a library for safe html construction
=================================================

We want to build a library which we can use to programmatically build a html website in Haskell and then render it.

*Note:* this exercise is intended to be solved using both a Haskell source file and ghci.
My recommendation is to implement the code in a file (``Something.hs``) then open ``ghci`` and load the file with the ``:load FileName.hs`` (this has autocompletion for the file name as well) command.
After that the types and functions you defined in the file will be in scope and you can play around with them.

*Note:* In ghci bindings must be created with ``let binding = expr``.

A base type
-----------

First we need a basic ``Html`` type.
For now this is just going to be a wrapper around a ``String`` containing the actiual html.

Define the ``Html`` type as a wrapper around ``String``. [#defining_html]_

Don't expose your constructor to the user of the library [#exposing]_ so that they cannot unsafely create ``Html`` values from ``String``.

Also create a function ``render`` or ``renderHtml`` which takes a ``Html`` value and returns it in rendered ``String``.
In this case that's simply the ``String`` contained in the ``Html`` value.
You'll then be able to use this function in the subsequent tasks to look at the ``Html`` values and verify you have implemented your manipulation functions correctly.

Creating html from strings
--------------------------

Now we need the user to be able to create ``Html`` values from strings, but we want that to be safe.
First we will enable them to create just html text nodes.
Html text nodes may not contain any of the special html characters like ``&``, ``<``, ``>``.
Write a function ``mkTextNode`` which takes a ``String`` as input and verifies that none of the above mentioned characters are in it. [#verifying]_
If one of the characters is found raise an ``error`` and if not return a ``Html`` value containing the string.

Concatenating html
------------------

Html elements can also be consecutive.
Like ``<div>...</div><span>...</span>``.

Write a function which takes as input *two* ``Html`` values and returns a ``Html`` value which is the concatenation of the two input ``Html`` values. [#concatenating]_

Html containers
---------------

Now we want to be able to use things like html ``div`` and ``span``.
Write at least two functions which implement one of the html containers like ``i``, ``div`` or ``span``.
I recommend calling the ``mkDiv`` and ``mkSpan`` etc.
For now we will not add any attributes to these containers.
They should accept a ``Html`` value as input and return a ``Html`` value.
And what they should do is add the respective opening and closing tags around the html value they have received as input. [#containers]_

Html documents
--------------

Now we want to model a whole html document.
First we will need to model the doctype.

#. Create a ``Doctype`` type with constructors for some of the most common html versions: ``Html`` (for html4) ``Html5`` (for html5) and ``XHtml``.

#. For the document itself we will create a ``Document`` type.
    This type should have three fields.

   #. ``doctype :: Doctype``
   #. ``headSection :: Html``
   #. ``bodySection :: Html``
   
   Implement this type using record syntax.
   This allows us to manipulate the fields later.

#. Lastly we need a way to render it.

   Create a ``renderDocument`` function which returns a string that is the concatenation of:

   * The correct doctype string for the ``Doctype``
   * The head html wrapped in ``<head></head>``
   * The body html wrapped in ``<body></body>``

Making the html editable
------------------------

Until now we have only used ``String`` for the internal html.
However we can do better.
We want to be able to edit our html safely after we have created it.
Also we want support for attributes.

#. Change the ``Html`` datatype such that [#new_html_type]_

   #. It can either be a text node which contains only a ``String``
   #. It is a container node (such as ``div``) which contains a string for the ``containerTag``, a list of attribute/value pairs ``containerAttributes`` [#pairs]_ and a list of ``containerChildren`` [#children]_
      Use a record here with the mentioned field names.

#. Rewrite the ``render`` function to use the new type, and also render the attributes. [#new_rendering]_

#. Rewrite the ``mkDiv`` etc. functions to create the new type. [#partial_application]_


Doing some inspection
---------------------

Now that we have this fancier Html tree we can do interesting things.
Implement the following queries as functions (they all return ``Bool``).

* Is a supplied ``Html`` value a text node 
* Does the node have a specific tag (hint: the type signature should be ``:: String -> Html -> Bool``)
* How many attributes does the node have? (assuming no attribute occurs twice in the attribute list) [#num_attrs]_
* Does the node have a specific attribute (hint: the type signature should be ``:: String -> Html -> Bool``) [#finding]_

Escaping (advanced)
-------------------

Change the text node creation so it doesn't fail when illegal characters are found but instead replaces them with the xml escape sequences.
The important thing to keep in mind here is that you need to replace single characters by strings of characters. [#replacing]_


========= =========
Character Escape
========= =========
``&``     ``&amp;``
--------- ---------
``<``     ``&lt;``
--------- ---------
``>``     ``&gt;``
========= =========

.. rubric:: footnotes

.. [#defining_html] You can use a ``data`` declaration, however since we only have one field in it you should use a ``newtype``.

.. [#exposing] Use the export list in your module to only export the type, not the constructor.

.. [#verifying] 
    Remember that the Haskell ``String`` type is just a list of characters.
    Look at the ``Data.List`` module in the ``base`` library documentation and find the function that allows you to test whether a certain character is in the string.
    (Hint: its the same function that tests whether a certain value is an *element* of the list.)

.. [#concatenating] 
    You'll have to unwrap the input ``Html`` values to get acces to the strings within.
    Look for an operator in ``Data.List`` which appends two lists together.
    You can use this operator to combine the strings as well.
    Finally wrap it all back up into a new ``Html`` value.

.. [#containers] 
    You'll again have to unwrap the ``Html``, prepend the start tag and append the end tag to it.
    Finally wrap it all back up into a new ``Html`` value

.. [#pairs] Pairs are the same a tuples. Both attribute and its value should be of type ``String``.

.. [#children] Children are again ``Html`` values.

.. [#new_html_type] 
    You can implement the different types of html by making it an algebraic datatype (``data``) with one constructor for the text node and one for the container node.
    Use record syntax for the latter.

.. [#new_rendering] 
    Some things that may come in handy here is the ``map`` function and the ``concat`` function.
    The first can be used (with an appropriate function) to transform for instance the list of ``Html`` children into a list of ``String``.
    The latter can be used to concatenate a list of ``String`` into a single ``String``.
    
    Haskell supports calling functions recursively.
    Meaning you can for instance call ``render`` from within ``render`` to render a nested ``Html`` value.

.. [#partial_application] This can be nicely done using a partially applied ``Container`` constructor.

.. [#num_attrs] This is the same as the length of the attribute list.

.. [#finding] 
    To see if an element of a list satisfies a predicate there are two ways.
    Either using ``map`` and ``any`` or using ``find``.
    I leave you to find out how to use these ;)

.. [#replacing] I'd recommend either to use ``concatMap`` or ``foldr``.
