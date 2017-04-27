Modules
=======

Haskell source code is structured into units which we call modules.
When you type ``import Data.List`` you import the ``Data.List`` module and bring the definitions contained in that module into scope.

There belongs a source file to each module.
The name of the source file is the name of the module and ends with ``.hs``.
For instance the ``Prelude`` module would be in a file called ``Prelude.hs``.

Hierarchical modules, such as ``Data.List`` have a path matching the module name.
For instance ``Data.List`` would be ``List.hs`` in a directory called ``Data``.

A source file which is to be a Haskell module always starts with a module header.
The module header is the keyword ``module`` followed by the name of the module, and optional export list and the keyword ``where``.

::

    module Data.Bool (Bool(True, False), bool, not) where

This header proceeds any other code in the source file.

Exporting
---------

The export list, which follows the module name is a comma separated list of items to export.
These items may be types, cosntructors, functions, type aliases, type classes and record fields/accessors.
Only items which are exported in this list will be available when the module is imported.

Therefore the export list may be used to hide certain implementation detail from the importer.
If the export list is omitted all top level declarations will be exported.

For types we can export just the type alone ``Bool`` or we export any number of constructors ``Bool(False)``, ``Bool(True, False)``.

Importing 
---------

To use the exported items from a module one must *import* them.
The keyword for this is ``import``.
The import definitions follow the module header.

Similarly to the module definition an import declaration can have an optional import list.
Only items in the import list will be in scope.

::

    module MyModule where

    import Data.Bool (not, (||))

If the import list is omitted all exported items from the module will be imported.
