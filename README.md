# List-Tries with HashMap as the supporting structure

This module connects the strict `HashMap` type from the `unordered-containers` package with the `TrieMap` type from the `list-tries` package.
The goal is to enable fast prefix lookup on lists of stringy types, for instance `[Text]` or `[ByteString]`.

## Notes

The tests were copied from the list-tries package (by Matti Niemenmaa) and edited to apply to `HashMap`-supported `TrieMap`s.

The strictness for `mapInKey'` is disabled until I figure out how to fix it.


