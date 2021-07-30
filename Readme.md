This is a tool to transpile a C header file (with includes) to a corresponding [Oberon+](https://github.com/rochus-keller/Oberon) definition module.

It already works well with the SDL2 headers (with only one modification), but consider the tool work-in-progress.

Note that since it is undecidable, whether a C pointer is indeed a pointer to the given object or in fact a pointer to an array of such objects,
and Oberon+ only supports pointers to structured types, all *basic_type are converted to CPOINTER TO CARRAY of basic_type (i.e. *[]),
but *structured_type are translated to Oberon+ CPOINTER TO structured_type (i.e. *). The special case of **type which is used to get a pointer
out of a procedure is handled by *[]*type for structured types and void, and *[][] for basic types.

Pending:

- [ ] process comments (line and block)
- [ ] consider #DEFINE constants
- [ ] use original type names for basic types (instead of plain C basic types)
