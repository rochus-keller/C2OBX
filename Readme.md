This is a tool to transpile a C header file (with includes) to a corresponding [Oberon+](https://github.com/rochus-keller/Oberon) definition module.

It already works well with the SDL2 headers (with only one modification), but consider the tool work-in-progress.

Note that char*/unsigned char* is converted to "carray of char/byte", but all other C pointer types are translated to Oberon+ cpointers, since it is undecidable, whether a C pointer is indeed a pointer to the given object or in fact a pointer to an array of such objects.

Pending:

- [ ] process comments (line and block)
- [ ] use original type names for basic types (instead of plain C basic types)
