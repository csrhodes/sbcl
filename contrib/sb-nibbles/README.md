The intent here is to provide implementations of nibbles-like
functions, that nibbles (or other libraries) can trampoline or convert
to, whether using compiler macros, deftransforms or other mechanisms.
These implementations are specialized to simple octet vectors, and it
is the library code's responsibility to extract the simple octet
vector from possibly-non-simple arguments.
