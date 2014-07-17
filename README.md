TypesafeSubst
=============

This code implements typesafe substitution in Scala using the same insights as in the type-safe
interpreter from Burak et al. (2007).

To demonstrate how this substitution can be used, we build beta-reduction and normalization on top of it.

Caveat: Currently the generic traversal infrastructure is not statically type-safe, but that's a separate problem; in this context I could just write it by hand. Type-safe generic traversal is a separate problem, which I believe might be solved by shapeless through code generation via macros.

Bibliography
------------
Emir, B., Odersky, M., and Williams, J. Matching objects with patterns. In
ECOOP. Springer-Verlag, 2007, pp. 273–298.
