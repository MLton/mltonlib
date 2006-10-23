Extended Basis Library
----------------------

   This library implements a number of minor extensions to the signatures
   and structures of the Standard ML Basis Library [1].  These extensions
   are done in a non-intrusive manner by simply rebinding the signatures
   and structures of the basis library.  The reason for extending the
   basis library in this way is that the extensions are naturally
   associated with specific basis library modules.  Extensions include
   things like isomorphisms and embeddings (pairs of the form (toX,
   fromX)), bounds (pairs of the form (minX, maxX)), and simple utility
   functions such as isZero, isEven and isOdd.


Info
----

   License:         MLton-LICENSE
   Portability:     portable
   Stability:       experimental
   Maintainer:      Vesa Karvonen <vesa.karvonen@cs.helsinki.fi>


About Library Organization
--------------------------

   public/

      This directory contains the documented signature definitions (*.sig)
      and listings of all top-level bindings exported by this library
      (export*.sml).  The contents of this directory should be sufficient
      to understand the extensions provided by this library.

   basis.mlb

      This basis file defines the extended basis library including all of
      the original basis library.  The idea is that users refer to this
      instead of the original basis library.

   extensions.mlb

      This basis file defines only the extensions provided by this
      library.  Users may refer to this basis file, but this is probably
      more interesting to maintainers.  You can, for example, run the
      command

         mlton -stop tc -show-basis extensions.basis extensions.mlb

      and inspect the generated file (extensions.basis) to get a precise
      and accurate picture of the extensions provided by this library.

   detail/

      This directory contains the implementation details of the library.

   detail/<COMPILER>/

      These directories (e.g. detail/mlton/) contain compiler specific
      implementation details.  Different compilers implement different
      subsets of the original basis library.


About Motivation and Scope
--------------------------

   The basis library, while certainly not perfect, is a valuable library
   and it doesn't make sense to throw it away.  There is a book describing
   the basis library and people just learning SML are likely to spend time
   learning the basis library.  It makes sense to build on that knowledge.

   However, maintaining 100% basis library compatibility is unlikely to
   lead to an "optimal" design.  In particular, here is what the basis
   library book [1] says (page 11, start of section 2, emphasis added):

      "We view the signature and structure names used below as being
      *reserved*.  For an implementation to be conforming, any module it
      provides that is named in the SML Basis Library must *exactly* match
      the description specified in the Library."

   So, the design of the basis library is supposed to be more or less cast
   in stone - at least if you want to claim that you've implemented the
   SML Basis Library.  However, one can argue that the basis library
   contains an organizational framework that goes beyond the exact
   signatures and structures specified.  For many simple extensions there
   is a place in that organizational framework, and while it isn't
   technically necessary to extended the basis library, it makes sense to
   do so because it can reduce the learning curve and make the entirety
   easier to use.

   On the other hand, it probably doesn't make sense to put everything
   into this library.  As a rule of thumb, things that naturally belong
   (fuzzy, yes) to specific basis library modules and what those things
   depend on should go into this library.  Everything else, even if looks
   like stuff that could be in a basis library, but there is no module in
   *the* basis library for it, should go into other libraries.


Contributions
-------------

   The signatures and structures defined by this library are not meant to
   be cast in stone!  We welcome contributions including new extensions,
   bugfixes, and ports to new compilers.  The recommended submit method
   for small contributions to this library is to send a message with a
   brief description of the proposed contribution as well as a patch
   containing full code and documentation (signature comments) to either
   the MLton-user list

      mlton-user@mlton.org

   or the MLton list

      mlton@mlton.org .

   For larger extensions or changes we recommend that you first contact
   the active maintainer(s) of this library.  The preferred contact method
   is through the above mailing lists.


References
----------

  [1] The Standard ML Basis Library.
      Emden R. Gansner and John H. Reppy.
      Cambridge University Press, 2004.
      ISBN 0521794781.
