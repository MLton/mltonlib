Basic Utilities Library
-----------------------

   This library implements basic utilities for SML programming that one
   could reasonably consider to be a part of the language.


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
      (export.sml).  The contents of this directory should be sufficient
      to understand what is provided by this library.

   basic.{cm,mlb,use}

      These build files define the library.  See the build files for
      further instructions.

   detail/

      This directory contains the implementation details of the library.


Contributions
-------------

   The signatures and structures defined by this library are not meant to
   be cast in stone!  We welcome contributions including new utilities,
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
