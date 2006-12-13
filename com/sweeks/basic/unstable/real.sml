(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure LargeReal = Real (structure Real = LargeReal)
structure Real = PackableReal (structure PackBig = PackRealBig
                               structure PackLittle = PackRealLittle
                               structure Real = Real)
structure Real32 = PackableReal (structure PackBig = PackReal32Big
                                 structure PackLittle = PackReal32Little
                                 structure Real = Real32)
structure Real64 = PackableReal (structure PackBig = PackReal64Big
                                 structure PackLittle = PackReal64Little
                                 structure Real = Real64)
