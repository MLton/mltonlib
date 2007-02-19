#ifndef PTR_INT_OBJ_H_20070218
#define PTR_INT_OBJ_H_20070218

/* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

union PtrIntObj {
  union PtrIntObj *next;
  int value;
};

#endif
