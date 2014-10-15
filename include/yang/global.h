//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_INCLUDE_YANG_GLOBAL_H
#define YANG_INCLUDE_YANG_GLOBAL_H

#include "type.h"

/** #sumline ## */
namespace yang {

/** #class */
struct Global {
  /** #member */
  Global(const Type& type, bool is_const)
    : type(type), is_const(is_const) {}
  /** #member */
  Type type;
  /** #member */
  bool is_const;
/** #summary */
};

} // ::yang
/**/

#endif
