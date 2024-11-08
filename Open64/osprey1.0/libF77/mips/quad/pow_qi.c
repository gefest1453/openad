/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 *
 * Description:  source code for pow_qi and pow_ql functions
 *
 * ====================================================================
 * ====================================================================
 */

#include "quad.h"

/* ====================================================================
 *
 * FunctionName    pow_qi
 *
 * Description    computes x**n for long double x and int n
 *
 * ====================================================================
 */

/* by value only */

long double __pow_qi(double ahi, double alo, int n)
{
  ldquad  x;
  long double  result;

  x.q.hi = ahi;
  x.q.lo = alo;
  result = __qpow(x.ld, (long double)n);
  return (result);
}

/* ====================================================================
 *
 * FunctionName    pow_ql
 *
 * Description    computes x**n for long double x and long long n
 *
 * ====================================================================
 */
/* by value only */

long double __pow_ql(double ahi, double alo, long long n)
{
  ldquad  x;
  long double  result;

  x.q.hi = ahi;
  x.q.lo = alo;
  result = __qpow(x.ld, (long double)n);
  return (result);
}
