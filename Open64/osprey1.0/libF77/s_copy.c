/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* $Header: /m_home/m_utkej/Argonne/cvs2svn/cvs/Open64/osprey1.0/libF77/s_copy.c,v 1.1.1.1 2002-05-22 20:09:13 dsystem Exp $ */

/* Craig Hansen - 24-Sept-86 */

#include <string.h>
#include <cmplrs/host.h>
#include "b_pad.h"

void
s_copy (string a, string b, fsize_t la, fsize_t lb)	/* assign strings:  a = b */
{
    register fsize_t	ld;
    register string	d;

    ld = la - lb;
    if (ld <= 0) {
	memcpy (a, b, la);
    } else {
	d = a + lb;
	memcpy (a, b, lb);
	b_pad (d, ld);
    }
}
