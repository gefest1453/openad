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
 * Revision history:
 *  11-Nov-94 - Original Version
 *
 * Description:
 *
 * General support for data layout functionality in the back end.
 *
 * Also see:  stblock.h for base block handling.
 *
 * ====================================================================
 * ====================================================================
 */
#ifndef	data_layout_INCLUDED
#define	data_layout_INCLUDED

#include "stab.h"
#include "wn.h"
#include "targ_sim.h"

#ifdef __cplusplus
extern "C" {
#endif

extern ST *SP_Sym;		/* stack pointer */
extern ST *FP_Sym;		/* frame pointer */
extern ST *Local_Spill_Sym;	/* reserved close location for spill */

/* direction for the stack frame to grow */
typedef enum { INCREMENT, DECREMENT } STACK_DIR;

/* Choice of stack model to use: */
typedef enum {
  SMODEL_UNDEF = 0,	/* Not yet defined */
  SMODEL_SMALL = 1,	/* Small frame, no frame pointer */
  SMODEL_LARGE = 2,	/* Large frame, use frame pointer */
  SMODEL_DYNAMIC = 3	/* Dynamic allocation, use frame pointer */
} STACK_MODEL;

extern STACK_MODEL Current_PU_Stack_Model;    /* stack model of current pu */

extern STACK_DIR Stack_Direction(void);

/*
 * Initialize_Stack_Frame must be called once per PU,
 * before it is split into regions.
 * This will determine the stack model,
 * set up internal structures, and allocate all formals.
 */
extern void Initialize_Stack_Frame (WN *PU_tree);

// This is called after lowering, when have more accurate view of code.
extern void Calculate_Stack_Frame_Sizes (WN *PU_tree);

/*
 * Finalize_Stack_Frame should be called once per PU before cgemit;
 * it returns the stack frame size.
 */
extern INT64 Finalize_Stack_Frame(void);

/* 
 * Verify that stack space needed for actuals doesn't overflow
 * what we initially reserved. 
 */
extern void Check_Actual_Stack_Size (WN *call_tree);

/* allocate a temporary for the vararg formal */
extern ST *Allocate_Vararg_Formal (void);

extern BOOL Is_Allocated (ST *st);	/* has ST been allocated? */

/* Allocate the memory location for this object 'st' */
extern void Allocate_Object ( ST *st );

/* This function allows the code generator to allocate space for */
/* register spilled temp variable */
extern void Allocate_Temp_To_Memory ( ST *st );

/* Get upformal symbol for altentries that will be copied to temp space */
extern ST* Get_Altentry_UpFormal_Symbol (ST *formal, PLOC ploc);

/* reset the upformal segment when start an alternate entry */
extern void Reset_UPFORMAL_Segment(void );

/* get vararg symbol that corresponds to ploc value */
extern ST* Get_Vararg_Symbol (PLOC);

/* 
 * Allocate the memory location for all global/static symbols.
 */
extern void Allocate_File_Statics (void);

/*
 * Pad global, C arrays
 */
extern void Pad_Global_Arrays();

/* return GUESS of whether ST will use an offset < 16 bits */
extern BOOL Uses_Small_Offset (ST *st, WN_OFFSET wn_ofst);

extern INT32 Stack_Alignment (void);

/* Return true if the ST is defined on the stack */
extern BOOL ST_on_stack(ST *);

/* Return true if the ST is defined in the pu */
extern BOOL ST_pu_defined(ST *);

/* Look for a formal-ref-base corresponding to the sym. */
extern ST * Get_ST_formal_ref_base (const ST *sym);
extern void Set_ST_formal_ref_base (const ST *sym, ST *base);

/* return 0 if no formal-preg-num */
extern PREG_NUM Get_ST_formal_preg_num (const ST *base);
extern void Set_ST_formal_preg_num (const ST *base, PREG_NUM p);

inline BOOL ST_has_formal_preg_num (const ST *const st)
  {
    return (ST_sclass(st) == SCLASS_FORMAL &&
	    Get_ST_formal_preg_num(st) != 0);
  }

/* Map arg-area-size to the corresponding PU */
extern UINT32 Get_PU_arg_area_size(PU_IDX);
extern void Set_PU_arg_area_size(PU_IDX, UINT32 size);

/* size of actual build area for current PU */
extern INT32 Current_PU_Actual_Size;	

/* return ST for __return_address if one exists */
extern ST * Find_Special_Return_Address_Symbol (void);

/* return stack offset adjustment, which may depend on size of pu frame */
extern INT Stack_Offset_Adjustment_For_PU (void);

#ifdef __cplusplus
}
#endif
#endif /* data_layout_INCLUDED */

