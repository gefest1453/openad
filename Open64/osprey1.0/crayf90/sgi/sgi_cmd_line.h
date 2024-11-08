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
 *  09-26-95 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef SGI_CMD_LINE_INCLUDED
#define SGI_CMD_LINE_INCLUDED

#ifdef _KEEP_RCS_ID
#endif /* _KEEP_RCS_ID */

extern char *FE_command_line;
extern char **save_argv;
extern INT8 Debug_Level;
extern INT save_argc;

extern BOOL IO_Comments;
extern BOOL Use_Three_Call;
extern BOOL FE_Full_Split_Set;
extern BOOL FE_Full_Split;
extern BOOL FE_Endloop_Marker;
extern BOOL Full_arrayexp;
extern mUINT16  FE_align ;

extern char *rii_file_name;
extern BOOL enable_dsm_recompile;
extern BOOL enable_dsm_processing;

extern char *FE_gdar_filename;

extern BOOL  global_chunk_pragma_set;   /* From the -chunk= command line option */
extern INT32 global_chunk_pragma_value;

extern INT32 global_schedtype_pragma_val; /* From the -mp_schedtype command line option */
extern BOOL global_schedtype_pragma_set;
extern BOOL cleanUpWhirl; 

extern BOOL process_cri_mp_pragmas;

extern BOOL FE_Call_Never_Return;

extern void add_cray_args ( char * );
extern void Cray_Woff ( char * );

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern void  sgi_cmd_line(int *argc, char **argv[]);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* SGI_CMD_LINE_INCLUDED */

