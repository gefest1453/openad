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
 *  07-Nov-94 - Original Version
 *
 * Description:
 *
 *    See tcon2c.h for a description of the exported functions and 
 *    variables.
 *
 * ====================================================================
 * ====================================================================
 */
#ifdef _KEEP_RCS_ID
#endif /* _KEEP_RCS_ID */

#include "whirl2c_common.h"
#include "tcon2c.h"
#include "alloca.h"

/*---------------------- Hidden utilities ---------------------*/
/*-------------------------------------------------------------*/
  
static char *
Remove_Trailing_Zero_Fraction(char *strbase)
{
   /* Expect the input to be of the form: "d.dddde+dd", where a '-' may 
    * occur in place of the '+', or the '+' could be omitted.  We view the
    * 'e' as any letter.
    */
   INT last, i;

   /* Get to the first digit from the right, which is non-zero.
    */
   for (last = 0; strbase[last] != '\0'; last++);
   for (i = last-1; strbase[i] == '0'; i--);

   /* Remove any unnecesary exponent part and the trailing zeros in the
    * fractional part.
    */
   if (strbase[i] < '0' || strbase[i] > '9')
   {
      while (strbase[i] < '0' || strbase[i] > '9') i--;
      while (strbase[i] == '0') i--;
      if (strbase[i] == '.')
      {
	 strbase[i+1] = '0';
	 last = i+2;
      }
      else
      {
	 last = i+1;
      }
   }
   else
   {
      INT j, remove_to;

      while (strbase[i] >= '0' && strbase[i] <= '9') i--; /* skip exp digits */
      while (strbase[i] < '0' || strbase[i] > '9') i--; /* skip exp letters */
      remove_to = i;

      while (strbase[i] == '0') i--; /* skip zero digits in the fraction */
      if (strbase[i] == '.')
	 i += 1;

      /* Move exponent part up till just after the non-zero fractional part
       */
      for (j = remove_to+1; j < last; j++)
	 strbase[++i] = strbase[j];
      last = i+1;
   }
   strbase[last] = '\0';

   return strbase;
} /* Remove_Trailing_Zero_Fraction */


static char *
TCON2C_append_string_char(char *str, char ch)
{
  BOOL escape;
  char escaped_ch;
  
  switch (ch)
  {
  case '\n':
     escaped_ch = 'n';
     escape = TRUE;
     break;
  case '\0':
     escaped_ch = '0';
     escape = TRUE;
     break;
  case '\t':
     escaped_ch = 't';
     escape = TRUE;
     break;
  case '\b':
     escaped_ch = 'b';
     escape = TRUE;
     break;
  case '\r':
     escaped_ch = 'r';
     escape = TRUE;
     break;
  case '\f':
     escaped_ch = 'f';
     escape = TRUE;
     break;
  case '\v':
     escaped_ch = 'v';
     escape = TRUE;
     break;
  case '\\':
     escaped_ch = '\\';
     escape = TRUE;
     break;
  case '\'':
     escaped_ch = '\'';
     escape = TRUE;
     break;
  case '\"':
     escaped_ch = '\"';
     escape = TRUE;
     break;
  default: 
     escaped_ch = ch;
     escape = FALSE;
     break;
  }
  if (escape)
     *str++ = '\\';
  *str++ = escaped_ch;

  return str;
} /* TCON2C_append_string_char */


void 
TCON2C_Append_String_Const(TOKEN_BUFFER tokens, 
			   const char  *orig_str, 
			   INT32        strlen)
{
   const char *str_base;
   char       *str;
   INT32       stridx;

   str_base = str = (char *)alloca(2*strlen + 3); /* "'", orig_str, "'", and "\0" */
   *(str++) = '\"';
   for (stridx = 0; stridx < strlen; stridx++)
      str = TCON2C_append_string_char(str, orig_str[stridx]);
//   while (str[-1] == '\0') str--;
   if (orig_str[strlen-1] == '\0'){
       str--;
       str--;
     }
   *(str++) = '\"';
   *(str++) = '\0';
   Append_Token_String(tokens, str_base);
} /* TCON2C_Append_String_Const */


/*---------------------- Exported functions -------------------*/
/*-------------------------------------------------------------*/

void 
TCON2C_initialize(void)
{
   return; /* do nothing for now */
} /* TCON2C_initialize */


void 
TCON2C_finalize(void)
{
   return; /* do nothing for now */
} /* TCON2C_finalize */


void 
TCON2C_translate(TOKEN_BUFFER tokens, TCON tvalue)
{
   char	       *strbase;
   char        *str;
   INT32        max_strlen, strlen, stridx;
   
   switch (TCON_ty(tvalue))
   {
   case MTYPE_STR:
      max_strlen = (Get_Maximum_Linelength()*2)/3;
      strlen = Targ_String_Length(tvalue);
      strbase = Targ_String_Address(tvalue);
      if (max_strlen > 0 && max_strlen < strlen)
      {
	 /* We need to split the string constant into substrings */
	 str = (char *)alloca(max_strlen + 1);
	 while (strlen > max_strlen)
	 {
	    for (stridx = 0; stridx < max_strlen; stridx++)
	       str[stridx] = strbase[stridx];
	    str[stridx] = '\0';
	    strbase = &strbase[stridx];
	    strlen -= max_strlen;
	    TCON2C_Append_String_Const(tokens, str, max_strlen);
	 }
      }
      TCON2C_Append_String_Const(tokens, strbase, strlen);
      break;

    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
      Append_Token_String(tokens, Targ_Print("%1d", tvalue));
      break;
      
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      Append_Token_String(tokens, Targ_Print("%1uU", tvalue));
      break;

    case MTYPE_I8:
      Append_Token_String(tokens, Targ_Print("%1lldLL", tvalue));
      break;

    case MTYPE_U8:
      Append_Token_String(tokens, Targ_Print("%1lluULL", tvalue));
      break;

    case MTYPE_F4:
      str = Targ_Print("%.10e", tvalue);
      strbase = Remove_Trailing_Zero_Fraction(str);
      /* Undo the 'e'->'d' conversion */
      if ((str = strchr(strbase, 'd')) != NULL)
	 *str = 'e';

      /* Add F suffix */
      Append_Token_String(tokens, Concat2_Strings(strbase, "F"));
      break;

    case MTYPE_F8:
      str = Targ_Print("%.20e", tvalue);
      strbase = Remove_Trailing_Zero_Fraction(str);
      /* Undo the 'e'->'d' conversion */
      if ((str = strchr(strbase, 'd')) != NULL)
	 *str = 'e';
      Append_Token_String(tokens, strbase);
      break;

    case MTYPE_FQ:
      str = Targ_Print(NULL, tvalue);
      strbase = Remove_Trailing_Zero_Fraction(str);
      /* Undo the 'e'->'d' conversion */
      if ((str = strchr(strbase, 'd')) != NULL)
	 *str = 'e'; 

      /* Add L suffix */
      Append_Token_String(tokens, Concat2_Strings(strbase, "L"));
      break;

    case MTYPE_C4:
    case MTYPE_C8:
    case MTYPE_CQ:
      Append_Token_Special(tokens, '{');
      TCON2C_translate(tokens, Extract_Complex_Real(tvalue));
      Append_Token_Special(tokens, ',');
      TCON2C_translate(tokens, Extract_Complex_Imag(tvalue));
      Append_Token_Special(tokens, '}');
      break;

   default:
      /* Only expression nodes should be handled here */
      ErrMsg (EC_Invalid_Case, "TCON2C_translate", __LINE__);
      Append_Token_String(tokens, "/*quad_constant*/");
      break;
   } /* switch */
} /* TCON2C_translate */
