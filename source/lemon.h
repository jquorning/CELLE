/*
**
**  lime.h
**
*/

#ifndef __LIME_H__
#define __LIME_H__

typedef int boolean_t;
extern boolean_t lemon_show_conflict;
extern boolean_t lemon_show_version;
extern boolean_t lemon_rp_flag;
extern boolean_t lemon_basis_flag;
extern boolean_t lemon_compress;
extern boolean_t lemon_be_quiet;
extern boolean_t lemon_statistics;
//extern boolean_t lemon_mh_flag;
extern boolean_t lemon_no_line_nos;
extern boolean_t lemon_no_resort;
extern boolean_t lemon_show_help;

typedef int language_t;
const language_t LANGUAGE_DEFAULT = 0;
const language_t LANGUAGE_ADA     = 1;
const language_t LANGUAGE_C       = 2;
const language_t LANGUAGE_CPP     = 3;

extern language_t lemon_language;

extern char *lemon_program_name;
extern char *lemon_input_file;
extern char *lemon_user_template;
extern char *lemon_output_dir;

char*
lime_get_user_template_name (void);
//  lempar.c for C and cherry_parser.adb for Ada.

void
lime_template_open
  (const char  *user_template, // User provided template. "" when none.
   int         *error_count,   // Incremented on error
   int         *success);      // Success = 0 when no template file is
//  Thisk function finds the template file and opens it. File handle
//  is located in the context structure.


void
lime_implementation_open
  (const char *file_name);
//  Open a file for writing then implementaion (parse.adb/parse.c).
//  File handler is located in the context structure.

void
lime_template_transfer
(const char  *name);
//
//

void
lime_print
  (const char  *out_name,
   int         no_line_nos,
   const char  *include);
//
//
//

void
lime_template_linedir
  (int         *line_number,
   const char  *file_name);
//
//

void
lime_write_include
//(int          mh_flag,
(const char  *include_name);
//
//

void
lime_generate_tokens
//  (int          mh_flag,
(const char  *tokenprefix,
 int          first,
 int          last);
//
//

const char *lime_get_token_callback
  (int  index);
//
//

void 
lime_generate_spec
   (const char  *file_name,
    const char  *prefix,
    const char  *module,
    int          first,
    int          last);
//  Generate spec file (parse.h for parse.y).


void
lime_generate_the_defines_1
  (const char     *yycodetype,
   int             nsymbol,
   const char     *yyactiontype,
   int             wildcard,
   int             wildcard_index);
//
//

void
lime_generate_the_defines_2
(const char  *stack_size);
//
//

struct mystruct
{
  int  use_count;
  int  index;
  int  dt_num;
};

void
lime_error_fallback
(
 const char       *errsym,
 struct mystruct  *my_struct,
 int               has_fallback);
//
//

struct lime_render_record
{
  int      Nxstate;
  int      nrule;
  int      nterminal;
  int      minShiftReduce;
  int      errAction;
  int      accAction;
  int      noAction;
  int      minReduce;
};

void
lime_render_constants
(struct lime_render_record  *render);
//
//

void lime_write_action_table
(int   N,
 int   No_Action);
//
//

void
lime_write_yy_lookahead
(int   n,
 int   nsymbol);
//
//

void
lime_write_yy_shift_offsets
(int          N,
 int          MnTknOfst,
 int          MxTknOfst,
 const char  *Min_Size_Type,
 int          Nactiontab,
 int          NO_OFFSET);
//
//

void
lime_write_yy_reduce_offsets
(int          N,
 int          MnNtOfst,
 int          MxNtOfst,
 const char  *Min_Size_Type,
 int          NO_OFFSET);
//
//

void
lime_write_default_action_table
(int N,
 int Error_Action,
 int Min_Reduce);
//
//

void
lime_put (const char* item);
//
//

void
lime_put_int (int item);
//
//

void
lime_put_line (const char* item);
//
//

void
lime_write_line_directive
(int         line_number,
 const char  *file_name);
//
//

void
lime_template_print
(const char  *line,
 int          no_line_nos,
 // int          line_number,
 const char  *out_name);
//
//

void
lime_write_arg_defines
(const char  *name,
 const char  *arg_ctx,
 int          extend,
 const char  *arg,
 const char  *arg_i);
//
//

void
lime_close_out (void);
//
//

void
lime_close_in (void);
//
//

//int
//lime_get_mh_flag (void);
//
//

void
lime_write_interface
(const char  *name,
 const char  *tokentype);
//
//

void lime_write_interface_begin (void);
void lime_write_interface_end   (void);
//
//

#endif
