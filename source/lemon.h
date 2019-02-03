/*
**  The author disclaims copyright to this source code.  In place of
**  a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, not taking more than you give.
**
**  lime.h
*/

#ifndef __LIME_H__
#define __LIME_H__

typedef int boolean_t;
extern boolean_t lemon_show_conflict;
extern boolean_t lemon_show_version;
extern boolean_t lemon_basis_flag;
extern boolean_t lemon_compress;
//extern boolean_t lemon_be_quiet;
extern boolean_t lemon_statistics;
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


/*
** a few forward declarations...
*/
//struct rule;
//struct lemon;
//struct action;

/********** From the file "struct.h" *************************************/
/*
** Principal data structures for the LEMON parser generator.
*/

typedef enum {LEMON_FALSE=0, LEMON_TRUE} Boolean;


/* Symbols (terminals and nonterminals) of the grammar are stored
** in the following: */
enum symbol_type {
  TERMINAL,
  NONTERMINAL,
  MULTITERMINAL
};

enum e_assoc {
    LEFT,
    RIGHT,
    NONE,
    UNK
};


struct symbol {
  const char *name;        /* Name of the symbol */
  int index;               /* Index number for this symbol */
  enum symbol_type type;   /* Symbols are all either TERMINALS or NTs */
  struct rule *rule;       /* Linked list of rules of this (if an NT) */
  struct symbol *fallback; /* fallback token in case this token doesn't parse */
  int prec;                /* Precedence if defined (-1 otherwise) */
  enum e_assoc assoc;      /* Associativity if precedence is defined */
  char *firstset;          /* First-set for all rules of this symbol */
  Boolean lambda;          /* True if NT and can generate an empty string */
  int useCnt;              /* Number of times used */
  char *destructor;        /* Code which executes whenever this symbol is
                           ** popped from the stack during error processing */
  int destLineno;          /* Line number for start of destructor.  Set to
                           ** -1 for duplicate destructors. */
  char *datatype;          /* The data type of information held by this
                           ** object. Only used if type==NONTERMINAL */
  int dtnum;               /* The data type number.  In the parser, the value
                           ** stack is a union.  The .yy%d element of this
                           ** union is the correct data type for this object */
  int bContent;            /* True if this symbol ever carries content - if
                           ** it is ever more than just syntax */
  /* The following fields are used by MULTITERMINALs only */
  int nsubsym;             /* Number of constituent symbols in the MULTI */
  struct symbol **subsym;  /* Array of constituent symbols */
};


/* Each production rule in the grammar is stored in the following
** structure.  */
struct rule {
  struct symbol *lhs;      /* Left-hand side of the rule */
  const char *lhsalias;    /* Alias for the LHS (NULL if none) */
  int lhsStart;            /* True if left-hand side is the start symbol */
  int ruleline;            /* Line number for the rule */
  int nrhs;                /* Number of RHS symbols */
  struct symbol **rhs;     /* The RHS symbols */
  const char **rhsalias;   /* An alias for each RHS symbol (NULL if none) */
  int line;                /* Line number at which code begins */
  const char *code;        /* The code executed when this rule is reduced */
  const char *codePrefix;  /* Setup code before code[] above */
  const char *codeSuffix;  /* Breakdown code after code[] above */
  int noCode;              /* True if this rule has no associated C code */
  int codeEmitted;         /* True if the code has been emitted already */
  struct symbol *precsym;  /* Precedence symbol for this rule */
  int index;               /* An index number for this rule */
  int iRule;               /* Rule number as used in the generated tables */
  Boolean canReduce;       /* True if this rule is ever reduced */
  Boolean doesReduce;      /* Reduce actions occur after optimization */
  struct rule *nextlhs;    /* Next rule with the same LHS */
  struct rule *next;       /* Next rule in the global list */
};


/* A configuration is a production rule of the grammar together with
** a mark (dot) showing how much of that rule has been processed so far.
** Configurations also contain a follow-set which is a list of terminal
** symbols which are allowed to immediately follow the end of the rule.
** Every configuration is recorded as an instance of the following: */
enum cfgstatus {
  COMPLETE,
  INCOMPLETE
};
struct config {
  struct rule *rp;         /* The rule upon which the configuration is based */
  int dot;                 /* The parse point */
  char *fws;               /* Follow-set for this configuration only */
  struct plink *fplp;      /* Follow-set forward propagation links */
  struct plink *bplp;      /* Follow-set backwards propagation links */
  struct state *stp;       /* Pointer to state which contains this */
  enum cfgstatus status;   /* used during followset and shift computations */
  struct config *next;     /* Next configuration in the state */
  struct config *bp;       /* The next basis configuration */
};

enum e_action {
  SHIFT,
  ACCEPT,
  REDUCE,
  ERROR,
  SSCONFLICT,              /* A shift/shift conflict */
  SRCONFLICT,              /* Was a reduce, but part of a conflict */
  RRCONFLICT,              /* Was a reduce, but part of a conflict */
  SH_RESOLVED,             /* Was a shift.  Precedence resolved conflict */
  RD_RESOLVED,             /* Was reduce.  Precedence resolved conflict */
  NOT_USED,                /* Deleted by compression */
  SHIFTREDUCE              /* Shift first, then reduce */
};


/* Every shift or reduce operation is stored as one of the following */
struct action {
  struct symbol *sp;       /* The look-ahead symbol */
  enum e_action type;
  union {
    struct state *stp;     /* The new state, if a shift */
    struct rule *rp;       /* The rule, if a reduce */
  } x;
  struct symbol *spOpt;    /* SHIFTREDUCE optimization to this symbol */
  struct action *next;     /* Next action for this state */
  struct action *collide;  /* Next action with the same hash */
};


/* Each state of the generated parser's finite state machine
** is encoded as an instance of the following structure. */
struct state {
  struct config *bp;       /* The basis configurations for this state */
  struct config *cfp;      /* All configurations in this set */
  int statenum;            /* Sequential number for this state */
  struct action *ap;       /* List of actions for this state */
  int nTknAct, nNtAct;     /* Number of actions on terminals and nonterminals */
  int iTknOfst, iNtOfst;   /* yy_action[] offset for terminals and nonterms */
  int iDfltReduce;         /* Default action is to REDUCE by this rule */
  struct rule *pDfltReduce;/* The default REDUCE rule. */
  int autoReduce;          /* True if this is an auto-reduce state */
};


/* A followset propagation link indicates that the contents of one
** configuration followset should be propagated to another whenever
** the first changes. */
struct plink {
  struct config *cfp;      /* The configuration to which linked */
  struct plink *next;      /* The next propagate link */
};
/* The state vector for the entire parser generator is recorded as
** follows.  (LEMON uses no global variables and makes little use of
** static variables.  Fields in the following structure can be thought
** of as begin global variables in the program.) */
struct lemon {
  struct state **sorted;   /* Table of states sorted by state number */
  struct rule *rule;       /* List of all rules */
  struct rule *startRule;  /* First rule */
  int nstate;              /* Number of states */
  int nxstate;             /* nstate with tail degenerate states removed */
  int nrule;               /* Number of rules */
  int nsymbol;             /* Number of terminal and nonterminal symbols */
  int nterminal;           /* Number of terminal symbols */
  int minShiftReduce;      /* Minimum shift-reduce action value */
  int errAction;           /* Error action value */
  int accAction;           /* Accept action value */
  int noAction;            /* No-op action value */
  int minReduce;           /* Minimum reduce action */
  int maxAction;           /* Maximum action value of any kind */
  struct symbol **symbols; /* Sorted array of pointers to symbols */
  int errorcnt;            /* Number of errors */
  struct symbol *errsym;   /* The error symbol */
  struct symbol *wildcard; /* Token that matches anything */
  char *name;              /* Name of the generated parser */
  char *arg;               /* Declaration of the 3th argument to parser */
  char *ctx;               /* Declaration of 2nd argument to constructor */
  char *tokentype;         /* Type of terminal symbols in the parser stack */
  char *vartype;           /* The default type of non-terminal symbols */
  char *start;             /* Name of the start symbol for the grammar */
  char *stacksize;         /* Size of the parser stack */
  char *include;           /* Code to put at the start of the C file */
  char *error;             /* Code to execute when an error is seen */
  char *overflow;          /* Code to execute on a stack overflow */
  char *failure;           /* Code to execute on parser failure */
  char *accept;            /* Code to execute when the parser excepts */
  char *extracode;         /* Code appended to the generated file */
  char *tokendest;         /* Code to execute to destroy token data */
  char *vardest;           /* Code for the default non-terminal destructor */
  char *filename;          /* Name of the input file */
  //  char *outname;           /* Name of the current output file */
  char *tokenprefix;       /* A prefix added to token names in the .h file */
  int nconflict;           /* Number of parsing conflicts */
  int nactiontab;          /* Number of entries in the yy_action[] table */
  int nlookaheadtab;       /* Number of entries in yy_lookahead[] */
  int tablesize;           /* Total table size of all tables in bytes */
  int basisflag;           /* Print only basis configurations */
  int has_fallback;        /* True if any %fallback is seen in the grammar */
  int nolinenosflag;       /* True if #line statements should not be printed */
  char *argv0;             /* Name of the program */
};

/**********************************************************************/
//#define NO_OFFSET (-2147483647)
const long NO_OFFSET = 2147483647;

/********** From the file "report.h" *************************************/
void lemon_reprint(struct lemon *);
void lemon_report_output(struct lemon *);
void lemon_report_table(struct lemon *);
void lemon_report_header(struct lemon *);
void lemon_compress_tables(struct lemon *);
void lemon_resort_states(struct lemon *);

/********** From the file "build.h" ************************************/
void lemon_find_rule_precedences(struct lemon*);
void lemon_find_first_sets(struct lemon*);
void lemon_find_states(struct lemon*);
void lemon_find_links(struct lemon*);
void lemon_find_follow_sets(struct lemon*);
void lemon_find_actions(struct lemon*);


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
lime_implementation_open (const char *file_name);
//  Open a file for writing then implementaion (parse.adb/parse.c).
//  File handler is located in the context structure.

void
lime_template_transfer (const char  *name);
//
//

void
lime_print (const char  *out_name,
            int         no_line_nos,
            const char  *include);
//
//
//

void
lime_template_linedir (int         *line_number,
                       const char  *file_name);
//
//

void
lime_write_include (const char  *include_name);
//
//

void
lime_generate_tokens (const char  *tokenprefix,
                      int          first,
                      int          last);
//
//

const char *lime_get_token_callback (int  index);
//
//

void 
lime_generate_spec (const char  *file_name,
                    const char  *prefix,
                    const char  *module,
                    int          first,
                    int          last);
//  Generate spec file (parse.h for parse.y).


void
lime_generate_the_defines_1 (const char     *yycodetype,
                             int             nsymbol,
                             const char     *yyactiontype,
                             int             wildcard,
                             int             wildcard_index);
//
//

void
lime_generate_the_defines_2 (const char  *stack_size);
//
//

struct mystruct
{
  int  use_count;
  int  index;
  int  dt_num;
};

void
lime_error_fallback (const char       *errsym,
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

void lime_write_action_table (int   N,
                              int   No_Action);
//
//

void
lime_write_yy_lookahead (int   n,
                         int   nsymbol);
//
//

void
lime_write_yy_shift_offsets (int          N,
                             int          MnTknOfst,
                             int          MxTknOfst,
                             const char  *Min_Size_Type,
                             int          Nactiontab,
                             int          NO_OFFSET);
//
//

void
lime_write_yy_reduce_offsets (int          N,
                              int          MnNtOfst,
                              int          MxNtOfst,
                              const char  *Min_Size_Type,
                              int          NO_OFFSET);
//
//

void
lime_write_default_action_table (int N,
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

void
lime_report_header
(const char  *token_prefix,
 const char  *base_name,
 const char  *module_name,
 int          terminal_last);
//
//

void lime_generate_reprint_of_grammar (void);
//
//

void
lime_set_out_name
(const char  *name);
//
//

const char*
lime_get_out_name
(void);
//
//

void
lime_power_on_self_test (void);
//
//

struct rule*
lime_rule_sort (struct rule*);
//
//

/*
struct symbol*
symbols_symbol_new (const char*);
struct symbol*
symbols_symbol_find (const char*);
*/

#endif

