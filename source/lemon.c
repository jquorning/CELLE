/*
**  The author disclaims copyright to this source code.  In place of
**  a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, not taking more than you give.
**
** This file contains all sources (including headers) to the LEMON
** LALR(1) parser generator.  The sources have been combined into a
** single file to make it easy to include LEMON in the source tree
** and Makefile of another program.
**
** The author of this program disclaims copyright.
*/
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#include "lemon.h"   /* Binding to C from Ada */
#include "cherry.h"
#include "actions.h"
#include "symbols.h"

#define ISSPACE(X) isspace((unsigned char)(X))
#define ISDIGIT(X) isdigit((unsigned char)(X))
#define ISALNUM(X) isalnum((unsigned char)(X))
#define ISALPHA(X) isalpha((unsigned char)(X))
#define ISUPPER(X) isupper((unsigned char)(X))
#define ISLOWER(X) islower((unsigned char)(X))


#ifndef __WIN32__
#   if defined(_WIN32) || defined(WIN32)
#       define __WIN32__
#   endif
#endif

#ifdef __WIN32__
#ifdef __cplusplus
extern "C" {
#endif
extern int access(const char *path, int mode);
#ifdef __cplusplus
}
#endif
#else
#include <unistd.h>
#endif

/* #define PRIVATE static */
#define PRIVATE

#ifdef TEST
#define MAXRHS 5       /* Set low to exercise exception code */
#else
#define MAXRHS 1000
#endif

static char *msort(char*,char**,int(*)(const char*,const char*));

/*
** Compilers are getting increasingly pedantic about type conversions
** as C evolves ever closer to Ada....  To work around the latest problems
** we have to define the following variant of strlen().
*/
#define lemonStrlen(X)   ((int)strlen(X))

/*
** Compilers are starting to complain about the use of sprintf() and strcpy(),
** saying they are unsafe.  So we define our own versions of those routines too.
**
** There are three routines here:  lemon_sprintf(), lemon_vsprintf(), and
** lemon_addtext(). The first two are replacements for sprintf() and vsprintf().
** The third is a helper routine for vsnprintf() that adds texts to the end of a
** buffer, making sure the buffer is always zero-terminated.
**
** The string formatter is a minimal subset of stdlib sprintf() supporting only
** a few simply conversions:
**
**   %d
**   %s
**   %.*s
**
*/
static void lemon_addtext(
  char *zBuf,           /* The buffer to which text is added */
  int *pnUsed,          /* Slots of the buffer used so far */
  const char *zIn,      /* Text to add */
  int nIn,              /* Bytes of text to add.  -1 to use strlen() */
  int iWidth            /* Field width.  Negative to left justify */
){
  if( nIn<0 ) for(nIn=0; zIn[nIn]; nIn++){}
  while( iWidth>nIn ){ zBuf[(*pnUsed)++] = ' '; iWidth--; }
  if( nIn==0 ) return;
  memcpy(&zBuf[*pnUsed], zIn, nIn);
  *pnUsed += nIn;
  while( (-iWidth)>nIn ){ zBuf[(*pnUsed)++] = ' '; iWidth++; }
  zBuf[*pnUsed] = 0;
}
static int lemon_vsprintf(char *str, const char *zFormat, va_list ap){
  int i, j, k, c;
  int nUsed = 0;
  const char *z;
  char zTemp[50];
  str[0] = 0;
  for(i=j=0; (c = zFormat[i])!=0; i++){
    if( c=='%' ){
      int iWidth = 0;
      lemon_addtext(str, &nUsed, &zFormat[j], i-j, 0);
      c = zFormat[++i];
      if( ISDIGIT(c) || (c=='-' && ISDIGIT(zFormat[i+1])) ){
        if( c=='-' ) i++;
        while( ISDIGIT(zFormat[i]) ) iWidth = iWidth*10 + zFormat[i++] - '0';
        if( c=='-' ) iWidth = -iWidth;
        c = zFormat[i];
      }
      if( c=='d' ){
        int v = va_arg(ap, int);
        if( v<0 ){
          lemon_addtext(str, &nUsed, "-", 1, iWidth);
          v = -v;
        }else if( v==0 ){
          lemon_addtext(str, &nUsed, "0", 1, iWidth);
        }
        k = 0;
        while( v>0 ){
          k++;
          zTemp[sizeof(zTemp)-k] = (v%10) + '0';
          v /= 10;
        }
        lemon_addtext(str, &nUsed, &zTemp[sizeof(zTemp)-k], k, iWidth);
      }else if( c=='s' ){
        z = va_arg(ap, const char*);
        lemon_addtext(str, &nUsed, z, -1, iWidth);
      }else if( c=='.' && memcmp(&zFormat[i], ".*s", 3)==0 ){
        i += 2;
        k = va_arg(ap, int);
        z = va_arg(ap, const char*);
        lemon_addtext(str, &nUsed, z, k, iWidth);
      }else if( c=='%' ){
        lemon_addtext(str, &nUsed, "%", 1, 0);
      }else{
        fprintf(stderr, "illegal format\n");
        exit(1);
      }
      j = i+1;
    }
  }
  lemon_addtext(str, &nUsed, &zFormat[j], i-j, 0);
  return nUsed;
}
static int lemon_sprintf(char *str, const char *format, ...){
  va_list ap;
  int rc;
  va_start(ap, format);
  rc = lemon_vsprintf(str, format, ap);
  va_end(ap);
  return rc;
}
static void lemon_strcpy(char *dest, const char *src){
  while( (*(dest++) = *(src++))!=0 ){}
}
static void lemon_strcat(char *dest, const char *src){
  while( *dest ) dest++;
  lemon_strcpy(dest, src);
}


/* a few forward declarations... */
struct rule;
struct lemon;
struct action;

static struct action *Action_new(void);
static struct action *Action_sort(struct action *);

/********* From the file "configlist.h" *********************************/
void lemon_configlist_init(void);
struct config *Configlist_add(struct rule *, int);
struct config *lemon_configlist_add_basis(struct rule *, int);
void Configlist_closure(struct lemon *);
void Configlist_sort(void);
void Configlist_sortbasis(void);
struct config *Configlist_return(void);
struct config *Configlist_basis(void);
void Configlist_eat(struct config *);
void Configlist_reset(void);

/********* From the file "error.h" ***************************************/
void ErrorMsg(const char *, int,const char *, ...);

/******** From the file "parse.h" *****************************************/
void Parse(struct lemon *lemp);

/********* From the file "plink.h" ***************************************/
struct plink *Plink_new(void);
void Plink_add(struct plink **, struct config *);
void Plink_copy(struct plink **, struct plink *);
void Plink_delete(struct plink *);

/********** From the file "set.h" ****************************************/
void  SetSize(int);             /* All sets will be of size N */
char *SetNew(void);               /* A new set for element 0..N */
void  SetFree(char*);             /* Deallocate a set */
int lemon_set_add(char*,int);            /* Add element to a set */
int SetUnion(char *,char *);    /* A <- A U B, thru element N */
#define SetFind(X,Y) (X[Y])       /* True if Y is in set X */

#define MemoryCheck(X) if((X)==0){ \
  extern void memory_error(); \
  memory_error(); \
}

/**************** From the file "table.h" *********************************/
/*
** All code in this file has been automatically generated
** from a specification in the file
**              "table.q"
** by the associative array code building program "aagen".
** Do not edit this file!  Instead, edit the specification
** file, then rerun aagen.
*/
/*
** Code for processing tables in the LEMON parser generator.
*/
/* Routines for handling a strings */

const char *Strsafe(const char *);

void Strsafe_init(void);
int Strsafe_insert(const char *);
const char *Strsafe_find(const char *);

/* Routines to manage the state table */

int Configcmp(const char *, const char *);
struct state *State_new(void);
void State_init(void);
int State_insert(struct state *, struct config *);
struct state *State_find(struct config *);
struct state **State_arrayof(void);

/* Routines used for efficiency in Configlist_add */

void Configtable_init(void);
int Configtable_insert(struct config *);
struct config *Configtable_find(struct config *);
void Configtable_clear(int(*)(struct config *));

/****************** Common data structure used as global **********************/

//struct lemon lem;

/*************** End of common data structure used as global ******************/

void
lime_partial_database_dump (struct lemon *lemp)
{
  printf ("LIME PARTIAL DATABASE DUMP\n");
  printf ("--------------------------\n");
  printf ("8*sizeof(lem): %ld\n", 8*sizeof (*lemp));
  printf ("nstate  : %ld\n", lemp->nstate);
  printf ("nxstate : %ld\n", lemp->nxstate);
  printf ("nrule   : %ld\n", lemp->nrule);
  printf ("nsymbol : %ld\n", lemp->nsymbol);
  printf ("nterminal : %ld\n", lemp->nterminal);
  printf ("minShiftReduce : %ld\n", lemp->minShiftReduce);
  printf ("errAction : %ld\n", lemp->errAction);
  printf ("accAction : %ld\n", lemp->accAction);
  printf ("noAction  : %ld\n", lemp->noAction);
  printf ("minReduce : %ld\n", lemp->minReduce);
  printf ("maxAction : %ld\n", lemp->maxAction);
  printf ("\n");
  printf ("start : %s\n", lemp->start);
  printf ("filename : %s\n", lemp->filename);
  printf ("\n");
  printf ("lemon_input_file : %s\n", lemon_input_file);
}

#if 0
void
lime_partial_database_dump_c (void)
{
  lime_partial_database_dump (&lem);
}
#endif

/****************** From the file "action.c" *******************************/
/*
** Routines processing parser actions in the LEMON parser generator.
*/

/* Allocate a new parser action */
static struct action *Action_new(void){
  static struct action *freelist = 0;
  struct action *newaction;

  if( freelist==0 ){
    int i;
    int amt = 100;
    freelist = (struct action *)calloc(amt, sizeof(struct action));
    if( freelist==0 ){
      fprintf(stderr,"Unable to allocate memory for a new parser action.");
      exit(1);
    }
    for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
    freelist[amt-1].next = 0;
  }
  newaction = freelist;
  freelist = freelist->next;
  return newaction;
}

#if 0
/* Compare two actions for sorting purposes.  Return negative, zero, or
** positive if the first action is less than, equal to, or greater than
** the first
*/
static int actioncmp(
  struct action *ap1,
  struct action *ap2
){
  int rc;
  rc = ap1->sp->index - ap2->sp->index;
  if( rc==0 ){
    rc = (int)ap1->type - (int)ap2->type;
  }
  if( rc==0 && (ap1->type==REDUCE || ap1->type==SHIFTREDUCE) ){
    rc = ap1->x.rp->index - ap2->x.rp->index;
  }
  if( rc==0 ){
    rc = (int) (ap2 - ap1);
  }
  return rc;
}
#endif

/* Sort parser actions */
static struct action *Action_sort(
  struct action *ap
){
  ap = (struct action *)msort((char *)ap,(char **)&ap->next,
                              (int(*)(const char*,const char*))actioncmp);
  return ap;
}

void Action_add(
  struct action **app,
  enum e_action type,
  struct symbol *sp,
  char *arg
){
  struct action *newaction;
  newaction = Action_new();
  newaction->next = *app;
  *app = newaction;
  newaction->type = type;
  newaction->sp = sp;
  newaction->spOpt = 0;
  if( type==SHIFT ){
    newaction->x.stp = (struct state *)arg;
  }else{
    newaction->x.rp = (struct rule *)arg;
  }
}

/********************** From the file "build.c" *****************************/
/*
** Routines to construction the finite state machine for the LEMON
** parser generator.
*/

/* Find a precedence symbol of every rule in the grammar.
**
** Those rules which have a precedence symbol coded in the input
** grammar using the "[symbol]" construct will already have the
** rp->precsym field filled.  Other rules take as their precedence
** symbol the first RHS symbol with a defined precedence.  If there
** are not RHS symbols with a defined precedence, the precedence
** symbol field is left blank.
*/
void lemon_find_rule_precedences(struct lemon *xp)
{
  struct rule *rp;
  for(rp=xp->rule; rp; rp=rp->next){
    if( rp->precsym==0 ){
      int i, j;
      for(i=0; i<rp->nrhs && rp->precsym==0; i++){
        struct symbol *sp = rp->rhs[i];
        if( sp->type==MULTITERMINAL ){
          for(j=0; j<sp->nsubsym; j++){
            if( sp->subsym[j]->prec>=0 ){
              rp->precsym = sp->subsym[j];
              break;
            }
          }
        }else if( sp->prec>=0 ){
          rp->precsym = rp->rhs[i];
        }
      }
    }
  }
  return;
}

/* Find all nonterminals which will generate the empty string.
** Then go back and compute the first sets of every nonterminal.
** The first set is the set of all terminal symbols which can begin
** a string generated by that nonterminal.
*/
void lemon_find_first_sets (struct lemon *lemp)
{
  int i, j;
  struct rule *rp;
  int progress;

  for(i=0; i<lemp->nsymbol; i++){
    lemp->symbols[i]->lambda = LEMON_FALSE;
  }
  for(i=lemp->nterminal; i<lemp->nsymbol; i++){
    lemp->symbols[i]->firstset = SetNew();
  }

  /* First compute all lambdas */
  do{
    progress = 0;
    for(rp=lemp->rule; rp; rp=rp->next){
      if( rp->lhs->lambda ) continue;
      for(i=0; i<rp->nrhs; i++){
        struct symbol *sp = rp->rhs[i];
        assert( sp->type==NONTERMINAL || sp->lambda==LEMON_FALSE );
        if( sp->lambda==LEMON_FALSE ) break;
      }
      if( i==rp->nrhs ){
        rp->lhs->lambda = LEMON_TRUE;
        progress = 1;
      }
    }
  }while( progress );

  /* Now compute all first sets */
  do{
    struct symbol *s1, *s2;
    progress = 0;
    for(rp=lemp->rule; rp; rp=rp->next){
      s1 = rp->lhs;
      for(i=0; i<rp->nrhs; i++){
        s2 = rp->rhs[i];
        if( s2->type==TERMINAL ){
          progress += lemon_set_add(s1->firstset,s2->index);
          break;
        }else if( s2->type==MULTITERMINAL ){
          for(j=0; j<s2->nsubsym; j++){
            progress += lemon_set_add(s1->firstset,s2->subsym[j]->index);
          }
          break;
        }else if( s1==s2 ){
          if( s1->lambda==LEMON_FALSE ) break;
        }else{
          progress += SetUnion(s1->firstset,s2->firstset);
          if( s2->lambda==LEMON_FALSE ) break;
        }
      }
    }
  }while( progress );
  return;
}

/* Compute all LR(0) states for the grammar.  Links
** are added to between some states so that the LR(1) follow sets
** can be computed later.
*/
PRIVATE struct state *lemon_get_state(struct lemon *);  /* forward reference */


/* Return a pointer to a state which is described by the configuration
** list which has been built from calls to Configlist_add.
*/
PRIVATE void buildshifts(struct lemon *, struct state *); /* Forwd ref */
PRIVATE struct state *lemon_get_state(struct lemon *lemp)
{
  struct config *cfp, *bp;
  struct state *stp;

  /* Extract the sorted basis of the new state.  The basis was constructed
  ** by prior calls to "Configlist_addbasis()". */
  Configlist_sortbasis();
  bp = Configlist_basis();

  /* Get a state with the same basis */
  stp = State_find(bp);
  if( stp ){
    /* A state with the same basis already exists!  Copy all the follow-set
    ** propagation links from the state under construction into the
    ** preexisting state, then return a pointer to the preexisting state */
    struct config *x, *y;
    for(x=bp, y=stp->bp; x && y; x=x->bp, y=y->bp){
      Plink_copy(&y->bplp,x->bplp);
      Plink_delete(x->fplp);
      x->fplp = x->bplp = 0;
    }
    cfp = Configlist_return();
    Configlist_eat(cfp);
  }else{
    /* This really is a new state.  Construct all the details */
    Configlist_closure(lemp);    /* Compute the configuration closure */
    Configlist_sort();           /* Sort the configuration closure */
    cfp = Configlist_return();   /* Get a pointer to the config list */
    stp = State_new();           /* A new state structure */
    MemoryCheck(stp);
    stp->bp = bp;                /* Remember the configuration basis */
    stp->cfp = cfp;              /* Remember the configuration closure */
    stp->statenum = lemp->nstate++; /* Every state gets a sequence number */
    stp->ap = 0;                 /* No actions, yet. */
    State_insert(stp,stp->bp);   /* Add to the state table */
    buildshifts(lemp,stp);       /* Recursively compute successor states */
  }
  return stp;
}

/*
** Return true if two symbols are the same.
*/
int same_symbol(struct symbol *a, struct symbol *b)
{
  int i;
  if( a==b ) return 1;
  if( a->type!=MULTITERMINAL ) return 0;
  if( b->type!=MULTITERMINAL ) return 0;
  if( a->nsubsym!=b->nsubsym ) return 0;
  for(i=0; i<a->nsubsym; i++){
    if( a->subsym[i]!=b->subsym[i] ) return 0;
  }
  return 1;
}

/* Construct all successor states to the given state.  A "successor"
** state is any state which can be reached by a shift action.
*/
PRIVATE void buildshifts(struct lemon *lemp, struct state *stp)
{
  struct config *cfp;  /* For looping thru the config closure of "stp" */
  struct config *bcfp; /* For the inner loop on config closure of "stp" */
  struct config *newcfg;  /* */
  struct symbol *sp;   /* Symbol following the dot in configuration "cfp" */
  struct symbol *bsp;  /* Symbol following the dot in configuration "bcfp" */
  struct state *newstp; /* A pointer to a successor state */

  /* Each configuration becomes complete after it contibutes to a successor
  ** state.  Initially, all configurations are incomplete */
  for(cfp=stp->cfp; cfp; cfp=cfp->next) cfp->status = INCOMPLETE;

  /* Loop through all configurations of the state "stp" */
  for(cfp=stp->cfp; cfp; cfp=cfp->next){
    if( cfp->status==COMPLETE ) continue;    /* Already used by inner loop */
    if( cfp->dot>=cfp->rp->nrhs ) continue;  /* Can't shift this config */
    Configlist_reset();                      /* Reset the new config set */
    sp = cfp->rp->rhs[cfp->dot];             /* Symbol after the dot */

    /* For every configuration in the state "stp" which has the symbol "sp"
    ** following its dot, add the same configuration to the basis set under
    ** construction but with the dot shifted one symbol to the right. */
    for(bcfp=cfp; bcfp; bcfp=bcfp->next){
      if( bcfp->status==COMPLETE ) continue;    /* Already used */
      if( bcfp->dot>=bcfp->rp->nrhs ) continue; /* Can't shift this one */
      bsp = bcfp->rp->rhs[bcfp->dot];           /* Get symbol after dot */
      if( !same_symbol(bsp,sp) ) continue;      /* Must be same as for "cfp" */
      bcfp->status = COMPLETE;                  /* Mark this config as used */
      newcfg = lemon_configlist_add_basis(bcfp->rp,bcfp->dot+1);
      Plink_add(&newcfg->bplp,bcfp);
    }

    /* Get a pointer to the state described by the basis configuration set
    ** constructed in the preceding loop */
    newstp = lemon_get_state(lemp);

    /* The state "newstp" is reached from the state "stp" by a shift action
    ** on the symbol "sp" */
    if( sp->type==MULTITERMINAL ){
      int i;
      for(i=0; i<sp->nsubsym; i++){
        Action_add(&stp->ap,SHIFT,sp->subsym[i],(char*)newstp);
      }
    }else{
      Action_add(&stp->ap,SHIFT,sp,(char *)newstp);
    }
  }
}

/*
** Construct the propagation links
*/
void lemon_find_links (struct lemon *lemp)
{
  int i;
  struct config *cfp, *other;
  struct state *stp;
  struct plink *plp;

  /* Housekeeping detail:
  ** Add to every propagate link a pointer back to the state to
  ** which the link is attached. */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){
      cfp->stp = stp;
    }
  }

  /* Convert all backlinks into forward links.  Only the forward
  ** links are used in the follow-set computation. */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){
      for(plp=cfp->bplp; plp; plp=plp->next){
        other = plp->cfp;
        Plink_add(&other->fplp,cfp);
      }
    }
  }
}

/* Compute all followsets.
**
** A followset is the set of all symbols which can come immediately
** after a configuration.
*/
void lemon_find_follow_sets(struct lemon *lemp)
{
  int i;
  struct config *cfp;
  struct plink *plp;
  int progress;
  int change;

  for(i=0; i<lemp->nstate; i++){
    for(cfp=lemp->sorted[i]->cfp; cfp; cfp=cfp->next){
      cfp->status = INCOMPLETE;
    }
  }

  do{
    progress = 0;
    for(i=0; i<lemp->nstate; i++){
      for(cfp=lemp->sorted[i]->cfp; cfp; cfp=cfp->next){
        if( cfp->status==COMPLETE ) continue;
        for(plp=cfp->fplp; plp; plp=plp->next){
          change = SetUnion(plp->cfp->fws,cfp->fws);
          if( change ){
            plp->cfp->status = INCOMPLETE;
            progress = 1;
          }
        }
        cfp->status = COMPLETE;
      }
    }
  }while( progress );
}

static int resolve_conflict(struct action *,struct action *);

/* Compute the reduce actions, and resolve conflicts.
*/
void lemon_find_actions(struct lemon *lemp)
{
  int i,j;
  struct config *cfp;
  struct state *stp;
  struct symbol *sp;
  struct rule *rp;

  /* Add all of the reduce actions
  ** A reduce action is added for each element of the followset of
  ** a configuration which has its dot at the extreme right.
  */
  for(i=0; i<lemp->nstate; i++){   /* Loop over all states */
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){  /* Loop over all configurations */
      if( cfp->rp->nrhs==cfp->dot ){        /* Is dot at extreme right? */
        for(j=0; j<lemp->nterminal; j++){
          if( SetFind(cfp->fws,j) ){
            /* Add a reduce action to the state "stp" which will reduce by the
            ** rule "cfp->rp" if the lookahead symbol is "lemp->symbols[j]" */
            Action_add(&stp->ap,REDUCE,lemp->symbols[j],(char *)cfp->rp);
          }
        }
      }
    }
  }

  /* Add the accepting token */
  cherry_add_the_accepting_token (lemp, sp);

  /* Add to the first state (which is always the starting state of the
  ** finite state machine) an action to ACCEPT if the lookahead is the
  ** start nonterminal.  */
  Action_add(&lemp->sorted[0]->ap,ACCEPT,sp,0);

  /* Resolve conflicts */
  for(i=0; i<lemp->nstate; i++){
    struct action *ap, *nap;
    stp = lemp->sorted[i];
    /* assert( stp->ap ); */
    stp->ap = Action_sort(stp->ap);
    for(ap=stp->ap; ap && ap->next; ap=ap->next){
      for(nap=ap->next; nap && nap->sp==ap->sp; nap=nap->next){
         /* The two actions "ap" and "nap" have the same lookahead.
         ** Figure out which one should be used */
         lemp->nconflict += resolve_conflict(ap,nap);
      }
    }
  }

  /* Report an error for each rule that can never be reduced. */
  for(rp=lemp->rule; rp; rp=rp->next) rp->canReduce = LEMON_FALSE;
  for(i=0; i<lemp->nstate; i++){
    struct action *ap;
    for(ap=lemp->sorted[i]->ap; ap; ap=ap->next){
      if( ap->type==REDUCE ) ap->x.rp->canReduce = LEMON_TRUE;
    }
  }
  for(rp=lemp->rule; rp; rp=rp->next){
    if( rp->canReduce ) continue;
    ErrorMsg(lemp->filename,rp->ruleline,"This rule can not be reduced.\n");
    lemp->errorcnt++;
  }
}

/* Resolve a conflict between the two given actions.  If the
** conflict can't be resolved, return non-zero.
**
** NO LONGER TRUE:
**   To resolve a conflict, first look to see if either action
**   is on an error rule.  In that case, take the action which
**   is not associated with the error rule.  If neither or both
**   actions are associated with an error rule, then try to
**   use precedence to resolve the conflict.
**
** If either action is a SHIFT, then it must be apx.  This
** function won't work if apx->type==REDUCE and apy->type==SHIFT.
*/
static int resolve_conflict(
  struct action *apx,
  struct action *apy
){
  struct symbol *spx, *spy;
  int errcnt = 0;
  assert( apx->sp==apy->sp );  /* Otherwise there would be no conflict */
  if( apx->type==SHIFT && apy->type==SHIFT ){
    apy->type = SSCONFLICT;
    errcnt++;
  }
  if( apx->type==SHIFT && apy->type==REDUCE ){
    spx = apx->sp;
    spy = apy->x.rp->precsym;
    if( spy==0 || spx->prec<0 || spy->prec<0 ){
      /* Not enough precedence information. */
      apy->type = SRCONFLICT;
      errcnt++;
    }else if( spx->prec>spy->prec ){    /* higher precedence wins */
      apy->type = RD_RESOLVED;
    }else if( spx->prec<spy->prec ){
      apx->type = SH_RESOLVED;
    }else if( spx->prec==spy->prec && spx->assoc==RIGHT ){ /* Use operator */
      apy->type = RD_RESOLVED;                             /* associativity */
    }else if( spx->prec==spy->prec && spx->assoc==LEFT ){  /* to break tie */
      apx->type = SH_RESOLVED;
    }else{
      assert( spx->prec==spy->prec && spx->assoc==NONE );
      apx->type = ERROR;
    }
  }else if( apx->type==REDUCE && apy->type==REDUCE ){
    spx = apx->x.rp->precsym;
    spy = apy->x.rp->precsym;
    if( spx==0 || spy==0 || spx->prec<0 ||
    spy->prec<0 || spx->prec==spy->prec ){
      apy->type = RRCONFLICT;
      errcnt++;
    }else if( spx->prec>spy->prec ){
      apy->type = RD_RESOLVED;
    }else if( spx->prec<spy->prec ){
      apx->type = RD_RESOLVED;
    }
  }else{
    assert(
      apx->type==SH_RESOLVED ||
      apx->type==RD_RESOLVED ||
      apx->type==SSCONFLICT ||
      apx->type==SRCONFLICT ||
      apx->type==RRCONFLICT ||
      apy->type==SH_RESOLVED ||
      apy->type==RD_RESOLVED ||
      apy->type==SSCONFLICT ||
      apy->type==SRCONFLICT ||
      apy->type==RRCONFLICT
    );
    /* The REDUCE/SHIFT case cannot happen because SHIFTs come before
    ** REDUCEs on the list.  If we reach this point it must be because
    ** the parser conflict had already been resolved. */
  }
  return errcnt;
}
/********************* From the file "configlist.c" *************************/
/*
** Routines to processing a configuration list and building a state
** in the LEMON parser generator.
*/

static struct config *freelist = 0;      /* List of free configurations */
static struct config *current = 0;       /* Top of list of configurations */
static struct config **currentend = 0;   /* Last on list of configs */
static struct config *basis = 0;         /* Top of list of basis configs */
static struct config **basisend = 0;     /* End of list of basis configs */

/* Return a pointer to a new configuration */
PRIVATE struct config *newconfig(void){
  struct config *newcfg;
  if( freelist==0 ){
    int i;
    int amt = 3;
    freelist = (struct config *)calloc( amt, sizeof(struct config) );
    if( freelist==0 ){
      fprintf(stderr,"Unable to allocate memory for a new configuration.");
      exit(1);
    }
    for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
    freelist[amt-1].next = 0;
  }
  newcfg = freelist;
  freelist = freelist->next;
  return newcfg;
}

/* The configuration "old" is no longer used */
PRIVATE void deleteconfig(struct config *old)
{
  old->next = freelist;
  freelist = old;
}

/* Initialized the configuration list builder */
void lemon_configlist_init(void){
  current = 0;
  currentend = &current;
  basis = 0;
  basisend = &basis;
  Configtable_init();
  return;
}

/* Initialized the configuration list builder */
void Configlist_reset(void){
  current = 0;
  currentend = &current;
  basis = 0;
  basisend = &basis;
  Configtable_clear(0);
  return;
}

/* Add another configuration to the configuration list */
struct config *Configlist_add(
  struct rule *rp,    /* The rule */
  int dot             /* Index into the RHS of the rule where the dot goes */
){
  struct config *cfp, model;

  assert( currentend!=0 );
  model.rp = rp;
  model.dot = dot;
  cfp = Configtable_find(&model);
  if( cfp==0 ){
    cfp = newconfig();
    cfp->rp = rp;
    cfp->dot = dot;
    cfp->fws = SetNew();
    cfp->stp = 0;
    cfp->fplp = cfp->bplp = 0;
    cfp->next = 0;
    cfp->bp = 0;
    *currentend = cfp;
    currentend = &cfp->next;
    Configtable_insert(cfp);
  }
  return cfp;
}

/* Add a basis configuration to the configuration list */
struct config *lemon_configlist_add_basis(struct rule *rp, int dot)
{
  struct config *cfp, model;

  assert( basisend!=0 );
  assert( currentend!=0 );
  model.rp = rp;
  model.dot = dot;
  cfp = Configtable_find(&model);
  if( cfp==0 ){
    cfp = newconfig();
    cfp->rp = rp;
    cfp->dot = dot;
    cfp->fws = SetNew();
    cfp->stp = 0;
    cfp->fplp = cfp->bplp = 0;
    cfp->next = 0;
    cfp->bp = 0;
    *currentend = cfp;
    currentend = &cfp->next;
    *basisend = cfp;
    basisend = &cfp->bp;
    Configtable_insert(cfp);
  }
  return cfp;
}

/* Compute the closure of the configuration list */
void Configlist_closure(struct lemon *lemp)
{
  struct config *cfp, *newcfp;
  struct rule *rp, *newrp;
  struct symbol *sp, *xsp;
  int i, dot;

  assert( currentend!=0 );
  for(cfp=current; cfp; cfp=cfp->next){
    rp = cfp->rp;
    dot = cfp->dot;
    if( dot>=rp->nrhs ) continue;
    sp = rp->rhs[dot];
    if( sp->type==NONTERMINAL ){
      if( sp->rule==0 && sp!=lemp->errsym ){
        ErrorMsg(lemp->filename,rp->line,"Nonterminal \"%s\" has no rules.",
          sp->name);
        lemp->errorcnt++;
      }
      for(newrp=sp->rule; newrp; newrp=newrp->nextlhs){
        newcfp = Configlist_add(newrp,0);
        for(i=dot+1; i<rp->nrhs; i++){
          xsp = rp->rhs[i];
          if( xsp->type==TERMINAL ){
            lemon_set_add(newcfp->fws,xsp->index);
            break;
          }else if( xsp->type==MULTITERMINAL ){
            int k;
            for(k=0; k<xsp->nsubsym; k++){
              lemon_set_add(newcfp->fws, xsp->subsym[k]->index);
            }
            break;
          }else{
            SetUnion(newcfp->fws,xsp->firstset);
            if( xsp->lambda==LEMON_FALSE ) break;
          }
        }
        if( i==rp->nrhs ) Plink_add(&cfp->fplp,newcfp);
      }
    }
  }
  return;
}

/* Sort the configuration list */
void Configlist_sort(void){
  current = (struct config*)msort((char*)current,(char**)&(current->next),
                                  Configcmp);
  currentend = 0;
  return;
}

/* Sort the basis configuration list */
void Configlist_sortbasis(void){
  basis = (struct config*)msort((char*)current,(char**)&(current->bp),
                                Configcmp);
  basisend = 0;
  return;
}

/* Return a pointer to the head of the configuration list and
** reset the list */
struct config *Configlist_return(void){
  struct config *old;
  old = current;
  current = 0;
  currentend = 0;
  return old;
}

/* Return a pointer to the head of the configuration list and
** reset the list */
struct config *Configlist_basis(void){
  struct config *old;
  old = basis;
  basis = 0;
  basisend = 0;
  return old;
}

/* Free all elements of the given configuration list */
void Configlist_eat(struct config *cfp)
{
  struct config *nextcfp;
  for(; cfp; cfp=nextcfp){
    nextcfp = cfp->next;
    assert( cfp->fplp==0 );
    assert( cfp->bplp==0 );
    if( cfp->fws ) SetFree(cfp->fws);
    deleteconfig(cfp);
  }
  return;
}
/***************** From the file "error.c" *********************************/
/*
** Code for printing error message.
*/

void ErrorMsg(const char *filename, int lineno, const char *format, ...){
  va_list ap;
  fprintf(stderr, "%s:%d: ", filename, lineno);
  va_start(ap, format);
  vfprintf(stderr,format,ap);
  va_end(ap);
  fprintf(stderr, "\n");
}
/**************** From the file "main.c" ************************************/
/*
** Main program file for the LEMON parser generator.
*/

/* Report an out-of-memory condition and abort.  This function
** is used mostly by the "MemoryCheck" macro in struct.h
*/
void memory_error(void){
  fprintf(stderr,"Out of memory.  Aborting...\n");
  exit(1);
}


/* Rember the name of the output directory 
*/
static char *outputDir = NULL;


/* Merge together to lists of rules ordered by rule.iRule */
static struct rule *Rule_merge(struct rule *pA, struct rule *pB){
  struct rule *pFirst = 0;
  struct rule **ppPrev = &pFirst;
  while( pA && pB ){
    if( pA->iRule<pB->iRule ){
      *ppPrev = pA;
      ppPrev = &pA->next;
      pA = pA->next;
    }else{
      *ppPrev = pB;
      ppPrev = &pB->next;
      pB = pB->next;
    }
  }
  if( pA ){
    *ppPrev = pA;
  }else{
    *ppPrev = pB;
  }
  return pFirst;
}

/*
** Sort a list of rules in order of increasing iRule value
*/
//static struct rule *Rule_sort(struct rule *rp){
//static struct rule *lime_rule_sort (struct rule *rp)
struct rule *lime_rule_sort (struct rule *rp)
{
  int i;
  struct rule *pNext;
  struct rule *x[32];
  memset(x, 0, sizeof(x));
  while( rp ){
    pNext = rp->next;
    rp->next = 0;
    for(i=0; i<sizeof(x)/sizeof(x[0]) && x[i]; i++){
      rp = Rule_merge(x[i], rp);
      x[i] = 0;
    }
    x[i] = rp;
    rp = pNext;
  }
  rp = 0;
  for(i=0; i<sizeof(x)/sizeof(x[0]); i++){
    rp = Rule_merge(x[i], rp);
  }
  return rp;
}

#if 0
/* forward reference */
static const char *minimum_size_type(int lwr, int upr, int *pnByte);
#endif

/* Print a single line of the "Parser Stats" output
 */
static void stats_line(const char *zLabel, int iValue){
  int nLabel = lemonStrlen(zLabel);
  printf("  %s%.*s %5d\n", zLabel,
         35-nLabel, "................................",
         iValue);
}

/* Generate a filename with the given suffix.  Space to hold the
** name comes from malloc() and must be freed by the calling
** function.
*/
PRIVATE char *file_makename(struct lemon *lemp, const char *suffix)
{
  char *name;
  char *cp;
  char *filename = lemp->filename;
  int sz;

  if( outputDir ){
    cp = strrchr(filename, '/');
    if( cp ) filename = cp + 1;
  }
  sz = lemonStrlen(filename);
  sz += lemonStrlen(suffix);
  if( outputDir ) sz += lemonStrlen(outputDir) + 1;
  sz += 5;
  name = (char*)malloc( sz );
  if( name==0 ){
    fprintf(stderr,"Can't allocate space for a filename.\n");
    exit(1);
  }
  name[0] = 0;
  if( outputDir ){
    lemon_strcpy(name, outputDir);
    lemon_strcat(name, "/");
  }
  lemon_strcat(name,filename);
  cp = strrchr(name,'.');
  if( cp ) *cp = 0;
  lemon_strcat(name,suffix);
  return name;
}


/*
** The main program entry from Ada
*/

/******************** From the file "msort.c" *******************************/
/*
** A generic merge-sort program.
**
** USAGE:
** Let "ptr" be a pointer to some structure which is at the head of
** a null-terminated list.  Then to sort the list call:
**
**     ptr = msort(ptr,&(ptr->next),cmpfnc);
**
** In the above, "cmpfnc" is a pointer to a function which compares
** two instances of the structure and returns an integer, as in
** strcmp.  The second argument is a pointer to the pointer to the
** second element of the linked list.  This address is used to compute
** the offset to the "next" field within the structure.  The offset to
** the "next" field must be constant for all structures in the list.
**
** The function returns a new pointer which is the head of the list
** after sorting.
**
** ALGORITHM:
** Merge-sort.
*/

/*
** Return a pointer to the next structure in the linked list.
*/
#define NEXT(A) (*(char**)(((char*)A)+offset))

/*
** Inputs:
**   a:       A sorted, null-terminated linked list.  (May be null).
**   b:       A sorted, null-terminated linked list.  (May be null).
**   cmp:     A pointer to the comparison function.
**   offset:  Offset in the structure to the "next" field.
**
** Return Value:
**   A pointer to the head of a sorted list containing the elements
**   of both a and b.
**
** Side effects:
**   The "next" pointers for elements in the lists a and b are
**   changed.
*/
static char *merge(
  char *a,
  char *b,
  int (*cmp)(const char*,const char*),
  int offset
){
  char *ptr, *head;

  if( a==0 ){
    head = b;
  }else if( b==0 ){
    head = a;
  }else{
    if( (*cmp)(a,b)<=0 ){
      ptr = a;
      a = NEXT(a);
    }else{
      ptr = b;
      b = NEXT(b);
    }
    head = ptr;
    while( a && b ){
      if( (*cmp)(a,b)<=0 ){
        NEXT(ptr) = a;
        ptr = a;
        a = NEXT(a);
      }else{
        NEXT(ptr) = b;
        ptr = b;
        b = NEXT(b);
      }
    }
    if( a ) NEXT(ptr) = a;
    else    NEXT(ptr) = b;
  }
  return head;
}

/*
** Inputs:
**   list:      Pointer to a singly-linked list of structures.
**   next:      Pointer to pointer to the second element of the list.
**   cmp:       A comparison function.
**
** Return Value:
**   A pointer to the head of a sorted list containing the elements
**   orginally in list.
**
** Side effects:
**   The "next" pointers for elements in list are changed.
*/
#define LISTSIZE 30
static char *msort(
  char *list,
  char **next,
  int (*cmp)(const char*,const char*)
){
  unsigned long offset;
  char *ep;
  char *set[LISTSIZE];
  int i;
  offset = (unsigned long)((char*)next - (char*)list);
  for(i=0; i<LISTSIZE; i++) set[i] = 0;
  while( list ){
    ep = list;
    list = NEXT(list);
    NEXT(ep) = 0;
    for(i=0; i<LISTSIZE-1 && set[i]!=0; i++){
      ep = merge(ep,set[i],cmp,offset);
      set[i] = 0;
    }
    set[i] = ep;
  }
  ep = 0;
  for(i=0; i<LISTSIZE; i++) if( set[i] ) ep = merge(set[i],ep,cmp,offset);
  return ep;
}



/*************************** From the file "plink.c" *********************/
/*
** Routines processing configuration follow-set propagation links
** in the LEMON parser generator.
*/
static struct plink *plink_freelist = 0;

/* Allocate a new plink */
struct plink *Plink_new(void){
  struct plink *newlink;

  if( plink_freelist==0 ){
    int i;
    int amt = 100;
    plink_freelist = (struct plink *)calloc( amt, sizeof(struct plink) );
    if( plink_freelist==0 ){
      fprintf(stderr,
      "Unable to allocate memory for a new follow-set propagation link.\n");
      exit(1);
    }
    for(i=0; i<amt-1; i++) plink_freelist[i].next = &plink_freelist[i+1];
    plink_freelist[amt-1].next = 0;
  }
  newlink = plink_freelist;
  plink_freelist = plink_freelist->next;
  return newlink;
}

/* Add a plink to a plink list */
void Plink_add(struct plink **plpp, struct config *cfp)
{
  struct plink *newlink;
  newlink = Plink_new();
  newlink->next = *plpp;
  *plpp = newlink;
  newlink->cfp = cfp;
}

/* Transfer every plink on the list "from" to the list "to" */
void Plink_copy(struct plink **to, struct plink *from)
{
  struct plink *nextpl;
  while( from ){
    nextpl = from->next;
    from->next = *to;
    *to = from;
    from = nextpl;
  }
}

/* Delete every plink on the list */
void Plink_delete(struct plink *plp)
{
  struct plink *nextpl;

  while( plp ){
    nextpl = plp->next;
    plp->next = plink_freelist;
    plink_freelist = plp;
    plp = nextpl;
  }
}
/*********************** From the file "report.c" **************************/
/*
** Procedures for generating reports and tables in the LEMON parser generator.
*/

/* Open a file with a name based on the name of the input file,
** but with a different (specified) suffix, and return a pointer
** to the stream */

PRIVATE FILE *file_open(
  struct lemon *lemp,
  const char *suffix,
  const char *mode
){
  FILE *fp;

  //if( lemp->outname ) free(lemp->outname);
  // if( lime_get_out_name () ) free( lime_get_out_name () );
  //lemp->outname = file_makename(lemp, suffix);
  //lime_set_out_name (file_makename(lemp, suffix));
  lemp->outname = file_makename(lemp, suffix);
  //fp = fopen(lime_get_out_name (), mode);
  fp = fopen(lemp->outname,mode);
  if( fp==0 && *mode=='w' ){
    fprintf(stderr,"Can't open file \"%s\".\n",lemp->outname);
    //fprintf(stderr,"Can't open file \"%s\".\n",lime_get_out_name ());
    lemp->errorcnt++;
    return 0;
  }
  return fp;
}


#if 1
/* Print a single rule.
*/
void RulePrint(FILE *fp, struct rule *rp, int iCursor){
  struct symbol *sp;
  int i, j;
  fprintf(fp,"%s ::=",rp->lhs->name);
  for(i=0; i<=rp->nrhs; i++){
    if( i==iCursor ) fprintf(fp," *");
    if( i==rp->nrhs ) break;
    sp = rp->rhs[i];
    if( sp->type==MULTITERMINAL ){
      fprintf(fp," %s", sp->subsym[0]->name);
      for(j=1; j<sp->nsubsym; j++){
        fprintf(fp,"|%s",sp->subsym[j]->name);
      }
    }else{
      fprintf(fp," %s", sp->name);
    }
  }
}
#endif

/* Print the rule for a configuration.
*/
void ConfigPrint(FILE *fp, struct config *cfp){
  RulePrint(fp, cfp->rp, cfp->dot);
}

/* #define TEST */
#if 0
/* Print a set */
PRIVATE void SetPrint(out,set,lemp)
FILE *out;
char *set;
struct lemon *lemp;
{
  int i;
  char *spacer;
  spacer = "";
  fprintf(out,"%12s[","");
  for(i=0; i<lemp->nterminal; i++){
    if( SetFind(set,i) ){
      fprintf(out,"%s%s",spacer,lemp->symbols[i]->name);
      spacer = " ";
    }
  }
  fprintf(out,"]\n");
}

/* Print a plink chain */
PRIVATE void PlinkPrint(out,plp,tag)
FILE *out;
struct plink *plp;
char *tag;
{
  while( plp ){
    fprintf(out,"%12s%s (state %2d) ","",tag,plp->cfp->stp->statenum);
    ConfigPrint(out,plp->cfp);
    fprintf(out,"\n");
    plp = plp->next;
  }
}
#endif

#if 0
/* Print an action to the given file descriptor.  Return FALSE if
** nothing was actually printed.
*/
int PrintAction(
  struct action *ap,          /* The action to print */
  FILE *fp,                   /* Print the action here */
  int indent                  /* Indent by this amount */
){
  int result = 1;
  switch( ap->type ){
    case SHIFT: {
      struct state *stp = ap->x.stp;
      fprintf(fp,"%*s shift        %-7d",indent,ap->sp->name,stp->statenum);
      break;
    }
    case REDUCE: {
      struct rule *rp = ap->x.rp;
      fprintf(fp,"%*s reduce       %-7d",indent,ap->sp->name,rp->iRule);
      RulePrint(fp, rp, -1);
      break;
    }
    case SHIFTREDUCE: {
      struct rule *rp = ap->x.rp;
      fprintf(fp,"%*s shift-reduce %-7d",indent,ap->sp->name,rp->iRule);
      RulePrint(fp, rp, -1);
      break;
    }
    case ACCEPT:
      fprintf(fp,"%*s accept",indent,ap->sp->name);
      break;
    case ERROR:
      fprintf(fp,"%*s error",indent,ap->sp->name);
      break;
    case SRCONFLICT:
    case RRCONFLICT:
      fprintf(fp,"%*s reduce       %-7d ** Parsing conflict **",
        indent,ap->sp->name,ap->x.rp->iRule);
      break;
    case SSCONFLICT:
      fprintf(fp,"%*s shift        %-7d ** Parsing conflict **",
        indent,ap->sp->name,ap->x.stp->statenum);
      break;
    case SH_RESOLVED:
      if( lemon_show_conflict ){
        fprintf(fp,"%*s shift        %-7d -- dropped by precedence",
                indent,ap->sp->name,ap->x.stp->statenum);
      }else{
        result = 0;
      }
      break;
    case RD_RESOLVED:
      if( lemon_show_conflict ){
        fprintf(fp,"%*s reduce %-7d -- dropped by precedence",
                indent,ap->sp->name,ap->x.rp->iRule);
      }else{
        result = 0;
      }
      break;
    case NOT_USED:
      result = 0;
      break;
  }
  if( result && ap->spOpt ){
    fprintf(fp,"  /* because %s==%s */", ap->sp->name, ap->spOpt->name);
  }
  return result;
}
#endif



/* Search for the file "name" which is in the same directory as
** the exacutable */
PRIVATE char *pathsearch(char *argv0, char *name, int modemask)
{
  const char *pathlist;
  char *pathbufptr;
  char *pathbuf;
  char *path,*cp;
  char c;

#ifdef __WIN32__
  cp = strrchr(argv0,'\\');
#else
  cp = strrchr(argv0,'/');
#endif
  if( cp ){
    c = *cp;
    *cp = 0;
    path = (char *)malloc( lemonStrlen(argv0) + lemonStrlen(name) + 2 );
    if( path ) lemon_sprintf(path,"%s/%s",argv0,name);
    *cp = c;
  }else{
    pathlist = getenv("PATH");
    if( pathlist==0 ) pathlist = ".:/bin:/usr/bin";
    pathbuf = (char *) malloc( lemonStrlen(pathlist) + 1 );
    path = (char *)malloc( lemonStrlen(pathlist)+lemonStrlen(name)+2 );
    if( (pathbuf != 0) && (path!=0) ){
      pathbufptr = pathbuf;
      lemon_strcpy(pathbuf, pathlist);
      while( *pathbuf ){
        cp = strchr(pathbuf,':');
        if( cp==0 ) cp = &pathbuf[lemonStrlen(pathbuf)];
        c = *cp;
        *cp = 0;
        lemon_sprintf(path,"%s/%s",pathbuf,name);
        *cp = c;
        if( c==0 ) pathbuf[0] = 0;
        else pathbuf = &cp[1];
        if( access(path,modemask)==0 ) break;
      }
      free(pathbufptr);
    }
  }
  return path;
}

/* Given an action, compute the integer value for that action
** which is to be put in the action table of the generated machine.
** Return negative if no action should be generated.
*/
PRIVATE int compute_action(struct lemon *lemp, struct action *ap)
{
  int act;
  switch( ap->type ){
    case SHIFT:  act = ap->x.stp->statenum;                        break;
    case SHIFTREDUCE: {
      /* Since a SHIFT is inherient after a prior REDUCE, convert any
      ** SHIFTREDUCE action with a nonterminal on the LHS into a simple
      ** REDUCE action: */
      if( ap->sp->index>=lemp->nterminal ){
        act = lemp->minReduce + ap->x.rp->iRule;
      }else{
        act = lemp->minShiftReduce + ap->x.rp->iRule;
      }
      break;
    }
    case REDUCE: act = lemp->minReduce + ap->x.rp->iRule;          break;
    case ERROR:  act = lemp->errAction;                            break;
    case ACCEPT: act = lemp->accAction;                            break;
    default:     act = -1; break;
  }
  return act;
}

#define LINESIZE 1000
/* The next cluster of routines are for reading the template file
** and writing the results to the generated parser */
/* The first function transfers data from "in" to "out" until
** a line is seen which begins with "%%".  The line number is
** tracked.
**
** if name!=0, then any word that begin with "Parse" is changed to
** begin with *name instead.
*/
PRIVATE void tplt_xfer(char *name, FILE *in, FILE *out, int *lineno)
{
  int i, iStart;
  char line[LINESIZE];
  while( fgets(line,LINESIZE,in) && (line[0]!='%' || line[1]!='%') ){
    (*lineno)++;
    iStart = 0;
    if( name ){
      for(i=0; line[i]; i++){
        if( line[i]=='P' && strncmp(&line[i],"Parse",5)==0
          && (i==0 || !ISALPHA(line[i-1]))
        ){
          if( i>iStart ) fprintf(out,"%.*s",i-iStart,&line[iStart]);
          fprintf(out,"%s",name);
          i += 4;
          iStart = i+1;
        }
      }
    }
    fprintf(out,"%s",&line[iStart]);
  }
}


/* Print a #line directive line to the output file. */
PRIVATE void tplt_linedir(FILE *out, int lineno, char *filename)
{
  //lime_template_line_directive (lineno, filename);
  
  fprintf(out,"#line %d \"",lineno);
  while( *filename ){
    if( *filename == '\\' ) putc('\\',out);
    putc(*filename,out);
    filename++;
  }
  fprintf(out,"\"\n");
}

/* Print a string to the file and keep the linenumber up to date */
/*
PRIVATE void tplt_print(FILE *out, struct lemoan *lemp, char *str, int *lineno)
{
  if( str==0 ) return;
  while( *str ){
    putc(*str,out);
    if( *str=='\n' ) (*lineno)++;
    str++;
  }
  if( str[-1]!='\n' ){
    putc('\n',out);
    (*lineno)++;
  }
  if (!lemp->nolinenosflag) {
    (*lineno)++; tplt_linedir(out,*lineno,lemp->outname);
  }
  return;
}
*/

#if 0
/*
** The following routine emits code for the destructor for the
** symbol sp
*/

void emit_destructor_code
(
 struct symbol *sp,
 struct lemon *lemp
)
{
 char *cp = 0;

 if( sp->type==TERMINAL ){
   cp = lemp->tokendest;
   if( cp==0 ) return;

   lime_put_line ("{");
 }else if( sp->destructor ){
   cp = sp->destructor;
   lime_put_line ("{");
   if( !lemp->nolinenosflag ){
     //(*lineno)++;   // XXX
     lime_write_line_directive (sp->destLineno, lemp->filename);
   }
 }else if( lemp->vardest ){
   cp = lemp->vardest;
   if( cp==0 ) return;

   lime_put_line ("{");
 }else{
 assert( 0 );  // Cannot happen
 }
 for(; *cp; cp++){
   if( *cp=='$' && cp[1]=='$' ){
     lime_put ("(yypminor->yy");
     lime_put_int (sp->dtnum);
     lime_put (")");
     cp++;
     continue;
   }
   //   if( *cp=='\n' ) (*lineno)++;  //  XXX
   //fputc(*cp,out);
   char lime_string[2];
   lime_string[0] = *cp;
   lime_string[1] = 0;
   lime_put (lime_string);
   //lime_put ("" & *cp);  // XXX
 }
 lime_put_line ("");
 if (!lemp->nolinenosflag) {
   lime_write_line_directive (0, lemp->outname);
   //lime_write_line_directive (0, lime_get_out_name ());
 }
 lime_put_line ("}");
 return;
}
#endif

/*
** Return TRUE (non-zero) if the given symbol has a destructor.
*/
int has_destructor(struct symbol *sp, struct lemon *lemp)
{
  int ret;
  if( sp->type==TERMINAL ){
    ret = lemp->tokendest!=0;
  }else{
    ret = lemp->vardest!=0 || sp->destructor!=0;
  }
  return ret;
}

/*
** Append text to a dynamically allocated string.  If zText is 0 then
** reset the string to be empty again.  Always return the complete text
** of the string (which is overwritten with each call).
**
** n bytes of zText are stored.  If n==0 then all of zText up to the first
** \000 terminator is stored.  zText can contain up to two instances of
** %d.  The values of p1 and p2 are written into the first and second
** %d.
**
** If n==-1, then the previous character is overwritten.
*/
PRIVATE char *append_str(const char *zText, int n, int p1, int p2){
  static char empty[1] = { 0 };
  static char *z = 0;
  static int alloced = 0;
  static int used = 0;
  int c;
  char zInt[40];
  if( zText==0 ){
    if( used==0 && z!=0 ) z[0] = 0;
    used = 0;
    return z;
  }
  if( n<=0 ){
    if( n<0 ){
      used += n;
      assert( used>=0 );
    }
    n = lemonStrlen(zText);
  }
  if( (int) (n+sizeof(zInt)*2+used) >= alloced ){
    alloced = n + sizeof(zInt)*2 + used + 200;
    z = (char *) realloc(z,  alloced);
  }
  if( z==0 ) return empty;
  while( n-- > 0 ){
    c = *(zText++);
    if( c=='%' && n>0 && zText[0]=='d' ){
      lemon_sprintf(zInt, "%d", p1);
      p1 = p2;
      lemon_strcpy(&z[used], zInt);
      used += lemonStrlen(&z[used]);
      zText++;
      n--;
    }else{
      z[used++] = (char)c;
    }
  }
  z[used] = 0;
  return z;
}

/*
** Write and transform the rp->code string so that symbols are expanded.
** Populate the rp->codePrefix and rp->codeSuffix strings, as appropriate.
**
** Return 1 if the expanded code requires that "yylhsminor" local variable
** to be defined.
*/
PRIVATE int translate_code(struct lemon *lemp, struct rule *rp){
  char *cp, *xp;
  int i;
  int rc = 0;            /* True if yylhsminor is used */
  int dontUseRhs0 = 0;   /* If true, use of left-most RHS label is illegal */
  const char *zSkip = 0; /* The zOvwrt comment within rp->code, or NULL */
  char lhsused = 0;      /* True if the LHS element has been used */
  char lhsdirect;        /* True if LHS writes directly into stack */
  char used[MAXRHS];     /* True for each RHS element which is used */
  char zLhs[50];         /* Convert the LHS symbol into this string */
  char zOvwrt[900];      /* Comment that to allow LHS to overwrite RHS */

  for(i=0; i<rp->nrhs; i++) used[i] = 0;
  lhsused = 0;

  if( rp->code==0 ){
    static char newlinestr[2] = { '\n', '\0' };
    rp->code = newlinestr;
    rp->line = rp->ruleline;
    rp->noCode = 1;
  }else{
    rp->noCode = 0;
  }


  if( rp->nrhs==0 ){
    /* If there are no RHS symbols, then writing directly to the LHS is ok */
    lhsdirect = 1;
  }else if( rp->rhsalias[0]==0 ){
    /* The left-most RHS symbol has no value.  LHS direct is ok.  But
    ** we have to call the distructor on the RHS symbol first. */
    lhsdirect = 1;
    if( has_destructor(rp->rhs[0],lemp) ){
      append_str(0,0,0,0);
      append_str("  yy_destructor(yypParser,%d,&yymsp[%d].minor);\n", 0,
                 rp->rhs[0]->index,1-rp->nrhs);
      rp->codePrefix = Strsafe(append_str(0,0,0,0));
      rp->noCode = 0;
    }
  }else if( rp->lhsalias==0 ){
    /* There is no LHS value symbol. */
    lhsdirect = 1;
  }else if( strcmp(rp->lhsalias,rp->rhsalias[0])==0 ){
    /* The LHS symbol and the left-most RHS symbol are the same, so
    ** direct writing is allowed */
    lhsdirect = 1;
    lhsused = 1;
    used[0] = 1;
    if( rp->lhs->dtnum!=rp->rhs[0]->dtnum ){
      ErrorMsg(lemp->filename,rp->ruleline,
        "%s(%s) and %s(%s) share the same label but have "
        "different datatypes.",
        rp->lhs->name, rp->lhsalias, rp->rhs[0]->name, rp->rhsalias[0]);
      lemp->errorcnt++;
    }
  }else{
    lemon_sprintf(zOvwrt, "/*%s-overwrites-%s*/",
                  rp->lhsalias, rp->rhsalias[0]);
    zSkip = strstr(rp->code, zOvwrt);
    if( zSkip!=0 ){
      /* The code contains a special comment that indicates that it is safe
      ** for the LHS label to overwrite left-most RHS label. */
      lhsdirect = 1;
    }else{
      lhsdirect = 0;
    }
  }
  if( lhsdirect ){
    sprintf(zLhs, "yymsp[%d].minor.yy%d",1-rp->nrhs,rp->lhs->dtnum);
  }else{
    rc = 1;
    sprintf(zLhs, "yylhsminor.yy%d",rp->lhs->dtnum);
  }

  append_str(0,0,0,0);

  /* This const cast is wrong but harmless, if we're careful. */
  for(cp=(char *)rp->code; *cp; cp++){
    if( cp==zSkip ){
      append_str(zOvwrt,0,0,0);
      cp += lemonStrlen(zOvwrt)-1;
      dontUseRhs0 = 1;
      continue;
    }
    if( ISALPHA(*cp) && (cp==rp->code || (!ISALNUM(cp[-1]) && cp[-1]!='_')) ){
      char saved;
      for(xp= &cp[1]; ISALNUM(*xp) || *xp=='_'; xp++);
      saved = *xp;
      *xp = 0;
      if( rp->lhsalias && strcmp(cp,rp->lhsalias)==0 ){
        append_str(zLhs,0,0,0);
        cp = xp;
        lhsused = 1;
      }else{
        for(i=0; i<rp->nrhs; i++){
          if( rp->rhsalias[i] && strcmp(cp,rp->rhsalias[i])==0 ){
            if( i==0 && dontUseRhs0 ){
              ErrorMsg(lemp->filename,rp->ruleline,
                 "Label %s used after '%s'.",
                 rp->rhsalias[0], zOvwrt);
              lemp->errorcnt++;
            }else if( cp!=rp->code && cp[-1]=='@' ){
              /* If the argument is of the form @X then substituted
              ** the token number of X, not the value of X */
              append_str("yymsp[%d].major",-1,i-rp->nrhs+1,0);
            }else{
              struct symbol *sp = rp->rhs[i];
              int dtnum;
              if( sp->type==MULTITERMINAL ){
                dtnum = sp->subsym[0]->dtnum;
              }else{
                dtnum = sp->dtnum;
              }
              append_str("yymsp[%d].minor.yy%d",0,i-rp->nrhs+1, dtnum);
            }
            cp = xp;
            used[i] = 1;
            break;
          }
        }
      }
      *xp = saved;
    }
    append_str(cp, 1, 0, 0);
  } /* End loop */

  /* Main code generation completed */
  cp = append_str(0,0,0,0);
  if( cp && cp[0] ) rp->code = Strsafe(cp);
  append_str(0,0,0,0);

  /* Check to make sure the LHS has been used */
  if( rp->lhsalias && !lhsused ){
    ErrorMsg(lemp->filename,rp->ruleline,
      "Label \"%s\" for \"%s(%s)\" is never used.",
        rp->lhsalias,rp->lhs->name,rp->lhsalias);
    lemp->errorcnt++;
  }

  /* Generate destructor code for RHS minor values which are not referenced.
  ** Generate error messages for unused labels and duplicate labels.
  */
  for(i=0; i<rp->nrhs; i++){
    if( rp->rhsalias[i] ){
      if( i>0 ){
        int j;
        if( rp->lhsalias && strcmp(rp->lhsalias,rp->rhsalias[i])==0 ){
          ErrorMsg(lemp->filename,rp->ruleline,
            "%s(%s) has the same label as the LHS but is not the left-most "
            "symbol on the RHS.",
            rp->rhs[i]->name, rp->rhsalias);
          lemp->errorcnt++;
        }
        for(j=0; j<i; j++){
          if( rp->rhsalias[j] && strcmp(rp->rhsalias[j],rp->rhsalias[i])==0 ){
            ErrorMsg(lemp->filename,rp->ruleline,
              "Label %s used for multiple symbols on the RHS of a rule.",
              rp->rhsalias[i]);
            lemp->errorcnt++;
            break;
          }
        }
      }
      if( !used[i] ){
        ErrorMsg(lemp->filename,rp->ruleline,
          "Label %s for \"%s(%s)\" is never used.",
          rp->rhsalias[i],rp->rhs[i]->name,rp->rhsalias[i]);
        lemp->errorcnt++;
      }
    }else if( i>0 && has_destructor(rp->rhs[i],lemp) ){
      append_str("  yy_destructor(yypParser,%d,&yymsp[%d].minor);\n", 0,
         rp->rhs[i]->index,i-rp->nrhs+1);
    }
  }

  /* If unable to write LHS values directly into the stack, write the
  ** saved LHS value now. */
  if( lhsdirect==0 ){
    append_str("  yymsp[%d].minor.yy%d = ", 0, 1-rp->nrhs, rp->lhs->dtnum);
    append_str(zLhs, 0, 0, 0);
    append_str(";\n", 0, 0, 0);
  }

  /* Suffix code generation complete */
  cp = append_str(0,0,0,0);
  if( cp && cp[0] ){
    rp->codeSuffix = Strsafe(cp);
    rp->noCode = 0;
  }

  return rc;
}

/*
** Compare two states for sorting purposes.  The smaller state is the
** one with the most non-terminal actions.  If they have the same number
** of non-terminal actions, then the smaller is the one with the most
** token actions.
*/
static int stateResortCompare(const void *a, const void *b){
  const struct state *pA = *(const struct state**)a;
  const struct state *pB = *(const struct state**)b;
  int n;

  n = pB->nNtAct - pA->nNtAct;
  if( n==0 ){
    n = pB->nTknAct - pA->nTknAct;
    if( n==0 ){
      n = pB->statenum - pA->statenum;
    }
  }
  assert( n!=0 );
  return n;
}


#if 0
/*
** Renumber and resort states so that states with fewer choices
** occur at the end.  Except, keep state 0 as the first state.
*/
void lemon_resort_states (struct lemon *lemp)
{
  int i;
  struct state *stp;
  struct action *ap;

  printf ("ResortStates\n");
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    stp->nTknAct = stp->nNtAct = 0;
    stp->iDfltReduce = -1; /* Init dflt action to "syntax error" */
    stp->iTknOfst = NO_OFFSET;
    stp->iNtOfst = NO_OFFSET;
    for(ap=stp->ap; ap; ap=ap->next){
      int iAction = compute_action(lemp,ap);
      if( iAction>=0 ){
        if( ap->sp->index<lemp->nterminal ){
          stp->nTknAct++;
        }else if( ap->sp->index<lemp->nsymbol ){
          stp->nNtAct++;
        }else{
          assert( stp->autoReduce==0 || stp->pDfltReduce==ap->x.rp );
          stp->iDfltReduce = iAction;
        }
      }
    }
  }
  qsort(&lemp->sorted[1], lemp->nstate-1, sizeof(lemp->sorted[0]),
        stateResortCompare);
  for(i=0; i<lemp->nstate; i++){
    lemp->sorted[i]->statenum = i;
  }
  lemp->nxstate = lemp->nstate;
  while( lemp->nxstate>1 && lemp->sorted[lemp->nxstate-1]->autoReduce ){
    lemp->nxstate--;
  }
}
#endif


/***************** From the file "set.c" ************************************/
/*
** Set manipulation routines for the LEMON parser generator.
*/

static int size = 0;

/* Set the set size */
void lemon_set_size (int n)
{
  size = n+1;
}

/* Allocate a new set */
char *SetNew(void){
  char *s;
  s = (char*)calloc( size, 1);
  if( s==0 ){
    extern void memory_error();
    memory_error();
  }
  return s;
}

/* Deallocate a set */
void SetFree(char *s)
{
  free(s);
}

/*
**
*/
void lemon_compute_LR_states (struct lemon *lemp)
{
  lemp->nstate = 0;
  cherry_find_states (lemp);
  lemp->sorted = State_arrayof();
}
  

/* Add a new element to the set.  Return TRUE if the element was added
** and FALSE if it was already there. */
int lemon_set_add(char *s, int e)
{
  int rv;
  assert( e>=0 && e<size );
  rv = s[e];
  s[e] = 1;
  return !rv;
}

/* Add every element of s2 to s1.  Return TRUE if s1 changes. */
int SetUnion(char *s1, char *s2)
{
  int i, progress;
  progress = 0;
  for(i=0; i<size; i++){
    if( s2[i]==0 ) continue;
    if( s1[i]==0 ){
      progress = 1;
      s1[i] = 1;
    }
  }
  return progress;
}
/********************** From the file "table.c" ****************************/
/*
** All code in this file has been automatically generated
** from a specification in the file
**              "table.q"
** by the associative array code building program "aagen".
** Do not edit this file!  Instead, edit the specification
** file, then rerun aagen.
*/
/*
** Code for processing tables in the LEMON parser generator.
*/

PRIVATE unsigned strhash(const char *x)
{
  unsigned h = 0;
  while( *x ) h = h*13 + *(x++);
  return h;
}

/* Works like strdup, sort of.  Save a string in malloced memory, but
** keep strings in a table so that the same string is not in more
** than one place.
*/
const char *Strsafe(const char *y)
{
  const char *z;
  char *cpy;

  if( y==0 ) return 0;
  z = Strsafe_find(y);
  if( z==0 && (cpy=(char *)malloc( lemonStrlen(y)+1 ))!=0 ){
    lemon_strcpy(cpy,y);
    z = cpy;
    Strsafe_insert(z);
  }
  MemoryCheck(z);
  return z;
}

/* There is one instance of the following structure for each
** associative array of type "x1".
*/
struct s_x1 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x1node *tbl;  /* The data stored here */
  struct s_x1node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x1".
*/
typedef struct s_x1node {
  const char *data;        /* The data */
  struct s_x1node *next;   /* Next entry with the same hash */
  struct s_x1node **from;  /* Previous link */
} x1node;

/* There is only one instance of the array, which is the following */
static struct s_x1 *x1a;

/* Allocate a new associative array */
void Strsafe_init(void){
  if( x1a ) return;
  x1a = (struct s_x1*)malloc( sizeof(struct s_x1) );
  if( x1a ){
    x1a->size = 1024;
    x1a->count = 0;
    x1a->tbl = (x1node*)calloc(1024, sizeof(x1node) + sizeof(x1node*));
    if( x1a->tbl==0 ){
      free(x1a);
      x1a = 0;
    }else{
      int i;
      x1a->ht = (x1node**)&(x1a->tbl[1024]);
      for(i=0; i<1024; i++) x1a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Strsafe_insert(const char *data)
{
  x1node *np;
  unsigned h;
  unsigned ph;

  if( x1a==0 ) return 0;
  ph = strhash(data);
  h = ph & (x1a->size-1);
  np = x1a->ht[h];
  while( np ){
    if( strcmp(np->data,data)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x1a->count>=x1a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x1 array;
    array.size = arrSize = x1a->size*2;
    array.count = x1a->count;
    array.tbl = (x1node*)calloc(arrSize, sizeof(x1node) + sizeof(x1node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x1node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x1a->count; i++){
      x1node *oldnp, *newnp;
      oldnp = &(x1a->tbl[i]);
      h = strhash(oldnp->data) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x1a->tbl);
    *x1a = array;
  }
  /* Insert the new data */
  h = ph & (x1a->size-1);
  np = &(x1a->tbl[x1a->count++]);
  np->data = data;
  if( x1a->ht[h] ) x1a->ht[h]->from = &(np->next);
  np->next = x1a->ht[h];
  x1a->ht[h] = np;
  np->from = &(x1a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
const char *Strsafe_find(const char *key)
{
  unsigned h;
  x1node *np;

  if( x1a==0 ) return 0;
  h = strhash(key) & (x1a->size-1);
  np = x1a->ht[h];
  while( np ){
    if( strcmp(np->data,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}


/* Compare two configurations */
int Configcmp(const char *_a,const char *_b)
{
  const struct config *a = (struct config *) _a;
  const struct config *b = (struct config *) _b;
  int x;
  x = a->rp->index - b->rp->index;
  if( x==0 ) x = a->dot - b->dot;
  return x;
}

/* Compare two states */
PRIVATE int statecmp(struct config *a, struct config *b)
{
  int rc;
  for(rc=0; rc==0 && a && b;  a=a->bp, b=b->bp){
    rc = a->rp->index - b->rp->index;
    if( rc==0 ) rc = a->dot - b->dot;
  }
  if( rc==0 ){
    if( a ) rc = 1;
    if( b ) rc = -1;
  }
  return rc;
}

/* Hash a state */
PRIVATE unsigned statehash(struct config *a)
{
  unsigned h=0;
  while( a ){
    h = h*571 + a->rp->index*37 + a->dot;
    a = a->bp;
  }
  return h;
}

/* Allocate a new state structure */
struct state *State_new()
{
  struct state *newstate;
  newstate = (struct state *)calloc(1, sizeof(struct state) );
  MemoryCheck(newstate);
  return newstate;
}

/* There is one instance of the following structure for each
** associative array of type "x3".
*/
struct s_x3 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x3node *tbl;  /* The data stored here */
  struct s_x3node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x3".
*/
typedef struct s_x3node {
  struct state *data;                  /* The data */
  struct config *key;                   /* The key */
  struct s_x3node *next;   /* Next entry with the same hash */
  struct s_x3node **from;  /* Previous link */
} x3node;

/* There is only one instance of the array, which is the following */
static struct s_x3 *x3a;

/* Allocate a new associative array */
void State_init(void){
  if( x3a ) return;
  x3a = (struct s_x3*)malloc( sizeof(struct s_x3) );
  if( x3a ){
    x3a->size = 128;
    x3a->count = 0;
    x3a->tbl = (x3node*)calloc(128, sizeof(x3node) + sizeof(x3node*));
    if( x3a->tbl==0 ){
      free(x3a);
      x3a = 0;
    }else{
      int i;
      x3a->ht = (x3node**)&(x3a->tbl[128]);
      for(i=0; i<128; i++) x3a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int State_insert(struct state *data, struct config *key)
{
  x3node *np;
  unsigned h;
  unsigned ph;

  if( x3a==0 ) return 0;
  ph = statehash(key);
  h = ph & (x3a->size-1);
  np = x3a->ht[h];
  while( np ){
    if( statecmp(np->key,key)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x3a->count>=x3a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x3 array;
    array.size = arrSize = x3a->size*2;
    array.count = x3a->count;
    array.tbl = (x3node*)calloc(arrSize, sizeof(x3node) + sizeof(x3node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x3node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x3a->count; i++){
      x3node *oldnp, *newnp;
      oldnp = &(x3a->tbl[i]);
      h = statehash(oldnp->key) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->key = oldnp->key;
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x3a->tbl);
    *x3a = array;
  }
  /* Insert the new data */
  h = ph & (x3a->size-1);
  np = &(x3a->tbl[x3a->count++]);
  np->key = key;
  np->data = data;
  if( x3a->ht[h] ) x3a->ht[h]->from = &(np->next);
  np->next = x3a->ht[h];
  x3a->ht[h] = np;
  np->from = &(x3a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct state *State_find(struct config *key)
{
  unsigned h;
  x3node *np;

  if( x3a==0 ) return 0;
  h = statehash(key) & (x3a->size-1);
  np = x3a->ht[h];
  while( np ){
    if( statecmp(np->key,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return an array of pointers to all data in the table.
** The array is obtained from malloc.  Return NULL if memory allocation
** problems, or if the array is empty. */
struct state **State_arrayof(void)
{
  struct state **array;
  int i,arrSize;
  if( x3a==0 ) return 0;
  arrSize = x3a->count;
  array = (struct state **)calloc(arrSize, sizeof(struct state *));
  if( array ){
    for(i=0; i<arrSize; i++) array[i] = x3a->tbl[i].data;
  }
  return array;
}

/* Hash a configuration */
PRIVATE unsigned confighash(struct config *a)
{
  unsigned h=0;
  h = h*571 + a->rp->index*37 + a->dot;
  return h;
}

/* There is one instance of the following structure for each
** associative array of type "x4".
*/
struct s_x4 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x4node *tbl;  /* The data stored here */
  struct s_x4node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x4".
*/
typedef struct s_x4node {
  struct config *data;                  /* The data */
  struct s_x4node *next;   /* Next entry with the same hash */
  struct s_x4node **from;  /* Previous link */
} x4node;

/* There is only one instance of the array, which is the following */
static struct s_x4 *x4a;

/* Allocate a new associative array */
void Configtable_init(void){
  if( x4a ) return;
  x4a = (struct s_x4*)malloc( sizeof(struct s_x4) );
  if( x4a ){
    x4a->size = 64;
    x4a->count = 0;
    x4a->tbl = (x4node*)calloc(64, sizeof(x4node) + sizeof(x4node*));
    if( x4a->tbl==0 ){
      free(x4a);
      x4a = 0;
    }else{
      int i;
      x4a->ht = (x4node**)&(x4a->tbl[64]);
      for(i=0; i<64; i++) x4a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Configtable_insert(struct config *data)
{
  x4node *np;
  unsigned h;
  unsigned ph;

  if( x4a==0 ) return 0;
  ph = confighash(data);
  h = ph & (x4a->size-1);
  np = x4a->ht[h];
  while( np ){
    if( Configcmp((const char *) np->data,(const char *) data)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x4a->count>=x4a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x4 array;
    array.size = arrSize = x4a->size*2;
    array.count = x4a->count;
    array.tbl = (x4node*)calloc(arrSize, sizeof(x4node) + sizeof(x4node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x4node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x4a->count; i++){
      x4node *oldnp, *newnp;
      oldnp = &(x4a->tbl[i]);
      h = confighash(oldnp->data) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x4a->tbl);
    *x4a = array;
  }
  /* Insert the new data */
  h = ph & (x4a->size-1);
  np = &(x4a->tbl[x4a->count++]);
  np->data = data;
  if( x4a->ht[h] ) x4a->ht[h]->from = &(np->next);
  np->next = x4a->ht[h];
  x4a->ht[h] = np;
  np->from = &(x4a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct config *Configtable_find(struct config *key)
{
  int h;
  x4node *np;

  if( x4a==0 ) return 0;
  h = confighash(key) & (x4a->size-1);
  np = x4a->ht[h];
  while( np ){
    if( Configcmp((const char *) np->data,(const char *) key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Remove all data from the table.  Pass each data to the function "f"
** as it is removed.  ("f" may be null to avoid this step.) */
void Configtable_clear(int(*f)(struct config *))
{
  int i;
  if( x4a==0 || x4a->count==0 ) return;
  if( f ) for(i=0; i<x4a->count; i++) (*f)(x4a->tbl[i].data);
  for(i=0; i<x4a->size; i++) x4a->ht[i] = 0;
  x4a->count = 0;
  return;
}
