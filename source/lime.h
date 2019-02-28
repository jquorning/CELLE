/*
**  The author disclaims copyright to this source code.  In place of
**  a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, not taking more than you give.
**
*/

#ifndef __LIME_H__
#define __LIME_H__



#if 1
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
#endif



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
lime_partial_database_dump_ada (void);
//
//

struct rule*
lime_rule_sort (struct rule*);
//
//

#endif /* __LIME_H__ */
