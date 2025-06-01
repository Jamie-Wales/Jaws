/* Generated from tailrecursive.scm by the CHICKEN compiler
   http://www.call-cc.org
   Version 5.4.0 (rev 1a1d1495)
   macosx-unix-clang-arm64 [ 64bit dload ptables ]
   command line: tailrecursive.scm
   uses: eval library
*/
#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word *av) C_noret;

static C_TLS C_word lf[3];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,13),40,99,111,117,110,116,100,111,119,110,32,110,41,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_123)
static void C_ccall f_123(C_word c,C_word *av) C_noret;
C_noret_decl(f_126)
static void C_ccall f_126(C_word c,C_word *av) C_noret;
C_noret_decl(f_128)
static void C_ccall f_128(C_word c,C_word *av) C_noret;
C_noret_decl(f_138)
static void C_ccall f_138(C_word c,C_word *av) C_noret;
C_noret_decl(f_148)
static void C_ccall f_148(C_word c,C_word *av) C_noret;
C_noret_decl(f_154)
static void C_ccall f_154(C_word c,C_word *av) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word *av) C_noret;

/* k121 */
static void C_ccall f_123(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_123,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_126,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_eval_toplevel(2,av2);}}

/* k124 in k121 */
static void C_ccall f_126(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_126,c,av);}
a=C_alloc(6);
t2=C_mutate((C_word*)lf[0]+1 /* (set! countdown ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_128,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_148,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("tailrecursive.scm:7: countdown"));
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[0]+1));
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[0]+1);
av2[1]=t3;
av2[2]=C_fix(0);
tp(3,av2);}}

/* countdown in k124 in k121 */
static void C_ccall f_128(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_128,c,av);}
a=C_alloc(4);
if(C_truep(C_i_nequalp(t2,C_fix(100000)))){
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_138,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("tailrecursive.scm:4: scheme#display"));
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[1]+1));
C_word *av2=av;
av2[0]=*((C_word*)lf[1]+1);
av2[1]=t3;
av2[2]=t2;
tp(3,av2);}}}

/* k136 in countdown in k124 in k121 */
static void C_ccall f_138(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(29,c,2)))){
C_save_and_reclaim((void *)f_138,c,av);}
a=C_alloc(29);
t2=C_s_a_i_plus(&a,2,((C_word*)t0)[2],C_fix(1));
C_trace(C_text("tailrecursive.scm:4: countdown"));
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[0]+1));
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[0]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t2;
tp(3,av2);}}

/* k146 in k124 in k121 */
static void C_ccall f_148(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_148,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_154,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("chicken.base#implicit-exit-handler"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[2]);
C_word *av2=av;
av2[0]=*((C_word*)lf[2]+1);
av2[1]=t2;
tp(2,av2);}}

/* k152 in k146 in k124 in k121 */
static void C_ccall f_154(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_154,c,av);}
t2=t1;{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
((C_proc)C_fast_retrieve_proc(t2))(2,av2);}}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point

void C_ccall C_toplevel(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) {C_kontinue(t1,C_SCHEME_UNDEFINED);}
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(C_calculate_demand(3,c,2));
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void*)C_toplevel,c,av);}
toplevel_initialized=1;
if(C_unlikely(!C_demand_2(21))){
C_save(t1);
C_rereclaim2(21*sizeof(C_word),1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,3);
lf[0]=C_h_intern(&lf[0],9, C_text("countdown"));
lf[1]=C_h_intern(&lf[1],14, C_text("scheme#display"));
lf[2]=C_h_intern(&lf[2],34, C_text("chicken.base#implicit-exit-handler"));
C_register_lf2(lf,3,create_ptable());{}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_123,a[2]=t1,tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_library_toplevel(2,av2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[8] = {
{C_text("f_123:tailrecursive_2escm"),(void*)f_123},
{C_text("f_126:tailrecursive_2escm"),(void*)f_126},
{C_text("f_128:tailrecursive_2escm"),(void*)f_128},
{C_text("f_138:tailrecursive_2escm"),(void*)f_138},
{C_text("f_148:tailrecursive_2escm"),(void*)f_148},
{C_text("f_154:tailrecursive_2escm"),(void*)f_154},
{C_text("toplevel:tailrecursive_2escm"),(void*)C_toplevel},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
o|safe globals: (countdown) 
o|replaced variables: 5 
o|removed binding forms: 5 
o|removed binding forms: 4 
o|simplifications: ((##core#call . 2)) 
o|  call simplifications:
o|    scheme#=
o|    scheme#+
o|contracted procedure: k133 
o|contracted procedure: k143 
o|simplifications: ((let . 1)) 
o|removed binding forms: 2 
*/
/* end of file */
