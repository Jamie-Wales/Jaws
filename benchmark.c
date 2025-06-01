/* Generated from benchmark.scm by the CHICKEN compiler
   http://www.call-cc.org
   Version 5.4.0 (rev 1a1d1495)
   macosx-unix-clang-arm64 [ 64bit dload ptables ]
   command line: benchmark.scm
   uses: eval library
*/
#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word *av) C_noret;

static C_TLS C_word lf[2];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,12),40,108,111,111,112,32,105,32,115,117,109,41,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,16),40,99,108,111,115,117,114,101,45,116,101,115,116,32,110,41};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_130)
static void C_ccall f_130(C_word c,C_word *av) C_noret;
C_noret_decl(f_133)
static void C_ccall f_133(C_word c,C_word *av) C_noret;
C_noret_decl(f_135)
static void C_ccall f_135(C_word c,C_word *av) C_noret;
C_noret_decl(f_151)
static void C_fcall f_151(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_176)
static void C_ccall f_176(C_word c,C_word *av) C_noret;
C_noret_decl(f_182)
static void C_ccall f_182(C_word c,C_word *av) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word *av) C_noret;

C_noret_decl(trf_151)
static void C_ccall trf_151(C_word c,C_word *av) C_noret;
static void C_ccall trf_151(C_word c,C_word *av){
C_word t0=av[3];
C_word t1=av[2];
C_word t2=av[1];
C_word t3=av[0];
f_151(t0,t1,t2,t3);}

/* k128 */
static void C_ccall f_130(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_130,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_133,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_eval_toplevel(2,av2);}}

/* k131 in k128 */
static void C_ccall f_133(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_133,c,av);}
a=C_alloc(6);
t2=C_mutate((C_word*)lf[0]+1 /* (set! closure-test ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_135,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_176,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("benchmark.scm:10: closure-test"));
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[0]+1));
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[0]+1);
av2[1]=t3;
av2[2]=C_fix(10000000);
tp(3,av2);}}

/* closure-test in k131 in k128 */
static void C_ccall f_135(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,4)))){
C_save_and_reclaim((void *)f_135,c,av);}
a=C_alloc(7);
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_151,a[2]=t2,a[3]=t4,a[4]=((C_word)li0),tmp=(C_word)a,a+=5,tmp));
t6=((C_word*)t4)[1];
f_151(t6,t1,C_fix(0),C_fix(0));}

/* loop in closure-test in k131 in k128 */
static void C_fcall f_151(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
loop:
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(87,0,3)))){
C_save_and_reclaim_args((void *)trf_151,4,t0,t1,t2,t3);}
a=C_alloc(87);
if(C_truep(C_i_lessp(t2,((C_word*)t0)[2]))){
t4=C_s_a_i_plus(&a,2,t2,C_fix(1));
t5=C_s_a_i_plus(&a,2,C_fix(1),t2);
t6=C_s_a_i_plus(&a,2,t3,t5);
C_trace(C_text("benchmark.scm:6: loop"));
t8=t1;
t9=t4;
t10=t6;
t1=t8;
t2=t9;
t3=t10;
goto loop;}
else{
t4=t1;{
C_word av2[2];
av2[0]=t4;
av2[1]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}}

/* k174 in k131 in k128 */
static void C_ccall f_176(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_176,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_182,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("chicken.base#implicit-exit-handler"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[1]);
C_word *av2=av;
av2[0]=*((C_word*)lf[1]+1);
av2[1]=t2;
tp(2,av2);}}

/* k180 in k174 in k131 in k128 */
static void C_ccall f_182(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_182,c,av);}
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
if(C_unlikely(!C_demand_2(14))){
C_save(t1);
C_rereclaim2(14*sizeof(C_word),1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,2);
lf[0]=C_h_intern(&lf[0],12, C_text("closure-test"));
lf[1]=C_h_intern(&lf[1],34, C_text("chicken.base#implicit-exit-handler"));
C_register_lf2(lf,2,create_ptable());{}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_130,a[2]=t1,tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_library_toplevel(2,av2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[8] = {
{C_text("f_130:benchmark_2escm"),(void*)f_130},
{C_text("f_133:benchmark_2escm"),(void*)f_133},
{C_text("f_135:benchmark_2escm"),(void*)f_135},
{C_text("f_151:benchmark_2escm"),(void*)f_151},
{C_text("f_176:benchmark_2escm"),(void*)f_176},
{C_text("f_182:benchmark_2escm"),(void*)f_182},
{C_text("toplevel:benchmark_2escm"),(void*)C_toplevel},
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
o|safe globals: (closure-test) 
o|contracted procedure: "(benchmark.scm:3) make-adder11" 
o|replaced variables: 10 
o|removed binding forms: 6 
o|substituted constant variable: x12 
o|replaced variables: 1 
o|removed binding forms: 9 
o|removed binding forms: 2 
o|contracted procedure: k144 
o|removed binding forms: 1 
o|contracted procedure: "(benchmark.scm:6) r145" 
o|removed binding forms: 1 
o|replaced variables: 2 
o|removed binding forms: 2 
o|simplifications: ((##core#call . 4)) 
o|  call simplifications:
o|    scheme#<
o|    scheme#+	3
o|contracted procedure: k156 
o|contracted procedure: k163 
o|contracted procedure: k171 
o|contracted procedure: k167 
o|simplifications: ((let . 1)) 
o|removed binding forms: 4 
o|customizable procedures: (loop15) 
o|calls to known targets: 2 
o|identified direct recursive calls: f_151 1 
o|fast box initializations: 1 
*/
/* end of file */
