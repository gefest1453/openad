#include "targ_isa_bundle.h"


const ISA_BUNDLE_INFO ISA_BUNDLE_info[] = {
 {
    "mii",           ".mii",     3,
    {  4 /*  M_Unit */,  2 /*  I_Unit */,  2 /*  I_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, ISA_EXEC_I_Unit, },
     0, 0x0, 0x004002002
  },
 {
    "mii_",          ".mii",     3,
    {  4 /*  M_Unit */,  2 /*  I_Unit */,  2 /*  I_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, ISA_EXEC_I_Unit, },
     1, 0x1, 0x004002002
  },
 {
    "mi_i",          ".mii",     3,
    {  4 /*  M_Unit */,  2 /*  I_Unit */,  2 /*  I_Unit */, },
    { FALSE,  TRUE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, ISA_EXEC_I_Unit, },
     2, 0x2, 0x004002002
  },
 {
    "mi_i_",         ".mii",     3,
    {  4 /*  M_Unit */,  2 /*  I_Unit */,  2 /*  I_Unit */, },
    { FALSE,  TRUE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, ISA_EXEC_I_Unit, },
     3, 0x3, 0x004002002
  },
 {
    "mlx",           ".mlx",     3,
    {  4 /*  M_Unit */, 32 /*  L_Unit */, 32 /*  L_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_L_Unit, ISA_EXEC_L_Unit, },
     4, 0x0, 0x004020020
  },
 {
    "mlx_",          ".mlx",     3,
    {  4 /*  M_Unit */, 32 /*  L_Unit */, 32 /*  L_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_L_Unit, ISA_EXEC_L_Unit, },
     5, 0x1, 0x004020020
  },
 {
    "reserved_06",   ".0x06",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
     6, 0x0, 0x001001001
  },
 {
    "reserved_07",   ".0x07",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
     7, 0x0, 0x001001001
  },
 {
    "mmi",           ".mmi",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */,  2 /*  I_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, },
     8, 0x0, 0x004004002
  },
 {
    "mmi_",          ".mmi",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */,  2 /*  I_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, },
     9, 0x1, 0x004004002
  },
 {
    "m_mi",          ".mmi",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */,  2 /*  I_Unit */, },
    {  TRUE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, },
    10, 0x4, 0x004004002
  },
 {
    "m_mi_",         ".mmi",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */,  2 /*  I_Unit */, },
    {  TRUE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, },
    11, 0x5, 0x004004002
  },
 {
    "mfi",           ".mfi",     3,
    {  4 /*  M_Unit */, 16 /*  F_Unit */,  2 /*  I_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_F_Unit, ISA_EXEC_I_Unit, },
    12, 0x0, 0x004010002
  },
 {
    "mfi_",          ".mfi",     3,
    {  4 /*  M_Unit */, 16 /*  F_Unit */,  2 /*  I_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_F_Unit, ISA_EXEC_I_Unit, },
    13, 0x1, 0x004010002
  },
 {
    "mmf",           ".mmf",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */, 16 /*  F_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_F_Unit, },
    14, 0x0, 0x004004010
  },
 {
    "mmf_",          ".mmf",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */, 16 /*  F_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_F_Unit, },
    15, 0x1, 0x004004010
  },
 {
    "mib",           ".mib",     3,
    {  4 /*  M_Unit */,  2 /*  I_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, ISA_EXEC_B_Unit, },
    16, 0x0, 0x004002080
  },
 {
    "mib_",          ".mib",     3,
    {  4 /*  M_Unit */,  2 /*  I_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_I_Unit, ISA_EXEC_B_Unit, },
    17, 0x1, 0x004002080
  },
 {
    "mbb",           ".mbb",     3,
    {  4 /*  M_Unit */,  8 /*  B_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_B_Unit, ISA_EXEC_B_Unit, },
    18, 0x0, 0x004008080
  },
 {
    "mbb_",          ".mbb",     3,
    {  4 /*  M_Unit */,  8 /*  B_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_B_Unit, ISA_EXEC_B_Unit, },
    19, 0x1, 0x004008080
  },
 {
    "reserved_14",   ".0x14",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
    20, 0x0, 0x001001001
  },
 {
    "reserved_15",   ".0x15",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
    21, 0x0, 0x001001001
  },
 {
    "bbb",           ".bbb",     3,
    {  8 /*  B_Unit */,  8 /*  B_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_B_Unit, ISA_EXEC_B_Unit, ISA_EXEC_B_Unit, },
    22, 0x0, 0x008008080
  },
 {
    "bbb_",          ".bbb",     3,
    {  8 /*  B_Unit */,  8 /*  B_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_B_Unit, ISA_EXEC_B_Unit, ISA_EXEC_B_Unit, },
    23, 0x1, 0x008008080
  },
 {
    "mmb",           ".mmb",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_B_Unit, },
    24, 0x0, 0x004004080
  },
 {
    "mmb_",          ".mmb",     3,
    {  4 /*  M_Unit */,  4 /*  M_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_M_Unit, ISA_EXEC_B_Unit, },
    25, 0x1, 0x004004080
  },
 {
    "reserved_1a",   ".0x1a",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
    26, 0x0, 0x001001001
  },
 {
    "reserved_1b",   ".0x1b",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
    27, 0x0, 0x001001001
  },
 {
    "mfb",           ".mfb",     3,
    {  4 /*  M_Unit */, 16 /*  F_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_F_Unit, ISA_EXEC_B_Unit, },
    28, 0x0, 0x004010080
  },
 {
    "mfb_",          ".mfb",     3,
    {  4 /*  M_Unit */, 16 /*  F_Unit */, 128 /* B2_Unit */, },
    { FALSE, FALSE,  TRUE, },
    { ISA_EXEC_M_Unit, ISA_EXEC_F_Unit, ISA_EXEC_B_Unit, },
    29, 0x1, 0x004010080
  },
 {
    "reserved_1e",   ".0x1e",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
    30, 0x0, 0x001001001
  },
 {
    "reserved_1f",   ".0x1f",    3,
    {  1 /*  R_Unit */,  1 /*  R_Unit */,  1 /*  R_Unit */, },
    { FALSE, FALSE, FALSE, },
    { ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, ISA_EXEC_R_Unit, },
    31, 0x0, 0x001001001
  },
  {
    "template_MAX", "", -1,
    { -1 /* ??????? */, -1 /* ??????? */, -1 /* ??????? */,},
    { FALSE, FALSE, FALSE,},
    -1, 0x0, 0x000000000
  }
};

const ISA_EXEC_UNIT_PROPERTY ISA_EXEC_unit_prop[759] = {
    6,  /* add:  I_Unit M_Unit */
    6,  /* add_1:  I_Unit M_Unit */
    6,  /* adds:  I_Unit M_Unit */
    6,  /* addl:  I_Unit M_Unit */
    6,  /* addp4:  I_Unit M_Unit */
    6,  /* addp4_i:  I_Unit M_Unit */
    4,  /* alloc_3:  M_Unit */
    6,  /* and:  I_Unit M_Unit */
    6,  /* and_i:  I_Unit M_Unit */
    6,  /* andcm:  I_Unit M_Unit */
    6,  /* andcm_i:  I_Unit M_Unit */
  136,  /* br.cond:  B_Unit B2_Unit */
  136,  /* br_r.cond:  B_Unit B2_Unit */
  136,  /* br.call:  B_Unit B2_Unit */
  136,  /* br_r.call:  B_Unit B2_Unit */
  136,  /* br.ret:  B_Unit B2_Unit */
  136,  /* br.ia:  B_Unit B2_Unit */
  128,  /* br.cloop:  B2_Unit */
  128,  /* br.ctop:  B2_Unit */
  128,  /* br.cexit:  B2_Unit */
  128,  /* br.wtop:  B2_Unit */
  128,  /* br.wexit:  B2_Unit */
   66,  /* break.i:  I_Unit I2_Unit */
  136,  /* break.b:  B_Unit B2_Unit */
    4,  /* break.m:  M_Unit */
   16,  /* break.f:  F_Unit */
   96,  /* break.x:  L_Unit I2_Unit */
    0,  /* brl.cond:  */
    0,  /* brl.call:  */
  128,  /* brp:  B2_Unit */
  128,  /* brp_r:  B2_Unit */
  128,  /* brp.ret:  B2_Unit */
  136,  /* bsw.0:  B_Unit B2_Unit */
  136,  /* bsw.1:  B_Unit B2_Unit */
    2,  /* chk.s.i:  I_Unit */
    4,  /* chk.s.m:  M_Unit */
    4,  /* chk_f.s:  M_Unit */
    4,  /* chk.a:  M_Unit */
    4,  /* chk_f.a:  M_Unit */
  136,  /* clrrrb:  B_Unit B2_Unit */
  136,  /* clrrrb.pr:  B_Unit B2_Unit */
    6,  /* cmp.eq:  I_Unit M_Unit */
    6,  /* cmp.eq.unc:  I_Unit M_Unit */
    6,  /* cmp.eq.and:  I_Unit M_Unit */
    6,  /* cmp.eq.or:  I_Unit M_Unit */
    6,  /* cmp.eq.or.andcm:  I_Unit M_Unit */
    6,  /* cmp.ne.and:  I_Unit M_Unit */
    6,  /* cmp.ne.or:  I_Unit M_Unit */
    6,  /* cmp.ne.or.andcm:  I_Unit M_Unit */
    6,  /* cmp.lt:  I_Unit M_Unit */
    6,  /* cmp.lt.unc:  I_Unit M_Unit */
    6,  /* cmp.ltu:  I_Unit M_Unit */
    6,  /* cmp.ltu.unc:  I_Unit M_Unit */
    6,  /* cmp_z1.lt.and:  I_Unit M_Unit */
    6,  /* cmp_z1.lt.or:  I_Unit M_Unit */
    6,  /* cmp_z1.lt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.le.and:  I_Unit M_Unit */
    6,  /* cmp_z1.le.or:  I_Unit M_Unit */
    6,  /* cmp_z1.le.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.gt.and:  I_Unit M_Unit */
    6,  /* cmp_z1.gt.or:  I_Unit M_Unit */
    6,  /* cmp_z1.gt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.ge.and:  I_Unit M_Unit */
    6,  /* cmp_z1.ge.or:  I_Unit M_Unit */
    6,  /* cmp_z1.ge.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_i.eq:  I_Unit M_Unit */
    6,  /* cmp_i.eq.unc:  I_Unit M_Unit */
    6,  /* cmp_i.eq.and:  I_Unit M_Unit */
    6,  /* cmp_i.eq.or:  I_Unit M_Unit */
    6,  /* cmp_i.eq.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_i.ne.and:  I_Unit M_Unit */
    6,  /* cmp_i.ne.or:  I_Unit M_Unit */
    6,  /* cmp_i.ne.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_i.lt:  I_Unit M_Unit */
    6,  /* cmp_i.lt.unc:  I_Unit M_Unit */
    6,  /* cmp_i.ltu:  I_Unit M_Unit */
    6,  /* cmp_i.ltu.unc:  I_Unit M_Unit */
    6,  /* cmp4.eq:  I_Unit M_Unit */
    6,  /* cmp4.eq.unc:  I_Unit M_Unit */
    6,  /* cmp4.eq.and:  I_Unit M_Unit */
    6,  /* cmp4.eq.or:  I_Unit M_Unit */
    6,  /* cmp4.eq.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4.ne.and:  I_Unit M_Unit */
    6,  /* cmp4.ne.or:  I_Unit M_Unit */
    6,  /* cmp4.ne.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4.lt:  I_Unit M_Unit */
    6,  /* cmp4.lt.unc:  I_Unit M_Unit */
    6,  /* cmp4.ltu:  I_Unit M_Unit */
    6,  /* cmp4.ltu.unc:  I_Unit M_Unit */
    6,  /* cmp4_z1.lt.and:  I_Unit M_Unit */
    6,  /* cmp4_z1.lt.or:  I_Unit M_Unit */
    6,  /* cmp4_z1.lt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.le.and:  I_Unit M_Unit */
    6,  /* cmp4_z1.le.or:  I_Unit M_Unit */
    6,  /* cmp4_z1.le.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.gt.and:  I_Unit M_Unit */
    6,  /* cmp4_z1.gt.or:  I_Unit M_Unit */
    6,  /* cmp4_z1.gt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.ge.and:  I_Unit M_Unit */
    6,  /* cmp4_z1.ge.or:  I_Unit M_Unit */
    6,  /* cmp4_z1.ge.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_i.eq:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.and:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.or:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.and:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.or:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_i.lt:  I_Unit M_Unit */
    6,  /* cmp4_i.lt.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.ltu:  I_Unit M_Unit */
    6,  /* cmp4_i.ltu.unc:  I_Unit M_Unit */
    4,  /* cmpxchg1:  M_Unit */
    4,  /* cmpxchg2:  M_Unit */
    4,  /* cmpxchg4:  M_Unit */
    4,  /* cmpxchg8:  M_Unit */
  136,  /* cover:  B_Unit B2_Unit */
    2,  /* czx1.l:  I_Unit */
    2,  /* czx1.r:  I_Unit */
    2,  /* czx2.l:  I_Unit */
    2,  /* czx2.r:  I_Unit */
    2,  /* dep:  I_Unit */
    2,  /* dep.z:  I_Unit */
    2,  /* dep_i:  I_Unit */
    2,  /* dep_i.z:  I_Unit */
  136,  /* epc:  B_Unit B2_Unit */
    2,  /* extr:  I_Unit */
    2,  /* extr.u:  I_Unit */
   16,  /* famax:  F_Unit */
   16,  /* famin:  F_Unit */
   16,  /* fand:  F_Unit */
   16,  /* fandcm:  F_Unit */
    4,  /* fc:  M_Unit */
   16,  /* fchkf:  F_Unit */
   16,  /* fclass.m:  F_Unit */
   16,  /* fclass.m.unc:  F_Unit */
   16,  /* fclrf:  F_Unit */
   16,  /* fcmp.eq:  F_Unit */
   16,  /* fcmp.eq.unc:  F_Unit */
   16,  /* fcmp.lt:  F_Unit */
   16,  /* fcmp.lt.unc:  F_Unit */
   16,  /* fcmp.le:  F_Unit */
   16,  /* fcmp.le.unc:  F_Unit */
   16,  /* fcmp.unord:  F_Unit */
   16,  /* fcmp.unord.unc:  F_Unit */
   16,  /* fcvt.fx:  F_Unit */
   16,  /* fcvt.fx.trunc:  F_Unit */
   16,  /* fcvt.fxu:  F_Unit */
   16,  /* fcvt.fxu.trunc:  F_Unit */
   16,  /* fcvt.xf:  F_Unit */
    4,  /* fetchadd4:  M_Unit */
    4,  /* fetchadd8:  M_Unit */
    4,  /* flushrs:  M_Unit */
   16,  /* fma:  F_Unit */
   16,  /* fma.s:  F_Unit */
   16,  /* fma.d:  F_Unit */
   16,  /* fmax:  F_Unit */
   16,  /* fmerge.ns:  F_Unit */
   16,  /* fmerge.s:  F_Unit */
   16,  /* fmerge.se:  F_Unit */
   16,  /* fmin:  F_Unit */
   16,  /* fmix.l:  F_Unit */
   16,  /* fmix.r:  F_Unit */
   16,  /* fmix.lr:  F_Unit */
   16,  /* fms:  F_Unit */
   16,  /* fms.s:  F_Unit */
   16,  /* fms.d:  F_Unit */
   16,  /* fnma:  F_Unit */
   16,  /* fnma.s:  F_Unit */
   16,  /* fnma.d:  F_Unit */
   16,  /* for:  F_Unit */
   16,  /* fpack:  F_Unit */
   16,  /* fpamax:  F_Unit */
   16,  /* fpamin:  F_Unit */
   16,  /* fpcmp.eq:  F_Unit */
   16,  /* fpcmp.lt:  F_Unit */
   16,  /* fpcmp.le:  F_Unit */
   16,  /* fpcmp.unord:  F_Unit */
   16,  /* fpcmp.neq:  F_Unit */
   16,  /* fpcmp.nlt:  F_Unit */
   16,  /* fpcmp.nle:  F_Unit */
   16,  /* fpcmp.ord:  F_Unit */
   16,  /* fpcvt.fx:  F_Unit */
   16,  /* fpcvt.fx.trunc:  F_Unit */
   16,  /* fpcvt.fxu:  F_Unit */
   16,  /* fpcvt.fxu.trunc:  F_Unit */
   16,  /* fpma:  F_Unit */
   16,  /* fpmax:  F_Unit */
   16,  /* fpmerge.ns:  F_Unit */
   16,  /* fpmerge.s:  F_Unit */
   16,  /* fpmerge.se:  F_Unit */
   16,  /* fpmin:  F_Unit */
   16,  /* fpms:  F_Unit */
   16,  /* fpnma:  F_Unit */
   16,  /* fprcpa:  F_Unit */
   16,  /* fprsqrta:  F_Unit */
   16,  /* frcpa:  F_Unit */
   16,  /* frsqrta:  F_Unit */
   16,  /* fselect:  F_Unit */
   16,  /* fsetc:  F_Unit */
   16,  /* fswap:  F_Unit */
   16,  /* fswap.nl:  F_Unit */
   16,  /* fswap.nr:  F_Unit */
   16,  /* fsxt.l:  F_Unit */
   16,  /* fsxt.r:  F_Unit */
    4,  /* fwb:  M_Unit */
   16,  /* fxor:  F_Unit */
    4,  /* getf.s:  M_Unit */
    4,  /* getf.d:  M_Unit */
    4,  /* getf.exp:  M_Unit */
    4,  /* getf.sig:  M_Unit */
    4,  /* invala:  M_Unit */
    4,  /* invala.e:  M_Unit */
    4,  /* invala_f.e:  M_Unit */
    4,  /* itc.i:  M_Unit */
    4,  /* itc.d:  M_Unit */
    4,  /* itr.i:  M_Unit */
    4,  /* itr.d:  M_Unit */
    4,  /* ld1:  M_Unit */
    4,  /* ld1_r:  M_Unit */
    4,  /* ld1_i:  M_Unit */
    4,  /* ld2:  M_Unit */
    4,  /* ld2_r:  M_Unit */
    4,  /* ld2_i:  M_Unit */
    4,  /* ld4:  M_Unit */
    4,  /* ld4_r:  M_Unit */
    4,  /* ld4_i:  M_Unit */
    4,  /* ld8:  M_Unit */
    4,  /* ld8_r:  M_Unit */
    4,  /* ld8_i:  M_Unit */
    4,  /* ld8.fill:  M_Unit */
    4,  /* ld8_r.fill:  M_Unit */
    4,  /* ld8_i.fill:  M_Unit */
    4,  /* ldfs:  M_Unit */
    4,  /* ldfs_r:  M_Unit */
    4,  /* ldfs_i:  M_Unit */
    4,  /* ldfd:  M_Unit */
    4,  /* ldfd_r:  M_Unit */
    4,  /* ldfd_i:  M_Unit */
    4,  /* ldfe:  M_Unit */
    4,  /* ldfe_r:  M_Unit */
    4,  /* ldfe_i:  M_Unit */
    4,  /* ldf8:  M_Unit */
    4,  /* ldf8_r:  M_Unit */
    4,  /* ldf8_i:  M_Unit */
    4,  /* ldf.fill:  M_Unit */
    4,  /* ldf_r.fill:  M_Unit */
    4,  /* ldf_i.fill:  M_Unit */
    4,  /* ldfps:  M_Unit */
    4,  /* ldfps_i:  M_Unit */
    4,  /* ldfpd:  M_Unit */
    4,  /* ldfpd_i:  M_Unit */
    4,  /* ldfp8:  M_Unit */
    4,  /* ldfp8_i:  M_Unit */
    4,  /* lfetch:  M_Unit */
    4,  /* lfetch.excl:  M_Unit */
    4,  /* lfetch.fault:  M_Unit */
    4,  /* lfetch.fault.excl:  M_Unit */
    4,  /* lfetch_r:  M_Unit */
    4,  /* lfetch_r.excl:  M_Unit */
    4,  /* lfetch_r.fault:  M_Unit */
    4,  /* lfetch_r.fault.excl:  M_Unit */
    4,  /* lfetch_i:  M_Unit */
    4,  /* lfetch_i.excl:  M_Unit */
    4,  /* lfetch_i.fault:  M_Unit */
    4,  /* lfetch_i.fault.excl:  M_Unit */
    4,  /* loadrs:  M_Unit */
    4,  /* mf:  M_Unit */
    4,  /* mf.a:  M_Unit */
    2,  /* mix1.l:  I_Unit */
    2,  /* mix1.r:  I_Unit */
    2,  /* mix2.l:  I_Unit */
    2,  /* mix2.r:  I_Unit */
    2,  /* mix4.l:  I_Unit */
    2,  /* mix4.r:  I_Unit */
    2,  /* mov_f_ar.i:  I_Unit */
    2,  /* mov_t_ar_r.i:  I_Unit */
    2,  /* mov_t_ar_i.i:  I_Unit */
    4,  /* mov_f_ar.m:  M_Unit */
    4,  /* mov_t_ar_r.m:  M_Unit */
    4,  /* mov_t_ar_i.m:  M_Unit */
    2,  /* mov_f_br:  I_Unit */
    2,  /* mov_t_br_i:  I_Unit */
    2,  /* mov_t_br.ret:  I_Unit */
    4,  /* mov_t_cr:  M_Unit */
    4,  /* mov_f_cr:  M_Unit */
    4,  /* mov_f_cpuid:  M_Unit */
    4,  /* mov_t_dbr:  M_Unit */
    4,  /* mov_f_dbr:  M_Unit */
    4,  /* mov_t_ibr:  M_Unit */
    4,  /* mov_f_ibr:  M_Unit */
    4,  /* mov_t_msr:  M_Unit */
    4,  /* mov_f_msr:  M_Unit */
    4,  /* mov_t_pkr:  M_Unit */
    4,  /* mov_f_pkr:  M_Unit */
    4,  /* mov_t_pmc:  M_Unit */
    4,  /* mov_f_pmc:  M_Unit */
    4,  /* mov_t_pmd:  M_Unit */
    4,  /* mov_f_pmd:  M_Unit */
    4,  /* mov_t_rr:  M_Unit */
    4,  /* mov_f_rr:  M_Unit */
    2,  /* mov_f_ip:  I_Unit */
    2,  /* mov_f_pr:  I_Unit */
    2,  /* mov_t_pr:  I_Unit */
    2,  /* mov_t_pr_i:  I_Unit */
    4,  /* mov_t_psr:  M_Unit */
    4,  /* mov_f_psr:  M_Unit */
    4,  /* mov_t_psrum:  M_Unit */
    4,  /* mov_f_psrum:  M_Unit */
   96,  /* movl:  L_Unit I2_Unit */
    2,  /* mux1:  I_Unit */
    2,  /* mux2:  I_Unit */
   66,  /* nop.i:  I_Unit I2_Unit */
  136,  /* nop.b:  B_Unit B2_Unit */
    4,  /* nop.m:  M_Unit */
   16,  /* nop.f:  F_Unit */
   96,  /* nop.x:  L_Unit I2_Unit */
    6,  /* or:  I_Unit M_Unit */
    6,  /* or_i:  I_Unit M_Unit */
    2,  /* pack2.sss:  I_Unit */
    2,  /* pack2.uss:  I_Unit */
    2,  /* pack4.sss:  I_Unit */
    6,  /* padd1:  I_Unit M_Unit */
    6,  /* padd1.sss:  I_Unit M_Unit */
    6,  /* padd1.uus:  I_Unit M_Unit */
    6,  /* padd1.uuu:  I_Unit M_Unit */
    6,  /* padd2:  I_Unit M_Unit */
    6,  /* padd2.sss:  I_Unit M_Unit */
    6,  /* padd2.uus:  I_Unit M_Unit */
    6,  /* padd2.uuu:  I_Unit M_Unit */
    6,  /* padd4:  I_Unit M_Unit */
    6,  /* pavg1:  I_Unit M_Unit */
    6,  /* pavg1.raz:  I_Unit M_Unit */
    6,  /* pavg2:  I_Unit M_Unit */
    6,  /* pavg2.raz:  I_Unit M_Unit */
    6,  /* pavgsub1:  I_Unit M_Unit */
    6,  /* pavgsub2:  I_Unit M_Unit */
    6,  /* pcmp1.eq:  I_Unit M_Unit */
    6,  /* pcmp1.gt:  I_Unit M_Unit */
    6,  /* pcmp2.eq:  I_Unit M_Unit */
    6,  /* pcmp2.gt:  I_Unit M_Unit */
    6,  /* pcmp4.eq:  I_Unit M_Unit */
    6,  /* pcmp4.gt:  I_Unit M_Unit */
    2,  /* pmax1.u:  I_Unit */
    2,  /* pmax2:  I_Unit */
    2,  /* pmin1.u:  I_Unit */
    2,  /* pmin2:  I_Unit */
    2,  /* pmpy2.r:  I_Unit */
    2,  /* pmpy2.l:  I_Unit */
    2,  /* pmpyshr2:  I_Unit */
    2,  /* pmpyshr2.u:  I_Unit */
    2,  /* popcnt:  I_Unit */
    4,  /* probe.r:  M_Unit */
    4,  /* probe.w:  M_Unit */
    4,  /* probe_i.r:  M_Unit */
    4,  /* probe_i.w:  M_Unit */
    4,  /* probe.r.fault:  M_Unit */
    4,  /* probe.w.fault:  M_Unit */
    4,  /* probe.rw.fault:  M_Unit */
    2,  /* psad1:  I_Unit */
    2,  /* pshl2:  I_Unit */
    2,  /* pshl4:  I_Unit */
    2,  /* pshl2_i:  I_Unit */
    2,  /* pshl4_i:  I_Unit */
    6,  /* pshladd2:  I_Unit M_Unit */
    2,  /* pshr2:  I_Unit */
    2,  /* pshr2.u:  I_Unit */
    2,  /* pshr4:  I_Unit */
    2,  /* pshr4.u:  I_Unit */
    2,  /* pshr2_i:  I_Unit */
    2,  /* pshr2_i.u:  I_Unit */
    2,  /* pshr4_i:  I_Unit */
    2,  /* pshr4_i.u:  I_Unit */
    6,  /* pshradd2:  I_Unit M_Unit */
    6,  /* psub1:  I_Unit M_Unit */
    6,  /* psub2:  I_Unit M_Unit */
    6,  /* psub4:  I_Unit M_Unit */
    6,  /* psub1.sss:  I_Unit M_Unit */
    6,  /* psub1.uus:  I_Unit M_Unit */
    6,  /* psub1.uuu:  I_Unit M_Unit */
    6,  /* psub2.sss:  I_Unit M_Unit */
    6,  /* psub2.uus:  I_Unit M_Unit */
    6,  /* psub2.uuu:  I_Unit M_Unit */
    4,  /* ptc.e:  M_Unit */
    4,  /* ptc.g:  M_Unit */
    4,  /* ptc.ga:  M_Unit */
    4,  /* ptc.l:  M_Unit */
    4,  /* ptr.d:  M_Unit */
    4,  /* ptr.i:  M_Unit */
  136,  /* rfi:  B_Unit B2_Unit */
    4,  /* rsm:  M_Unit */
    4,  /* rum:  M_Unit */
    4,  /* setf.s:  M_Unit */
    4,  /* setf.d:  M_Unit */
    4,  /* setf.exp:  M_Unit */
    4,  /* setf.sig:  M_Unit */
    2,  /* shl:  I_Unit */
    6,  /* shladd:  I_Unit M_Unit */
    6,  /* shladdp4:  I_Unit M_Unit */
    2,  /* shr:  I_Unit */
    2,  /* shr.u:  I_Unit */
    2,  /* shrp:  I_Unit */
    4,  /* srlz.i:  M_Unit */
    4,  /* srlz.d:  M_Unit */
    4,  /* ssm:  M_Unit */
    4,  /* st1:  M_Unit */
    4,  /* st1_i:  M_Unit */
    4,  /* st2:  M_Unit */
    4,  /* st2_i:  M_Unit */
    4,  /* st4:  M_Unit */
    4,  /* st4_i:  M_Unit */
    4,  /* st8:  M_Unit */
    4,  /* st8_i:  M_Unit */
    4,  /* st8.spill:  M_Unit */
    4,  /* st8_i.spill:  M_Unit */
    4,  /* stfs:  M_Unit */
    4,  /* stfs_i:  M_Unit */
    4,  /* stfd:  M_Unit */
    4,  /* stfd_i:  M_Unit */
    4,  /* stfe:  M_Unit */
    4,  /* stfe_i:  M_Unit */
    4,  /* stf8:  M_Unit */
    4,  /* stf8_i:  M_Unit */
    4,  /* stf.spill:  M_Unit */
    4,  /* stf_i.spill:  M_Unit */
    6,  /* sub:  I_Unit M_Unit */
    6,  /* sub_1:  I_Unit M_Unit */
    6,  /* sub_i:  I_Unit M_Unit */
    4,  /* sum:  M_Unit */
    2,  /* sxt1:  I_Unit */
    2,  /* sxt2:  I_Unit */
    2,  /* sxt4:  I_Unit */
    4,  /* sync.i:  M_Unit */
    4,  /* tak:  M_Unit */
    2,  /* tbit.z:  I_Unit */
    2,  /* tbit.z.unc:  I_Unit */
    2,  /* tbit.z.and:  I_Unit */
    2,  /* tbit.z.or:  I_Unit */
    2,  /* tbit.z.or.andcm:  I_Unit */
    2,  /* tbit.nz.and:  I_Unit */
    2,  /* tbit.nz.or:  I_Unit */
    2,  /* tbit.nz.or.andcm:  I_Unit */
    4,  /* thash:  M_Unit */
    2,  /* tnat.z:  I_Unit */
    2,  /* tnat.z.unc:  I_Unit */
    2,  /* tnat.z.and:  I_Unit */
    2,  /* tnat.z.or:  I_Unit */
    2,  /* tnat.z.or.andcm:  I_Unit */
    2,  /* tnat.nz.and:  I_Unit */
    2,  /* tnat.nz.or:  I_Unit */
    2,  /* tnat.nz.or.andcm:  I_Unit */
    4,  /* tpa:  M_Unit */
    4,  /* ttag:  M_Unit */
    2,  /* unpack1.l:  I_Unit */
    2,  /* unpack1.h:  I_Unit */
    2,  /* unpack2.l:  I_Unit */
    2,  /* unpack2.h:  I_Unit */
    2,  /* unpack4.l:  I_Unit */
    2,  /* unpack4.h:  I_Unit */
    4,  /* xchg1:  M_Unit */
    4,  /* xchg2:  M_Unit */
    4,  /* xchg4:  M_Unit */
    4,  /* xchg8:  M_Unit */
   16,  /* xma.l:  F_Unit */
   16,  /* xma.h:  F_Unit */
   16,  /* xma.hu:  F_Unit */
    6,  /* xor:  I_Unit M_Unit */
    6,  /* xor_i:  I_Unit M_Unit */
    2,  /* zxt1:  I_Unit */
    2,  /* zxt2:  I_Unit */
    2,  /* zxt4:  I_Unit */
    4,  /* alloc:  M_Unit */
  136,  /* br:  B_Unit B2_Unit */
  136,  /* br_r:  B_Unit B2_Unit */
    0,  /* brl:  */
    6,  /* cmp.eq.orcm:  I_Unit M_Unit */
    6,  /* cmp.eq.andcm:  I_Unit M_Unit */
    6,  /* cmp.eq.and.orcm:  I_Unit M_Unit */
    6,  /* cmp.ne:  I_Unit M_Unit */
    6,  /* cmp.ne.unc:  I_Unit M_Unit */
    6,  /* cmp.ne.orcm:  I_Unit M_Unit */
    6,  /* cmp.ne.andcm:  I_Unit M_Unit */
    6,  /* cmp.ne.and.orcm:  I_Unit M_Unit */
    6,  /* cmp.le:  I_Unit M_Unit */
    6,  /* cmp.le.unc:  I_Unit M_Unit */
    6,  /* cmp.gt:  I_Unit M_Unit */
    6,  /* cmp.gt.unc:  I_Unit M_Unit */
    6,  /* cmp.ge:  I_Unit M_Unit */
    6,  /* cmp.ge.unc:  I_Unit M_Unit */
    6,  /* cmp.leu:  I_Unit M_Unit */
    6,  /* cmp.leu.unc:  I_Unit M_Unit */
    6,  /* cmp.gtu:  I_Unit M_Unit */
    6,  /* cmp.gtu.unc:  I_Unit M_Unit */
    6,  /* cmp.geu:  I_Unit M_Unit */
    6,  /* cmp.geu.unc:  I_Unit M_Unit */
    6,  /* cmp_z1.lt.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.lt.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.lt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.le.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.le.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.le.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.gt.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.gt.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.gt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.ge.orcm:  I_Unit M_Unit */
    6,  /* cmp_z1.ge.andcm:  I_Unit M_Unit */
    6,  /* cmp_z1.ge.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.lt.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.lt.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.lt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.le.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.le.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.le.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.gt.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.gt.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.gt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.ge.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.ge.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.ge.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_z2.lt.and:  I_Unit M_Unit */
    6,  /* cmp_z2.lt.or:  I_Unit M_Unit */
    6,  /* cmp_z2.lt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.le.and:  I_Unit M_Unit */
    6,  /* cmp_z2.le.or:  I_Unit M_Unit */
    6,  /* cmp_z2.le.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.gt.and:  I_Unit M_Unit */
    6,  /* cmp_z2.gt.or:  I_Unit M_Unit */
    6,  /* cmp_z2.gt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_z2.ge.and:  I_Unit M_Unit */
    6,  /* cmp_z2.ge.or:  I_Unit M_Unit */
    6,  /* cmp_z2.ge.or.andcm:  I_Unit M_Unit */
    6,  /* cmp_i.eq.orcm:  I_Unit M_Unit */
    6,  /* cmp_i.eq.andcm:  I_Unit M_Unit */
    6,  /* cmp_i.eq.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_i.ne:  I_Unit M_Unit */
    6,  /* cmp_i.ne.unc:  I_Unit M_Unit */
    6,  /* cmp_i.ne.orcm:  I_Unit M_Unit */
    6,  /* cmp_i.ne.andcm:  I_Unit M_Unit */
    6,  /* cmp_i.ne.and.orcm:  I_Unit M_Unit */
    6,  /* cmp_i.le:  I_Unit M_Unit */
    6,  /* cmp_i.le.unc:  I_Unit M_Unit */
    6,  /* cmp_i.gt:  I_Unit M_Unit */
    6,  /* cmp_i.gt.unc:  I_Unit M_Unit */
    6,  /* cmp_i.ge:  I_Unit M_Unit */
    6,  /* cmp_i.ge.unc:  I_Unit M_Unit */
    6,  /* cmp_i.leu:  I_Unit M_Unit */
    6,  /* cmp_i.leu.unc:  I_Unit M_Unit */
    6,  /* cmp_i.gtu:  I_Unit M_Unit */
    6,  /* cmp_i.gtu.unc:  I_Unit M_Unit */
    6,  /* cmp_i.geu:  I_Unit M_Unit */
    6,  /* cmp_i.geu.unc:  I_Unit M_Unit */
    6,  /* cmp4.eq.orcm:  I_Unit M_Unit */
    6,  /* cmp4.eq.andcm:  I_Unit M_Unit */
    6,  /* cmp4.eq.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4.ne:  I_Unit M_Unit */
    6,  /* cmp4.ne.unc:  I_Unit M_Unit */
    6,  /* cmp4.ne.orcm:  I_Unit M_Unit */
    6,  /* cmp4.ne.andcm:  I_Unit M_Unit */
    6,  /* cmp4.ne.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4.le:  I_Unit M_Unit */
    6,  /* cmp4.le.unc:  I_Unit M_Unit */
    6,  /* cmp4.gt:  I_Unit M_Unit */
    6,  /* cmp4.gt.unc:  I_Unit M_Unit */
    6,  /* cmp4.ge:  I_Unit M_Unit */
    6,  /* cmp4.ge.unc:  I_Unit M_Unit */
    6,  /* cmp4.leu:  I_Unit M_Unit */
    6,  /* cmp4.leu.unc:  I_Unit M_Unit */
    6,  /* cmp4.gtu:  I_Unit M_Unit */
    6,  /* cmp4.gtu.unc:  I_Unit M_Unit */
    6,  /* cmp4.geu:  I_Unit M_Unit */
    6,  /* cmp4.geu.unc:  I_Unit M_Unit */
    6,  /* cmp4_z1.lt.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.lt.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.lt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.le.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.le.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.le.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.gt.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.gt.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.gt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.ge.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.ge.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z1.ge.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.lt.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.lt.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.lt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.le.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.le.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.le.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.gt.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.gt.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.gt.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.ge.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.ge.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.ge.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.lt.and:  I_Unit M_Unit */
    6,  /* cmp4_z2.lt.or:  I_Unit M_Unit */
    6,  /* cmp4_z2.lt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.le.and:  I_Unit M_Unit */
    6,  /* cmp4_z2.le.or:  I_Unit M_Unit */
    6,  /* cmp4_z2.le.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.gt.and:  I_Unit M_Unit */
    6,  /* cmp4_z2.gt.or:  I_Unit M_Unit */
    6,  /* cmp4_z2.gt.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_z2.ge.and:  I_Unit M_Unit */
    6,  /* cmp4_z2.ge.or:  I_Unit M_Unit */
    6,  /* cmp4_z2.ge.or.andcm:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.orcm:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.andcm:  I_Unit M_Unit */
    6,  /* cmp4_i.eq.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_i.ne:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.orcm:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.andcm:  I_Unit M_Unit */
    6,  /* cmp4_i.ne.and.orcm:  I_Unit M_Unit */
    6,  /* cmp4_i.le:  I_Unit M_Unit */
    6,  /* cmp4_i.le.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.gt:  I_Unit M_Unit */
    6,  /* cmp4_i.gt.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.ge:  I_Unit M_Unit */
    6,  /* cmp4_i.ge.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.leu:  I_Unit M_Unit */
    6,  /* cmp4_i.leu.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.gtu:  I_Unit M_Unit */
    6,  /* cmp4_i.gtu.unc:  I_Unit M_Unit */
    6,  /* cmp4_i.geu:  I_Unit M_Unit */
    6,  /* cmp4_i.geu.unc:  I_Unit M_Unit */
   16,  /* fabs:  F_Unit */
   16,  /* fadd:  F_Unit */
   16,  /* fadd.s:  F_Unit */
   16,  /* fadd.d:  F_Unit */
   16,  /* fclass.nm:  F_Unit */
   16,  /* fclass.nm.unc:  F_Unit */
   16,  /* fcmp.gt:  F_Unit */
   16,  /* fcmp.gt.unc:  F_Unit */
   16,  /* fcmp.ge:  F_Unit */
   16,  /* fcmp.ge.unc:  F_Unit */
   16,  /* fcmp.neq:  F_Unit */
   16,  /* fcmp.neq.unc:  F_Unit */
   16,  /* fcmp.nlt:  F_Unit */
   16,  /* fcmp.nlt.unc:  F_Unit */
   16,  /* fcmp.nle:  F_Unit */
   16,  /* fcmp.nle.unc:  F_Unit */
   16,  /* fcmp.ngt:  F_Unit */
   16,  /* fcmp.ngt.unc:  F_Unit */
   16,  /* fcmp.nge:  F_Unit */
   16,  /* fcmp.nge.unc:  F_Unit */
   16,  /* fcmp.ord:  F_Unit */
   16,  /* fcmp.ord.unc:  F_Unit */
   16,  /* fcvt.xuf:  F_Unit */
   16,  /* fcvt.xuf.s:  F_Unit */
   16,  /* fcvt.xuf.d:  F_Unit */
   16,  /* fmpy:  F_Unit */
   16,  /* fmpy.s:  F_Unit */
   16,  /* fmpy.d:  F_Unit */
   16,  /* fneg:  F_Unit */
   16,  /* fnegabs:  F_Unit */
   16,  /* fnmpy:  F_Unit */
   16,  /* fnmpy.s:  F_Unit */
   16,  /* fnmpy.d:  F_Unit */
   16,  /* fnorm:  F_Unit */
   16,  /* fnorm.s:  F_Unit */
   16,  /* fnorm.d:  F_Unit */
   16,  /* fpabs:  F_Unit */
   16,  /* fpcmp.gt:  F_Unit */
   16,  /* fpcmp.ge:  F_Unit */
   16,  /* fpcmp.ngt:  F_Unit */
   16,  /* fpcmp.nge:  F_Unit */
   16,  /* fpmpy:  F_Unit */
   16,  /* fpneg:  F_Unit */
   16,  /* fpnegabs:  F_Unit */
   16,  /* fpnmpy:  F_Unit */
   16,  /* fsub:  F_Unit */
   16,  /* fsub.s:  F_Unit */
   16,  /* fsub.d:  F_Unit */
    2,  /* mov_t_br:  I_Unit */
   16,  /* mov_f:  F_Unit */
    6,  /* mov:  I_Unit M_Unit */
    6,  /* mov_i:  I_Unit M_Unit */
    2,  /* shl_i:  I_Unit */
    2,  /* shr_i:  I_Unit */
    2,  /* shr_i.u:  I_Unit */
    2,  /* tbit.nz:  I_Unit */
    2,  /* tbit.nz.unc:  I_Unit */
    2,  /* tnat.nz:  I_Unit */
    2,  /* tnat.nz.unc:  I_Unit */
   16,  /* xma.lu:  F_Unit */
   16,  /* xmpy.l:  F_Unit */
   16,  /* xmpy.lu:  F_Unit */
   16,  /* xmpy.h:  F_Unit */
   16,  /* xmpy.hu:  F_Unit */
  222,  /* break:  I_Unit M_Unit B_Unit F_Unit I2_Unit B2_Unit */
    6,  /* chk.s:  I_Unit M_Unit */
    2,  /* cmp.lt.and:  I_Unit */
    2,  /* cmp.lt.or:  I_Unit */
    2,  /* cmp.lt.or.andcm:  I_Unit */
    2,  /* cmp.le.and:  I_Unit */
    2,  /* cmp.le.or:  I_Unit */
    2,  /* cmp.le.or.andcm:  I_Unit */
    2,  /* cmp.gt.and:  I_Unit */
    2,  /* cmp.gt.or:  I_Unit */
    2,  /* cmp.gt.or.andcm:  I_Unit */
    2,  /* cmp.ge.and:  I_Unit */
    2,  /* cmp.ge.or:  I_Unit */
    2,  /* cmp.ge.or.andcm:  I_Unit */
    2,  /* cmp.lt.orcm:  I_Unit */
    2,  /* cmp.lt.andcm:  I_Unit */
    2,  /* cmp.lt.and.orcm:  I_Unit */
    2,  /* cmp.le.orcm:  I_Unit */
    2,  /* cmp.le.andcm:  I_Unit */
    2,  /* cmp.le.and.orcm:  I_Unit */
    2,  /* cmp.gt.orcm:  I_Unit */
    2,  /* cmp.gt.andcm:  I_Unit */
    2,  /* cmp.gt.and.orcm:  I_Unit */
    2,  /* cmp.ge.orcm:  I_Unit */
    2,  /* cmp.ge.andcm:  I_Unit */
    2,  /* cmp.ge.and.orcm:  I_Unit */
    2,  /* cmp4.lt.and:  I_Unit */
    2,  /* cmp4.lt.or:  I_Unit */
    2,  /* cmp4.lt.or.andcm:  I_Unit */
    2,  /* cmp4.le.and:  I_Unit */
    2,  /* cmp4.le.or:  I_Unit */
    2,  /* cmp4.le.or.andcm:  I_Unit */
    2,  /* cmp4.gt.and:  I_Unit */
    2,  /* cmp4.gt.or:  I_Unit */
    2,  /* cmp4.gt.or.andcm:  I_Unit */
    2,  /* cmp4.ge.and:  I_Unit */
    2,  /* cmp4.ge.or:  I_Unit */
    2,  /* cmp4.ge.or.andcm:  I_Unit */
    2,  /* cmp4.lt.orcm:  I_Unit */
    2,  /* cmp4.lt.andcm:  I_Unit */
    2,  /* cmp4.lt.and.orcm:  I_Unit */
    2,  /* cmp4.le.orcm:  I_Unit */
    2,  /* cmp4.le.andcm:  I_Unit */
    2,  /* cmp4.le.and.orcm:  I_Unit */
    2,  /* cmp4.gt.orcm:  I_Unit */
    2,  /* cmp4.gt.andcm:  I_Unit */
    2,  /* cmp4.gt.and.orcm:  I_Unit */
    2,  /* cmp4.ge.orcm:  I_Unit */
    2,  /* cmp4.ge.andcm:  I_Unit */
    2,  /* cmp4.ge.and.orcm:  I_Unit */
    6,  /* mov_f_ar:  I_Unit M_Unit */
    6,  /* mov_t_ar_r:  I_Unit M_Unit */
    6,  /* mov_t_ar_i:  I_Unit M_Unit */
  222,  /* nop:  I_Unit M_Unit B_Unit F_Unit I2_Unit B2_Unit */
    0,  /* asm:  */
    0,  /* intrncall:  */
    0,  /* spadjust:  */
    0,  /* copy.br:  */
    0,  /* begin.pregtn:  */
    0,  /* end.pregtn:  */
    0,  /* bwd.bar:  */
    0,  /* fwd.bar:  */
    0,  /* dfixup:  */
    0,  /* ffixup:  */
    0,  /* ifixup:  */
    0,  /* label:  */
    0,  /* noop:  */
};

const ISA_BUNDLE_PACK_INFO ISA_BUNDLE_pack_info[] = {
  { ISA_BUNDLE_PACK_COMP_template ,  0,  0,  0, 0x000000000000001f },  /* TEMPLATE */
  { ISA_BUNDLE_PACK_COMP_slot+0   ,  0,  0,  5, 0x00003fffffffffe0 },  /* SLOT0 */
  { ISA_BUNDLE_PACK_COMP_slot+1   ,  0,  0, 46, 0xffffc00000000000 },  /* SLOT1 */
  { ISA_BUNDLE_PACK_COMP_slot+1   ,  1, 18,  0, 0x00000000007fffff },  /* SLOT1 */
  { ISA_BUNDLE_PACK_COMP_slot+2   ,  1,  0, 23, 0xffffffffff800000 },  /* SLOT2 */
  { ISA_BUNDLE_PACK_COMP_end      , -1, -1, -1,                 -1 },  /* END */
};

const mUINT8 ISA_BUNDLE_pack_info_index[5] = {
   5, /* ISA_BUNDLE_PACK_COMP_end */
   0, /* ISA_BUNDLE_PACK_COMP_template */
   1, /* ISA_BUNDLE_PACK_COMP_slot+0 */
   2, /* ISA_BUNDLE_PACK_COMP_slot+1 */
   4, /* ISA_BUNDLE_PACK_COMP_slot+2 */
};
