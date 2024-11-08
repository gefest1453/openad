/* ====================================================================
 * ====================================================================
 *
 * Description:
 *
 *   A description of the ISA (actually just an enum of all the opcodes).
 *   The description exports the following:
 *
 *   TOPCODE stands for Target OPCODE; prefix is TOP.
 *
 *   typedef (enum) TOP
 *      Contains all the target opcodes.  Their names have the form
 *      TOP_<name>.
 *
 *   typedef mTOP
 *      The smallest integer type that can contain all values of a TOP,
 *      including TOP_UNDEFINED -- useful for conserving space in tables.
 *
 *   const TOP TOP_UNDEFINED
 *      Useful value guaranteed not to be a valid TOP.
 *
 *   const int TOP_count
 *      Gives the number of topcodes.
 *
 *   const char* TOP_Name(TOP topcode)
 *      Returns an assembler style name for the given TOP.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef TOPCODE_INCLUDED
#define TOPCODE_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#ifndef defs_INCLUDED
#define defs_INCLUDED
typedef signed int INT;
typedef signed int INT32;
typedef signed long long INT64;
typedef signed char mINT8;
typedef signed short mINT16;
typedef signed int mINT32;
typedef signed long long mINT64;
typedef unsigned int UINT;
typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef unsigned char mUINT8;
typedef unsigned short mUINT16;
typedef unsigned int mUINT32;
typedef unsigned long long mUINT64;
typedef int BOOL;
typedef unsigned char mBOOL;
#ifndef TRUE
#define TRUE    ((BOOL) 1)
#endif
#ifndef FALSE
#define FALSE   ((BOOL) 0)
#endif
#if (defined(_LANGUAGE_C) || defined(__GNUC__)) && !defined(inline)
#define inline static __inline
#endif
#endif

typedef enum topcode {
  TOP_add,
  TOP_add_1,
  TOP_adds,
  TOP_addl,
  TOP_addp4,
  TOP_addp4_i,
  TOP_alloc_3,
  TOP_and,
  TOP_and_i,
  TOP_andcm,
  TOP_andcm_i,
  TOP_br_cond,
  TOP_br_r_cond,
  TOP_br_call,
  TOP_br_r_call,
  TOP_br_ret,
  TOP_br_ia,
  TOP_br_cloop,
  TOP_br_ctop,
  TOP_br_cexit,
  TOP_br_wtop,
  TOP_br_wexit,
  TOP_break_i,
  TOP_break_b,
  TOP_break_m,
  TOP_break_f,
  TOP_break_x,
  TOP_brl_cond,
  TOP_brl_call,
  TOP_brp,
  TOP_brp_r,
  TOP_brp_ret,
  TOP_bsw_0,
  TOP_bsw_1,
  TOP_chk_s_i,
  TOP_chk_s_m,
  TOP_chk_f_s,
  TOP_chk_a,
  TOP_chk_f_a,
  TOP_clrrrb,
  TOP_clrrrb_pr,
  TOP_cmp_eq,
  TOP_cmp_eq_unc,
  TOP_cmp_eq_and,
  TOP_cmp_eq_or,
  TOP_cmp_eq_or_andcm,
  TOP_cmp_ne_and,
  TOP_cmp_ne_or,
  TOP_cmp_ne_or_andcm,
  TOP_cmp_lt,
  TOP_cmp_lt_unc,
  TOP_cmp_ltu,
  TOP_cmp_ltu_unc,
  TOP_cmp_z1_lt_and,
  TOP_cmp_z1_lt_or,
  TOP_cmp_z1_lt_or_andcm,
  TOP_cmp_z1_le_and,
  TOP_cmp_z1_le_or,
  TOP_cmp_z1_le_or_andcm,
  TOP_cmp_z1_gt_and,
  TOP_cmp_z1_gt_or,
  TOP_cmp_z1_gt_or_andcm,
  TOP_cmp_z1_ge_and,
  TOP_cmp_z1_ge_or,
  TOP_cmp_z1_ge_or_andcm,
  TOP_cmp_i_eq,
  TOP_cmp_i_eq_unc,
  TOP_cmp_i_eq_and,
  TOP_cmp_i_eq_or,
  TOP_cmp_i_eq_or_andcm,
  TOP_cmp_i_ne_and,
  TOP_cmp_i_ne_or,
  TOP_cmp_i_ne_or_andcm,
  TOP_cmp_i_lt,
  TOP_cmp_i_lt_unc,
  TOP_cmp_i_ltu,
  TOP_cmp_i_ltu_unc,
  TOP_cmp4_eq,
  TOP_cmp4_eq_unc,
  TOP_cmp4_eq_and,
  TOP_cmp4_eq_or,
  TOP_cmp4_eq_or_andcm,
  TOP_cmp4_ne_and,
  TOP_cmp4_ne_or,
  TOP_cmp4_ne_or_andcm,
  TOP_cmp4_lt,
  TOP_cmp4_lt_unc,
  TOP_cmp4_ltu,
  TOP_cmp4_ltu_unc,
  TOP_cmp4_z1_lt_and,
  TOP_cmp4_z1_lt_or,
  TOP_cmp4_z1_lt_or_andcm,
  TOP_cmp4_z1_le_and,
  TOP_cmp4_z1_le_or,
  TOP_cmp4_z1_le_or_andcm,
  TOP_cmp4_z1_gt_and,
  TOP_cmp4_z1_gt_or,
  TOP_cmp4_z1_gt_or_andcm,
  TOP_cmp4_z1_ge_and,
  TOP_cmp4_z1_ge_or,
  TOP_cmp4_z1_ge_or_andcm,
  TOP_cmp4_i_eq,
  TOP_cmp4_i_eq_unc,
  TOP_cmp4_i_eq_and,
  TOP_cmp4_i_eq_or,
  TOP_cmp4_i_eq_or_andcm,
  TOP_cmp4_i_ne_and,
  TOP_cmp4_i_ne_or,
  TOP_cmp4_i_ne_or_andcm,
  TOP_cmp4_i_lt,
  TOP_cmp4_i_lt_unc,
  TOP_cmp4_i_ltu,
  TOP_cmp4_i_ltu_unc,
  TOP_cmpxchg1,
  TOP_cmpxchg2,
  TOP_cmpxchg4,
  TOP_cmpxchg8,
  TOP_cover,
  TOP_czx1_l,
  TOP_czx1_r,
  TOP_czx2_l,
  TOP_czx2_r,
  TOP_dep,
  TOP_dep_z,
  TOP_dep_i,
  TOP_dep_i_z,
  TOP_epc,
  TOP_extr,
  TOP_extr_u,
  TOP_famax,
  TOP_famin,
  TOP_fand,
  TOP_fandcm,
  TOP_fc,
  TOP_fchkf,
  TOP_fclass_m,
  TOP_fclass_m_unc,
  TOP_fclrf,
  TOP_fcmp_eq,
  TOP_fcmp_eq_unc,
  TOP_fcmp_lt,
  TOP_fcmp_lt_unc,
  TOP_fcmp_le,
  TOP_fcmp_le_unc,
  TOP_fcmp_unord,
  TOP_fcmp_unord_unc,
  TOP_fcvt_fx,
  TOP_fcvt_fx_trunc,
  TOP_fcvt_fxu,
  TOP_fcvt_fxu_trunc,
  TOP_fcvt_xf,
  TOP_fetchadd4,
  TOP_fetchadd8,
  TOP_flushrs,
  TOP_fma,
  TOP_fma_s,
  TOP_fma_d,
  TOP_fmax,
  TOP_fmerge_ns,
  TOP_fmerge_s,
  TOP_fmerge_se,
  TOP_fmin,
  TOP_fmix_l,
  TOP_fmix_r,
  TOP_fmix_lr,
  TOP_fms,
  TOP_fms_s,
  TOP_fms_d,
  TOP_fnma,
  TOP_fnma_s,
  TOP_fnma_d,
  TOP_for,
  TOP_fpack,
  TOP_fpamax,
  TOP_fpamin,
  TOP_fpcmp_eq,
  TOP_fpcmp_lt,
  TOP_fpcmp_le,
  TOP_fpcmp_unord,
  TOP_fpcmp_neq,
  TOP_fpcmp_nlt,
  TOP_fpcmp_nle,
  TOP_fpcmp_ord,
  TOP_fpcvt_fx,
  TOP_fpcvt_fx_trunc,
  TOP_fpcvt_fxu,
  TOP_fpcvt_fxu_trunc,
  TOP_fpma,
  TOP_fpmax,
  TOP_fpmerge_ns,
  TOP_fpmerge_s,
  TOP_fpmerge_se,
  TOP_fpmin,
  TOP_fpms,
  TOP_fpnma,
  TOP_fprcpa,
  TOP_fprsqrta,
  TOP_frcpa,
  TOP_frsqrta,
  TOP_fselect,
  TOP_fsetc,
  TOP_fswap,
  TOP_fswap_nl,
  TOP_fswap_nr,
  TOP_fsxt_l,
  TOP_fsxt_r,
  TOP_fwb,
  TOP_fxor,
  TOP_getf_s,
  TOP_getf_d,
  TOP_getf_exp,
  TOP_getf_sig,
  TOP_invala,
  TOP_invala_e,
  TOP_invala_f_e,
  TOP_itc_i,
  TOP_itc_d,
  TOP_itr_i,
  TOP_itr_d,
  TOP_ld1,
  TOP_ld1_r,
  TOP_ld1_i,
  TOP_ld2,
  TOP_ld2_r,
  TOP_ld2_i,
  TOP_ld4,
  TOP_ld4_r,
  TOP_ld4_i,
  TOP_ld8,
  TOP_ld8_r,
  TOP_ld8_i,
  TOP_ld8_fill,
  TOP_ld8_r_fill,
  TOP_ld8_i_fill,
  TOP_ldfs,
  TOP_ldfs_r,
  TOP_ldfs_i,
  TOP_ldfd,
  TOP_ldfd_r,
  TOP_ldfd_i,
  TOP_ldfe,
  TOP_ldfe_r,
  TOP_ldfe_i,
  TOP_ldf8,
  TOP_ldf8_r,
  TOP_ldf8_i,
  TOP_ldf_fill,
  TOP_ldf_r_fill,
  TOP_ldf_i_fill,
  TOP_ldfps,
  TOP_ldfps_i,
  TOP_ldfpd,
  TOP_ldfpd_i,
  TOP_ldfp8,
  TOP_ldfp8_i,
  TOP_lfetch,
  TOP_lfetch_excl,
  TOP_lfetch_fault,
  TOP_lfetch_fault_excl,
  TOP_lfetch_r,
  TOP_lfetch_r_excl,
  TOP_lfetch_r_fault,
  TOP_lfetch_r_fault_excl,
  TOP_lfetch_i,
  TOP_lfetch_i_excl,
  TOP_lfetch_i_fault,
  TOP_lfetch_i_fault_excl,
  TOP_loadrs,
  TOP_mf,
  TOP_mf_a,
  TOP_mix1_l,
  TOP_mix1_r,
  TOP_mix2_l,
  TOP_mix2_r,
  TOP_mix4_l,
  TOP_mix4_r,
  TOP_mov_f_ar_i,
  TOP_mov_t_ar_r_i,
  TOP_mov_t_ar_i_i,
  TOP_mov_f_ar_m,
  TOP_mov_t_ar_r_m,
  TOP_mov_t_ar_i_m,
  TOP_mov_f_br,
  TOP_mov_t_br_i,
  TOP_mov_t_br_ret,
  TOP_mov_t_cr,
  TOP_mov_f_cr,
  TOP_mov_f_cpuid,
  TOP_mov_t_dbr,
  TOP_mov_f_dbr,
  TOP_mov_t_ibr,
  TOP_mov_f_ibr,
  TOP_mov_t_msr,
  TOP_mov_f_msr,
  TOP_mov_t_pkr,
  TOP_mov_f_pkr,
  TOP_mov_t_pmc,
  TOP_mov_f_pmc,
  TOP_mov_t_pmd,
  TOP_mov_f_pmd,
  TOP_mov_t_rr,
  TOP_mov_f_rr,
  TOP_mov_f_ip,
  TOP_mov_f_pr,
  TOP_mov_t_pr,
  TOP_mov_t_pr_i,
  TOP_mov_t_psr,
  TOP_mov_f_psr,
  TOP_mov_t_psrum,
  TOP_mov_f_psrum,
  TOP_movl,
  TOP_mux1,
  TOP_mux2,
  TOP_nop_i,
  TOP_nop_b,
  TOP_nop_m,
  TOP_nop_f,
  TOP_nop_x,
  TOP_or,
  TOP_or_i,
  TOP_pack2_sss,
  TOP_pack2_uss,
  TOP_pack4_sss,
  TOP_padd1,
  TOP_padd1_sss,
  TOP_padd1_uus,
  TOP_padd1_uuu,
  TOP_padd2,
  TOP_padd2_sss,
  TOP_padd2_uus,
  TOP_padd2_uuu,
  TOP_padd4,
  TOP_pavg1,
  TOP_pavg1_raz,
  TOP_pavg2,
  TOP_pavg2_raz,
  TOP_pavgsub1,
  TOP_pavgsub2,
  TOP_pcmp1_eq,
  TOP_pcmp1_gt,
  TOP_pcmp2_eq,
  TOP_pcmp2_gt,
  TOP_pcmp4_eq,
  TOP_pcmp4_gt,
  TOP_pmax1_u,
  TOP_pmax2,
  TOP_pmin1_u,
  TOP_pmin2,
  TOP_pmpy2_r,
  TOP_pmpy2_l,
  TOP_pmpyshr2,
  TOP_pmpyshr2_u,
  TOP_popcnt,
  TOP_probe_r,
  TOP_probe_w,
  TOP_probe_i_r,
  TOP_probe_i_w,
  TOP_probe_r_fault,
  TOP_probe_w_fault,
  TOP_probe_rw_fault,
  TOP_psad1,
  TOP_pshl2,
  TOP_pshl4,
  TOP_pshl2_i,
  TOP_pshl4_i,
  TOP_pshladd2,
  TOP_pshr2,
  TOP_pshr2_u,
  TOP_pshr4,
  TOP_pshr4_u,
  TOP_pshr2_i,
  TOP_pshr2_i_u,
  TOP_pshr4_i,
  TOP_pshr4_i_u,
  TOP_pshradd2,
  TOP_psub1,
  TOP_psub2,
  TOP_psub4,
  TOP_psub1_sss,
  TOP_psub1_uus,
  TOP_psub1_uuu,
  TOP_psub2_sss,
  TOP_psub2_uus,
  TOP_psub2_uuu,
  TOP_ptc_e,
  TOP_ptc_g,
  TOP_ptc_ga,
  TOP_ptc_l,
  TOP_ptr_d,
  TOP_ptr_i,
  TOP_rfi,
  TOP_rsm,
  TOP_rum,
  TOP_setf_s,
  TOP_setf_d,
  TOP_setf_exp,
  TOP_setf_sig,
  TOP_shl,
  TOP_shladd,
  TOP_shladdp4,
  TOP_shr,
  TOP_shr_u,
  TOP_shrp,
  TOP_srlz_i,
  TOP_srlz_d,
  TOP_ssm,
  TOP_st1,
  TOP_st1_i,
  TOP_st2,
  TOP_st2_i,
  TOP_st4,
  TOP_st4_i,
  TOP_st8,
  TOP_st8_i,
  TOP_st8_spill,
  TOP_st8_i_spill,
  TOP_stfs,
  TOP_stfs_i,
  TOP_stfd,
  TOP_stfd_i,
  TOP_stfe,
  TOP_stfe_i,
  TOP_stf8,
  TOP_stf8_i,
  TOP_stf_spill,
  TOP_stf_i_spill,
  TOP_sub,
  TOP_sub_1,
  TOP_sub_i,
  TOP_sum,
  TOP_sxt1,
  TOP_sxt2,
  TOP_sxt4,
  TOP_sync_i,
  TOP_tak,
  TOP_tbit_z,
  TOP_tbit_z_unc,
  TOP_tbit_z_and,
  TOP_tbit_z_or,
  TOP_tbit_z_or_andcm,
  TOP_tbit_nz_and,
  TOP_tbit_nz_or,
  TOP_tbit_nz_or_andcm,
  TOP_thash,
  TOP_tnat_z,
  TOP_tnat_z_unc,
  TOP_tnat_z_and,
  TOP_tnat_z_or,
  TOP_tnat_z_or_andcm,
  TOP_tnat_nz_and,
  TOP_tnat_nz_or,
  TOP_tnat_nz_or_andcm,
  TOP_tpa,
  TOP_ttag,
  TOP_unpack1_l,
  TOP_unpack1_h,
  TOP_unpack2_l,
  TOP_unpack2_h,
  TOP_unpack4_l,
  TOP_unpack4_h,
  TOP_xchg1,
  TOP_xchg2,
  TOP_xchg4,
  TOP_xchg8,
  TOP_xma_l,
  TOP_xma_h,
  TOP_xma_hu,
  TOP_xor,
  TOP_xor_i,
  TOP_zxt1,
  TOP_zxt2,
  TOP_zxt4,
  TOP_alloc,
  TOP_br,
  TOP_br_r,
  TOP_brl,
  TOP_cmp_eq_orcm,
  TOP_cmp_eq_andcm,
  TOP_cmp_eq_and_orcm,
  TOP_cmp_ne,
  TOP_cmp_ne_unc,
  TOP_cmp_ne_orcm,
  TOP_cmp_ne_andcm,
  TOP_cmp_ne_and_orcm,
  TOP_cmp_le,
  TOP_cmp_le_unc,
  TOP_cmp_gt,
  TOP_cmp_gt_unc,
  TOP_cmp_ge,
  TOP_cmp_ge_unc,
  TOP_cmp_leu,
  TOP_cmp_leu_unc,
  TOP_cmp_gtu,
  TOP_cmp_gtu_unc,
  TOP_cmp_geu,
  TOP_cmp_geu_unc,
  TOP_cmp_z1_lt_orcm,
  TOP_cmp_z1_lt_andcm,
  TOP_cmp_z1_lt_and_orcm,
  TOP_cmp_z1_le_orcm,
  TOP_cmp_z1_le_andcm,
  TOP_cmp_z1_le_and_orcm,
  TOP_cmp_z1_gt_orcm,
  TOP_cmp_z1_gt_andcm,
  TOP_cmp_z1_gt_and_orcm,
  TOP_cmp_z1_ge_orcm,
  TOP_cmp_z1_ge_andcm,
  TOP_cmp_z1_ge_and_orcm,
  TOP_cmp_z2_lt_orcm,
  TOP_cmp_z2_lt_andcm,
  TOP_cmp_z2_lt_and_orcm,
  TOP_cmp_z2_le_orcm,
  TOP_cmp_z2_le_andcm,
  TOP_cmp_z2_le_and_orcm,
  TOP_cmp_z2_gt_orcm,
  TOP_cmp_z2_gt_andcm,
  TOP_cmp_z2_gt_and_orcm,
  TOP_cmp_z2_ge_orcm,
  TOP_cmp_z2_ge_andcm,
  TOP_cmp_z2_ge_and_orcm,
  TOP_cmp_z2_lt_and,
  TOP_cmp_z2_lt_or,
  TOP_cmp_z2_lt_or_andcm,
  TOP_cmp_z2_le_and,
  TOP_cmp_z2_le_or,
  TOP_cmp_z2_le_or_andcm,
  TOP_cmp_z2_gt_and,
  TOP_cmp_z2_gt_or,
  TOP_cmp_z2_gt_or_andcm,
  TOP_cmp_z2_ge_and,
  TOP_cmp_z2_ge_or,
  TOP_cmp_z2_ge_or_andcm,
  TOP_cmp_i_eq_orcm,
  TOP_cmp_i_eq_andcm,
  TOP_cmp_i_eq_and_orcm,
  TOP_cmp_i_ne,
  TOP_cmp_i_ne_unc,
  TOP_cmp_i_ne_orcm,
  TOP_cmp_i_ne_andcm,
  TOP_cmp_i_ne_and_orcm,
  TOP_cmp_i_le,
  TOP_cmp_i_le_unc,
  TOP_cmp_i_gt,
  TOP_cmp_i_gt_unc,
  TOP_cmp_i_ge,
  TOP_cmp_i_ge_unc,
  TOP_cmp_i_leu,
  TOP_cmp_i_leu_unc,
  TOP_cmp_i_gtu,
  TOP_cmp_i_gtu_unc,
  TOP_cmp_i_geu,
  TOP_cmp_i_geu_unc,
  TOP_cmp4_eq_orcm,
  TOP_cmp4_eq_andcm,
  TOP_cmp4_eq_and_orcm,
  TOP_cmp4_ne,
  TOP_cmp4_ne_unc,
  TOP_cmp4_ne_orcm,
  TOP_cmp4_ne_andcm,
  TOP_cmp4_ne_and_orcm,
  TOP_cmp4_le,
  TOP_cmp4_le_unc,
  TOP_cmp4_gt,
  TOP_cmp4_gt_unc,
  TOP_cmp4_ge,
  TOP_cmp4_ge_unc,
  TOP_cmp4_leu,
  TOP_cmp4_leu_unc,
  TOP_cmp4_gtu,
  TOP_cmp4_gtu_unc,
  TOP_cmp4_geu,
  TOP_cmp4_geu_unc,
  TOP_cmp4_z1_lt_orcm,
  TOP_cmp4_z1_lt_andcm,
  TOP_cmp4_z1_lt_and_orcm,
  TOP_cmp4_z1_le_orcm,
  TOP_cmp4_z1_le_andcm,
  TOP_cmp4_z1_le_and_orcm,
  TOP_cmp4_z1_gt_orcm,
  TOP_cmp4_z1_gt_andcm,
  TOP_cmp4_z1_gt_and_orcm,
  TOP_cmp4_z1_ge_orcm,
  TOP_cmp4_z1_ge_andcm,
  TOP_cmp4_z1_ge_and_orcm,
  TOP_cmp4_z2_lt_orcm,
  TOP_cmp4_z2_lt_andcm,
  TOP_cmp4_z2_lt_and_orcm,
  TOP_cmp4_z2_le_orcm,
  TOP_cmp4_z2_le_andcm,
  TOP_cmp4_z2_le_and_orcm,
  TOP_cmp4_z2_gt_orcm,
  TOP_cmp4_z2_gt_andcm,
  TOP_cmp4_z2_gt_and_orcm,
  TOP_cmp4_z2_ge_orcm,
  TOP_cmp4_z2_ge_andcm,
  TOP_cmp4_z2_ge_and_orcm,
  TOP_cmp4_z2_lt_and,
  TOP_cmp4_z2_lt_or,
  TOP_cmp4_z2_lt_or_andcm,
  TOP_cmp4_z2_le_and,
  TOP_cmp4_z2_le_or,
  TOP_cmp4_z2_le_or_andcm,
  TOP_cmp4_z2_gt_and,
  TOP_cmp4_z2_gt_or,
  TOP_cmp4_z2_gt_or_andcm,
  TOP_cmp4_z2_ge_and,
  TOP_cmp4_z2_ge_or,
  TOP_cmp4_z2_ge_or_andcm,
  TOP_cmp4_i_eq_orcm,
  TOP_cmp4_i_eq_andcm,
  TOP_cmp4_i_eq_and_orcm,
  TOP_cmp4_i_ne,
  TOP_cmp4_i_ne_unc,
  TOP_cmp4_i_ne_orcm,
  TOP_cmp4_i_ne_andcm,
  TOP_cmp4_i_ne_and_orcm,
  TOP_cmp4_i_le,
  TOP_cmp4_i_le_unc,
  TOP_cmp4_i_gt,
  TOP_cmp4_i_gt_unc,
  TOP_cmp4_i_ge,
  TOP_cmp4_i_ge_unc,
  TOP_cmp4_i_leu,
  TOP_cmp4_i_leu_unc,
  TOP_cmp4_i_gtu,
  TOP_cmp4_i_gtu_unc,
  TOP_cmp4_i_geu,
  TOP_cmp4_i_geu_unc,
  TOP_fabs,
  TOP_fadd,
  TOP_fadd_s,
  TOP_fadd_d,
  TOP_fclass_nm,
  TOP_fclass_nm_unc,
  TOP_fcmp_gt,
  TOP_fcmp_gt_unc,
  TOP_fcmp_ge,
  TOP_fcmp_ge_unc,
  TOP_fcmp_neq,
  TOP_fcmp_neq_unc,
  TOP_fcmp_nlt,
  TOP_fcmp_nlt_unc,
  TOP_fcmp_nle,
  TOP_fcmp_nle_unc,
  TOP_fcmp_ngt,
  TOP_fcmp_ngt_unc,
  TOP_fcmp_nge,
  TOP_fcmp_nge_unc,
  TOP_fcmp_ord,
  TOP_fcmp_ord_unc,
  TOP_fcvt_xuf,
  TOP_fcvt_xuf_s,
  TOP_fcvt_xuf_d,
  TOP_fmpy,
  TOP_fmpy_s,
  TOP_fmpy_d,
  TOP_fneg,
  TOP_fnegabs,
  TOP_fnmpy,
  TOP_fnmpy_s,
  TOP_fnmpy_d,
  TOP_fnorm,
  TOP_fnorm_s,
  TOP_fnorm_d,
  TOP_fpabs,
  TOP_fpcmp_gt,
  TOP_fpcmp_ge,
  TOP_fpcmp_ngt,
  TOP_fpcmp_nge,
  TOP_fpmpy,
  TOP_fpneg,
  TOP_fpnegabs,
  TOP_fpnmpy,
  TOP_fsub,
  TOP_fsub_s,
  TOP_fsub_d,
  TOP_mov_t_br,
  TOP_mov_f,
  TOP_mov,
  TOP_mov_i,
  TOP_shl_i,
  TOP_shr_i,
  TOP_shr_i_u,
  TOP_tbit_nz,
  TOP_tbit_nz_unc,
  TOP_tnat_nz,
  TOP_tnat_nz_unc,
  TOP_xma_lu,
  TOP_xmpy_l,
  TOP_xmpy_lu,
  TOP_xmpy_h,
  TOP_xmpy_hu,
  TOP_break,
  TOP_chk_s,
  TOP_cmp_lt_and,
  TOP_cmp_lt_or,
  TOP_cmp_lt_or_andcm,
  TOP_cmp_le_and,
  TOP_cmp_le_or,
  TOP_cmp_le_or_andcm,
  TOP_cmp_gt_and,
  TOP_cmp_gt_or,
  TOP_cmp_gt_or_andcm,
  TOP_cmp_ge_and,
  TOP_cmp_ge_or,
  TOP_cmp_ge_or_andcm,
  TOP_cmp_lt_orcm,
  TOP_cmp_lt_andcm,
  TOP_cmp_lt_and_orcm,
  TOP_cmp_le_orcm,
  TOP_cmp_le_andcm,
  TOP_cmp_le_and_orcm,
  TOP_cmp_gt_orcm,
  TOP_cmp_gt_andcm,
  TOP_cmp_gt_and_orcm,
  TOP_cmp_ge_orcm,
  TOP_cmp_ge_andcm,
  TOP_cmp_ge_and_orcm,
  TOP_cmp4_lt_and,
  TOP_cmp4_lt_or,
  TOP_cmp4_lt_or_andcm,
  TOP_cmp4_le_and,
  TOP_cmp4_le_or,
  TOP_cmp4_le_or_andcm,
  TOP_cmp4_gt_and,
  TOP_cmp4_gt_or,
  TOP_cmp4_gt_or_andcm,
  TOP_cmp4_ge_and,
  TOP_cmp4_ge_or,
  TOP_cmp4_ge_or_andcm,
  TOP_cmp4_lt_orcm,
  TOP_cmp4_lt_andcm,
  TOP_cmp4_lt_and_orcm,
  TOP_cmp4_le_orcm,
  TOP_cmp4_le_andcm,
  TOP_cmp4_le_and_orcm,
  TOP_cmp4_gt_orcm,
  TOP_cmp4_gt_andcm,
  TOP_cmp4_gt_and_orcm,
  TOP_cmp4_ge_orcm,
  TOP_cmp4_ge_andcm,
  TOP_cmp4_ge_and_orcm,
  TOP_mov_f_ar,
  TOP_mov_t_ar_r,
  TOP_mov_t_ar_i,
  TOP_nop,
  TOP_asm,
  TOP_intrncall,
  TOP_spadjust,
  TOP_copy_br,
  TOP_begin_pregtn,
  TOP_end_pregtn,
  TOP_bwd_bar,
  TOP_fwd_bar,
  TOP_dfixup,
  TOP_ffixup,
  TOP_ifixup,
  TOP_label,
  TOP_noop,
  TOP_UNDEFINED
} TOP;

typedef mUINT16 mTOP;

#define TOP_count 759

extern const char* TOP_Name(TOP topcode);

#ifdef __cplusplus
}
#endif
#endif
