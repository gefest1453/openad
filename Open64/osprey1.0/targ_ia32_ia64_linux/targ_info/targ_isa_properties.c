#include "targ_isa_properties.h"

const mUINT64 ISA_PROPERTIES_flags[] = {
  0x0000001000000200ULL, /* add: iadd predicated */
  0x0000001000000000ULL, /* add_1: predicated */
  0x0000001000000200ULL, /* adds: iadd predicated */
  0x0000001000000200ULL, /* addl: iadd predicated */
  0x0000001000000200ULL, /* addp4: iadd predicated */
  0x0000001000000200ULL, /* addp4_i: iadd predicated */
  0x0000004008000000ULL, /* alloc_3: f_group side_effects */
  0x0000001200000000ULL, /* and: iand predicated */
  0x0000001200000000ULL, /* and_i: iand predicated */
  0x0000001000000000ULL, /* andcm: predicated */
  0x0000001000000000ULL, /* andcm_i: predicated */
  0x0000001000000050ULL, /* br.cond: xfer cond predicated */
  0x0000001080000050ULL, /* br_r.cond: xfer cond ijump predicated */
  0x0000001000000030ULL, /* br.call: xfer call predicated */
  0x0000001080000030ULL, /* br_r.call: xfer call ijump predicated */
  0x0000001080000010ULL, /* br.ret: xfer ijump predicated */
  0x0000000080000010ULL, /* br.ia: xfer ijump */
  0x0000000000000050ULL, /* br.cloop: xfer cond */
  0x0000000000000050ULL, /* br.ctop: xfer cond */
  0x0000000000000050ULL, /* br.cexit: xfer cond */
  0x0000001000000050ULL, /* br.wtop: xfer cond predicated */
  0x0000001000000050ULL, /* br.wexit: xfer cond predicated */
  0x0000001000000010ULL, /* break.i: xfer predicated */
  0x0000001000000010ULL, /* break.b: xfer predicated */
  0x0000001000000010ULL, /* break.m: xfer predicated */
  0x0000001000000010ULL, /* break.f: xfer predicated */
  0x0000001000000010ULL, /* break.x: xfer predicated */
  0x0000001000000050ULL, /* brl.cond: xfer cond predicated */
  0x0000001000000010ULL, /* brl.call: xfer predicated */
  0x0000008000000000ULL, /* brp: branch_predict */
  0x0000008000000000ULL, /* brp_r: branch_predict */
  0x0000008000000000ULL, /* brp.ret: branch_predict */
  0x0000000030000000ULL, /* bsw.0: l_group privileged */
  0x0000000030000000ULL, /* bsw.1: l_group privileged */
  0x0000001000000000ULL, /* chk.s.i: predicated */
  0x0000001000000000ULL, /* chk.s.m: predicated */
  0x0000001000000000ULL, /* chk_f.s: predicated */
  0x0000001000000000ULL, /* chk.a: predicated */
  0x0000001000000000ULL, /* chk_f.a: predicated */
  0x0000002010000000ULL, /* clrrrb: l_group access_reg_bank */
  0x0000002010000000ULL, /* clrrrb.pr: l_group access_reg_bank */
  0x0000001400000000ULL, /* cmp.eq: icmp predicated */
  0x0000001400000000ULL, /* cmp.eq.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.eq.and: icmp predicated */
  0x0000001400000000ULL, /* cmp.eq.or: icmp predicated */
  0x0000001400000000ULL, /* cmp.eq.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.and: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.or: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.lt: icmp predicated */
  0x0000001400000000ULL, /* cmp.lt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.ltu: icmp predicated */
  0x0000001400000000ULL, /* cmp.ltu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.lt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.lt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.lt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.le.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.le.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.le.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.gt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.gt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.gt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.ge.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.ge.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.ge.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.lt: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.lt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ltu: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ltu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.lt: icmp predicated */
  0x0000001400000000ULL, /* cmp4.lt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ltu: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ltu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.lt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.lt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.lt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.le.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.le.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.le.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.gt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.gt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.gt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.ge.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.ge.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.ge.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.lt: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.lt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ltu: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ltu.unc: icmp predicated */
  0x0000001000000000ULL, /* cmpxchg1: predicated */
  0x0000001000000000ULL, /* cmpxchg2: predicated */
  0x0000001000000000ULL, /* cmpxchg4: predicated */
  0x0000001000000000ULL, /* cmpxchg8: predicated */
  0x0000000010000000ULL, /* cover: l_group */
  0x0000001000000000ULL, /* czx1.l: predicated */
  0x0000001000000000ULL, /* czx1.r: predicated */
  0x0000001000000000ULL, /* czx2.l: predicated */
  0x0000001000000000ULL, /* czx2.r: predicated */
  0x0000001000000000ULL, /* dep: predicated */
  0x0000001000000000ULL, /* dep.z: predicated */
  0x0000001000000000ULL, /* dep_i: predicated */
  0x0000001000000000ULL, /* dep_i.z: predicated */
  0x0000000000000000ULL, /* epc: */
  0x0000001000000000ULL, /* extr: predicated */
  0x0000001000000000ULL, /* extr.u: predicated */
  0x0000001000011000ULL, /* famax: flop fmisc predicated */
  0x0000001000011000ULL, /* famin: flop fmisc predicated */
  0x0000001200010000ULL, /* fand: fmisc iand predicated */
  0x0000001000010000ULL, /* fandcm: fmisc predicated */
  0x0000001000000000ULL, /* fc: predicated */
  0x0000001000000000ULL, /* fchkf: predicated */
  0x0000001000000000ULL, /* fclass.m: predicated */
  0x0000001000000000ULL, /* fclass.m.unc: predicated */
  0x0000001000000000ULL, /* fclrf: predicated */
  0x0000001000001000ULL, /* fcmp.eq: flop predicated */
  0x0000001000001000ULL, /* fcmp.eq.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.lt: flop predicated */
  0x0000001000001000ULL, /* fcmp.lt.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.le: flop predicated */
  0x0000001000001000ULL, /* fcmp.le.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.unord: flop predicated */
  0x0000001000001000ULL, /* fcmp.unord.unc: flop predicated */
  0x0000001000001000ULL, /* fcvt.fx: flop predicated */
  0x0000001000001000ULL, /* fcvt.fx.trunc: flop predicated */
  0x0000001000001000ULL, /* fcvt.fxu: flop predicated */
  0x0000001000001000ULL, /* fcvt.fxu.trunc: flop predicated */
  0x0000001000001000ULL, /* fcvt.xf: flop predicated */
  0x0000001000000000ULL, /* fetchadd4: predicated */
  0x0000001000000000ULL, /* fetchadd8: predicated */
  0x0000000008000000ULL, /* flushrs: f_group */
  0x0000001000021000ULL, /* fma: flop madd predicated */
  0x0000001000021000ULL, /* fma.s: flop madd predicated */
  0x0000001000021000ULL, /* fma.d: flop madd predicated */
  0x0000001000011000ULL, /* fmax: flop fmisc predicated */
  0x0000001000011000ULL, /* fmerge.ns: flop fmisc predicated */
  0x0000001000011000ULL, /* fmerge.s: flop fmisc predicated */
  0x0000001000011000ULL, /* fmerge.se: flop fmisc predicated */
  0x0000001000011000ULL, /* fmin: flop fmisc predicated */
  0x0000001000011000ULL, /* fmix.l: flop fmisc predicated */
  0x0000001000011000ULL, /* fmix.r: flop fmisc predicated */
  0x0000001000011000ULL, /* fmix.lr: flop fmisc predicated */
  0x0000001000021000ULL, /* fms: flop madd predicated */
  0x0000001000021000ULL, /* fms.s: flop madd predicated */
  0x0000001000021000ULL, /* fms.d: flop madd predicated */
  0x0000001000021000ULL, /* fnma: flop madd predicated */
  0x0000001000021000ULL, /* fnma.s: flop madd predicated */
  0x0000001000021000ULL, /* fnma.d: flop madd predicated */
  0x0000001040010000ULL, /* for: fmisc ior predicated */
  0x0000001000010000ULL, /* fpack: fmisc predicated */
  0x0000001000001000ULL, /* fpamax: flop predicated */
  0x0000001000001000ULL, /* fpamin: flop predicated */
  0x0000001000001000ULL, /* fpcmp.eq: flop predicated */
  0x0000001000001000ULL, /* fpcmp.lt: flop predicated */
  0x0000001000001000ULL, /* fpcmp.le: flop predicated */
  0x0000001000001000ULL, /* fpcmp.unord: flop predicated */
  0x0000001000001000ULL, /* fpcmp.neq: flop predicated */
  0x0000001000001000ULL, /* fpcmp.nlt: flop predicated */
  0x0000001000001000ULL, /* fpcmp.nle: flop predicated */
  0x0000001000001000ULL, /* fpcmp.ord: flop predicated */
  0x0000001000000000ULL, /* fpcvt.fx: predicated */
  0x0000001000000000ULL, /* fpcvt.fx.trunc: predicated */
  0x0000001000000000ULL, /* fpcvt.fxu: predicated */
  0x0000001000000000ULL, /* fpcvt.fxu.trunc: predicated */
  0x0000001000021000ULL, /* fpma: flop madd predicated */
  0x0000001000001000ULL, /* fpmax: flop predicated */
  0x0000001000001000ULL, /* fpmerge.ns: flop predicated */
  0x0000001000001000ULL, /* fpmerge.s: flop predicated */
  0x0000001000001000ULL, /* fpmerge.se: flop predicated */
  0x0000001000001000ULL, /* fpmin: flop predicated */
  0x0000001000001000ULL, /* fpms: flop predicated */
  0x0000001000021000ULL, /* fpnma: flop madd predicated */
  0x0000001001001000ULL, /* fprcpa: flop fdiv predicated */
  0x0000001003001000ULL, /* fprsqrta: flop fdiv sqrt predicated */
  0x0000001001011000ULL, /* frcpa: flop fmisc fdiv predicated */
  0x0000001003011000ULL, /* frsqrta: flop fmisc fdiv sqrt predicated */
  0x0000001000010000ULL, /* fselect: fmisc predicated */
  0x0000005000000000ULL, /* fsetc: predicated side_effects */
  0x0000001000010000ULL, /* fswap: fmisc predicated */
  0x0000001000010000ULL, /* fswap.nl: fmisc predicated */
  0x0000001000010000ULL, /* fswap.nr: fmisc predicated */
  0x0000001000010000ULL, /* fsxt.l: fmisc predicated */
  0x0000001000010000ULL, /* fsxt.r: fmisc predicated */
  0x0000001000000000ULL, /* fwb: predicated */
  0x0000001100010000ULL, /* fxor: fmisc ixor predicated */
  0x0000001000000000ULL, /* getf.s: predicated */
  0x0000001000000000ULL, /* getf.d: predicated */
  0x0000001000000000ULL, /* getf.exp: predicated */
  0x0000001000000000ULL, /* getf.sig: predicated */
  0x0000001000000000ULL, /* invala: predicated */
  0x0000001000000000ULL, /* invala.e: predicated */
  0x0000001000000000ULL, /* invala_f.e: predicated */
  0x0000001020000000ULL, /* itc.i: privileged predicated */
  0x0000001020000000ULL, /* itc.d: privileged predicated */
  0x0000001020000000ULL, /* itr.i: privileged predicated */
  0x0000001020000000ULL, /* itr.d: privileged predicated */
  0x0000001004000001ULL, /* ld1: load memtrap predicated */
  0x0000001004000101ULL, /* ld1_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ld1_i: load same_res memtrap predicated */
  0x0000001004000001ULL, /* ld2: load memtrap predicated */
  0x0000001004000101ULL, /* ld2_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ld2_i: load same_res memtrap predicated */
  0x0000001004000001ULL, /* ld4: load memtrap predicated */
  0x0000001004000101ULL, /* ld4_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ld4_i: load same_res memtrap predicated */
  0x0000001004000001ULL, /* ld8: load memtrap predicated */
  0x0000001004000101ULL, /* ld8_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ld8_i: load same_res memtrap predicated */
  0x0000001004000009ULL, /* ld8.fill: load mem_fill_type memtrap predicated */
  0x0000001004000109ULL, /* ld8_r.fill: load mem_fill_type same_res memtrap predicated */
  0x0000001004000109ULL, /* ld8_i.fill: load mem_fill_type same_res memtrap predicated */
  0x0000001004000001ULL, /* ldfs: load memtrap predicated */
  0x0000001004000101ULL, /* ldfs_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ldfs_i: load same_res memtrap predicated */
  0x0000001004000001ULL, /* ldfd: load memtrap predicated */
  0x0000001004000101ULL, /* ldfd_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ldfd_i: load same_res memtrap predicated */
  0x0000001004000001ULL, /* ldfe: load memtrap predicated */
  0x0000001004000101ULL, /* ldfe_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ldfe_i: load same_res memtrap predicated */
  0x0000001004000001ULL, /* ldf8: load memtrap predicated */
  0x0000001004000101ULL, /* ldf8_r: load same_res memtrap predicated */
  0x0000001004000101ULL, /* ldf8_i: load same_res memtrap predicated */
  0x0000001004000009ULL, /* ldf.fill: load mem_fill_type memtrap predicated */
  0x0000001004000109ULL, /* ldf_r.fill: load mem_fill_type same_res memtrap predicated */
  0x0000001004000109ULL, /* ldf_i.fill: load mem_fill_type same_res memtrap predicated */
  0x0000001004000001ULL, /* ldfps: load memtrap predicated */
  0x0000001004000001ULL, /* ldfps_i: load memtrap predicated */
  0x0000001004000001ULL, /* ldfpd: load memtrap predicated */
  0x0000001004000001ULL, /* ldfpd_i: load memtrap predicated */
  0x0000001004000001ULL, /* ldfp8: load memtrap predicated */
  0x0000001004000001ULL, /* ldfp8_i: load memtrap predicated */
  0x0000001000000004ULL, /* lfetch: prefetch predicated */
  0x0000001000000004ULL, /* lfetch.excl: prefetch predicated */
  0x0000001000000000ULL, /* lfetch.fault: predicated */
  0x0000001000000000ULL, /* lfetch.fault.excl: predicated */
  0x0000001000000104ULL, /* lfetch_r: prefetch same_res predicated */
  0x0000001000000104ULL, /* lfetch_r.excl: prefetch same_res predicated */
  0x0000001000000100ULL, /* lfetch_r.fault: same_res predicated */
  0x0000001000000100ULL, /* lfetch_r.fault.excl: same_res predicated */
  0x0000001000000104ULL, /* lfetch_i: prefetch same_res predicated */
  0x0000001000000104ULL, /* lfetch_i.excl: prefetch same_res predicated */
  0x0000001000000100ULL, /* lfetch_i.fault: same_res predicated */
  0x0000001000000100ULL, /* lfetch_i.fault.excl: same_res predicated */
  0x0000000008000000ULL, /* loadrs: f_group */
  0x0000001000000000ULL, /* mf: predicated */
  0x0000001000000000ULL, /* mf.a: predicated */
  0x0000001000080000ULL, /* mix1.l: mmshf predicated */
  0x0000001000080000ULL, /* mix1.r: mmshf predicated */
  0x0000001000080000ULL, /* mix2.l: mmshf predicated */
  0x0000001000080000ULL, /* mix2.r: mmshf predicated */
  0x0000001000080000ULL, /* mix4.l: mmshf predicated */
  0x0000001000080000ULL, /* mix4.r: mmshf predicated */
  0x0000001000000000ULL, /* mov_f_ar.i: predicated */
  0x0000001000000000ULL, /* mov_t_ar_r.i: predicated */
  0x0000001000000000ULL, /* mov_t_ar_i.i: predicated */
  0x0000001000000000ULL, /* mov_f_ar.m: predicated */
  0x0000001000000000ULL, /* mov_t_ar_r.m: predicated */
  0x0000001000000000ULL, /* mov_t_ar_i.m: predicated */
  0x0000001000000000ULL, /* mov_f_br: predicated */
  0x0000001000000000ULL, /* mov_t_br_i: predicated */
  0x0000001000000000ULL, /* mov_t_br.ret: predicated */
  0x0000001000000000ULL, /* mov_t_cr: predicated */
  0x0000001000000000ULL, /* mov_f_cr: predicated */
  0x0000001000000000ULL, /* mov_f_cpuid: predicated */
  0x0000001000000000ULL, /* mov_t_dbr: predicated */
  0x0000001000000000ULL, /* mov_f_dbr: predicated */
  0x0000001000000000ULL, /* mov_t_ibr: predicated */
  0x0000001000000000ULL, /* mov_f_ibr: predicated */
  0x0000001000000000ULL, /* mov_t_msr: predicated */
  0x0000001000000000ULL, /* mov_f_msr: predicated */
  0x0000001000000000ULL, /* mov_t_pkr: predicated */
  0x0000001000000000ULL, /* mov_f_pkr: predicated */
  0x0000001000000000ULL, /* mov_t_pmc: predicated */
  0x0000001000000000ULL, /* mov_f_pmc: predicated */
  0x0000001000000000ULL, /* mov_t_pmd: predicated */
  0x0000001000000000ULL, /* mov_f_pmd: predicated */
  0x0000001000000000ULL, /* mov_t_rr: predicated */
  0x0000001000000000ULL, /* mov_f_rr: predicated */
  0x0000001000000000ULL, /* mov_f_ip: predicated */
  0x0000003000000000ULL, /* mov_f_pr: predicated access_reg_bank */
  0x0000003000000000ULL, /* mov_t_pr: predicated access_reg_bank */
  0x0000003000000000ULL, /* mov_t_pr_i: predicated access_reg_bank */
  0x0000001000000000ULL, /* mov_t_psr: predicated */
  0x0000001000000000ULL, /* mov_f_psr: predicated */
  0x0000001000000000ULL, /* mov_t_psrum: predicated */
  0x0000001000000000ULL, /* mov_f_psrum: predicated */
  0x0000001000000000ULL, /* movl: predicated */
  0x0000001000080000ULL, /* mux1: mmshf predicated */
  0x0000001000080000ULL, /* mux2: mmshf predicated */
  0x0000001000200000ULL, /* nop.i: noop predicated */
  0x0000001000200000ULL, /* nop.b: noop predicated */
  0x0000001000200000ULL, /* nop.m: noop predicated */
  0x0000001000200000ULL, /* nop.f: noop predicated */
  0x0000001000200000ULL, /* nop.x: noop predicated */
  0x0000001040000000ULL, /* or: ior predicated */
  0x0000001040000000ULL, /* or_i: ior predicated */
  0x0000001000080000ULL, /* pack2.sss: mmshf predicated */
  0x0000001000080000ULL, /* pack2.uss: mmshf predicated */
  0x0000001000080000ULL, /* pack4.sss: mmshf predicated */
  0x0000001000040000ULL, /* padd1: mmalu predicated */
  0x0000001000040000ULL, /* padd1.sss: mmalu predicated */
  0x0000001000040000ULL, /* padd1.uus: mmalu predicated */
  0x0000001000040000ULL, /* padd1.uuu: mmalu predicated */
  0x0000001000040000ULL, /* padd2: mmalu predicated */
  0x0000001000040000ULL, /* padd2.sss: mmalu predicated */
  0x0000001000040000ULL, /* padd2.uus: mmalu predicated */
  0x0000001000040000ULL, /* padd2.uuu: mmalu predicated */
  0x0000001000040000ULL, /* padd4: mmalu predicated */
  0x0000001000040000ULL, /* pavg1: mmalu predicated */
  0x0000001000040000ULL, /* pavg1.raz: mmalu predicated */
  0x0000001000040000ULL, /* pavg2: mmalu predicated */
  0x0000001000040000ULL, /* pavg2.raz: mmalu predicated */
  0x0000001000040000ULL, /* pavgsub1: mmalu predicated */
  0x0000001000040000ULL, /* pavgsub2: mmalu predicated */
  0x0000001000040000ULL, /* pcmp1.eq: mmalu predicated */
  0x0000001000040000ULL, /* pcmp1.gt: mmalu predicated */
  0x0000001000040000ULL, /* pcmp2.eq: mmalu predicated */
  0x0000001000040000ULL, /* pcmp2.gt: mmalu predicated */
  0x0000001000040000ULL, /* pcmp4.eq: mmalu predicated */
  0x0000001000040000ULL, /* pcmp4.gt: mmalu predicated */
  0x0000001000040000ULL, /* pmax1.u: mmalu predicated */
  0x0000001000040000ULL, /* pmax2: mmalu predicated */
  0x0000001000040000ULL, /* pmin1.u: mmalu predicated */
  0x0000001000040000ULL, /* pmin2: mmalu predicated */
  0x0000001000100000ULL, /* pmpy2.r: mmmul predicated */
  0x0000001000100000ULL, /* pmpy2.l: mmmul predicated */
  0x0000001000100000ULL, /* pmpyshr2: mmmul predicated */
  0x0000001000100000ULL, /* pmpyshr2.u: mmmul predicated */
  0x0000001000100000ULL, /* popcnt: mmmul predicated */
  0x0000001000000000ULL, /* probe.r: predicated */
  0x0000001000000000ULL, /* probe.w: predicated */
  0x0000001000000000ULL, /* probe_i.r: predicated */
  0x0000001000000000ULL, /* probe_i.w: predicated */
  0x0000001000000000ULL, /* probe.r.fault: predicated */
  0x0000001000000000ULL, /* probe.w.fault: predicated */
  0x0000001000000000ULL, /* probe.rw.fault: predicated */
  0x0000001000040000ULL, /* psad1: mmalu predicated */
  0x0000001000080000ULL, /* pshl2: mmshf predicated */
  0x0000001000080000ULL, /* pshl4: mmshf predicated */
  0x0000001000080000ULL, /* pshl2_i: mmshf predicated */
  0x0000001000080000ULL, /* pshl4_i: mmshf predicated */
  0x0000001000040000ULL, /* pshladd2: mmalu predicated */
  0x0000001000080000ULL, /* pshr2: mmshf predicated */
  0x0000001000080000ULL, /* pshr2.u: mmshf predicated */
  0x0000001000080000ULL, /* pshr4: mmshf predicated */
  0x0000001000080000ULL, /* pshr4.u: mmshf predicated */
  0x0000001000080000ULL, /* pshr2_i: mmshf predicated */
  0x0000001000080000ULL, /* pshr2_i.u: mmshf predicated */
  0x0000001000080000ULL, /* pshr4_i: mmshf predicated */
  0x0000001000080000ULL, /* pshr4_i.u: mmshf predicated */
  0x0000001000040000ULL, /* pshradd2: mmalu predicated */
  0x0000001000040000ULL, /* psub1: mmalu predicated */
  0x0000001000040000ULL, /* psub2: mmalu predicated */
  0x0000001000040000ULL, /* psub4: mmalu predicated */
  0x0000001000040000ULL, /* psub1.sss: mmalu predicated */
  0x0000001000040000ULL, /* psub1.uus: mmalu predicated */
  0x0000001000040000ULL, /* psub1.uuu: mmalu predicated */
  0x0000001000040000ULL, /* psub2.sss: mmalu predicated */
  0x0000001000040000ULL, /* psub2.uus: mmalu predicated */
  0x0000001000040000ULL, /* psub2.uuu: mmalu predicated */
  0x0000001020000000ULL, /* ptc.e: privileged predicated */
  0x0000001020000000ULL, /* ptc.g: privileged predicated */
  0x0000001020000000ULL, /* ptc.ga: privileged predicated */
  0x0000001020000000ULL, /* ptc.l: privileged predicated */
  0x0000001020000000ULL, /* ptr.d: privileged predicated */
  0x0000001020000000ULL, /* ptr.i: privileged predicated */
  0x0000000030000000ULL, /* rfi: l_group privileged */
  0x0000001020000000ULL, /* rsm: privileged predicated */
  0x0000001000000000ULL, /* rum: predicated */
  0x0000001000000000ULL, /* setf.s: predicated */
  0x0000001000000000ULL, /* setf.d: predicated */
  0x0000001000000000ULL, /* setf.exp: predicated */
  0x0000001000000000ULL, /* setf.sig: predicated */
  0x0000001000080000ULL, /* shl: mmshf predicated */
  0x0000001000000000ULL, /* shladd: predicated */
  0x0000001000000000ULL, /* shladdp4: predicated */
  0x0000001000080000ULL, /* shr: mmshf predicated */
  0x0000001000080000ULL, /* shr.u: mmshf predicated */
  0x0000001000000000ULL, /* shrp: predicated */
  0x0000001000000000ULL, /* srlz.i: predicated */
  0x0000001000000000ULL, /* srlz.d: predicated */
  0x0000001020000000ULL, /* ssm: privileged predicated */
  0x0000001004000002ULL, /* st1: store memtrap predicated */
  0x0000001004000102ULL, /* st1_i: store same_res memtrap predicated */
  0x0000001004000002ULL, /* st2: store memtrap predicated */
  0x0000001004000102ULL, /* st2_i: store same_res memtrap predicated */
  0x0000001004000002ULL, /* st4: store memtrap predicated */
  0x0000001004000102ULL, /* st4_i: store same_res memtrap predicated */
  0x0000001004000002ULL, /* st8: store memtrap predicated */
  0x0000001004000102ULL, /* st8_i: store same_res memtrap predicated */
  0x000000100400000aULL, /* st8.spill: store mem_fill_type memtrap predicated */
  0x000000100400010aULL, /* st8_i.spill: store mem_fill_type same_res memtrap predicated */
  0x0000001004000002ULL, /* stfs: store memtrap predicated */
  0x0000001004000102ULL, /* stfs_i: store same_res memtrap predicated */
  0x0000001004000002ULL, /* stfd: store memtrap predicated */
  0x0000001004000102ULL, /* stfd_i: store same_res memtrap predicated */
  0x0000001004000002ULL, /* stfe: store memtrap predicated */
  0x0000001004000102ULL, /* stfe_i: store same_res memtrap predicated */
  0x0000001004000002ULL, /* stf8: store memtrap predicated */
  0x0000001004000102ULL, /* stf8_i: store same_res memtrap predicated */
  0x000000100400000aULL, /* stf.spill: store mem_fill_type memtrap predicated */
  0x000000100400010aULL, /* stf_i.spill: store mem_fill_type same_res memtrap predicated */
  0x0000001000000400ULL, /* sub: isub predicated */
  0x0000001000000000ULL, /* sub_1: predicated */
  0x0000001000000400ULL, /* sub_i: isub predicated */
  0x0000001000000000ULL, /* sum: predicated */
  0x0000001000000000ULL, /* sxt1: predicated */
  0x0000001000000000ULL, /* sxt2: predicated */
  0x0000001000000000ULL, /* sxt4: predicated */
  0x0000001000000000ULL, /* sync.i: predicated */
  0x0000001020000000ULL, /* tak: privileged predicated */
  0x0000001000000000ULL, /* tbit.z: predicated */
  0x0000001000000000ULL, /* tbit.z.unc: predicated */
  0x0000001000000000ULL, /* tbit.z.and: predicated */
  0x0000001000000000ULL, /* tbit.z.or: predicated */
  0x0000001000000000ULL, /* tbit.z.or.andcm: predicated */
  0x0000001000000000ULL, /* tbit.nz.and: predicated */
  0x0000001000000000ULL, /* tbit.nz.or: predicated */
  0x0000001000000000ULL, /* tbit.nz.or.andcm: predicated */
  0x0000001000000000ULL, /* thash: predicated */
  0x0000001000000000ULL, /* tnat.z: predicated */
  0x0000001000000000ULL, /* tnat.z.unc: predicated */
  0x0000001000000000ULL, /* tnat.z.and: predicated */
  0x0000001000000000ULL, /* tnat.z.or: predicated */
  0x0000001000000000ULL, /* tnat.z.or.andcm: predicated */
  0x0000001000000000ULL, /* tnat.nz.and: predicated */
  0x0000001000000000ULL, /* tnat.nz.or: predicated */
  0x0000001000000000ULL, /* tnat.nz.or.andcm: predicated */
  0x0000001020000000ULL, /* tpa: privileged predicated */
  0x0000001000000000ULL, /* ttag: predicated */
  0x0000001000080000ULL, /* unpack1.l: mmshf predicated */
  0x0000001000080000ULL, /* unpack1.h: mmshf predicated */
  0x0000001000080000ULL, /* unpack2.l: mmshf predicated */
  0x0000001000080000ULL, /* unpack2.h: mmshf predicated */
  0x0000001000080000ULL, /* unpack4.l: mmshf predicated */
  0x0000001000080000ULL, /* unpack4.h: mmshf predicated */
  0x0000001000000000ULL, /* xchg1: predicated */
  0x0000001000000000ULL, /* xchg2: predicated */
  0x0000001000000000ULL, /* xchg4: predicated */
  0x0000001000000000ULL, /* xchg8: predicated */
  0x0000001000000800ULL, /* xma.l: imul predicated */
  0x0000001000000800ULL, /* xma.h: imul predicated */
  0x0000001000000800ULL, /* xma.hu: imul predicated */
  0x0000001100000000ULL, /* xor: ixor predicated */
  0x0000001100000000ULL, /* xor_i: ixor predicated */
  0x0000001000000000ULL, /* zxt1: predicated */
  0x0000001000000000ULL, /* zxt2: predicated */
  0x0000001000000000ULL, /* zxt4: predicated */
  0x0000004008000000ULL, /* alloc: f_group side_effects */
  0x0000000000000010ULL, /* br: xfer */
  0x0000000080000010ULL, /* br_r: xfer ijump */
  0x0000000000000010ULL, /* brl: xfer */
  0x0000001400000000ULL, /* cmp.eq.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.eq.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.eq.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.ne.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp.le: icmp predicated */
  0x0000001400000000ULL, /* cmp.le.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.gt: icmp predicated */
  0x0000001400000000ULL, /* cmp.gt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.ge: icmp predicated */
  0x0000001400000000ULL, /* cmp.ge.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.leu: icmp predicated */
  0x0000001400000000ULL, /* cmp.leu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.gtu: icmp predicated */
  0x0000001400000000ULL, /* cmp.gtu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp.geu: icmp predicated */
  0x0000001400000000ULL, /* cmp.geu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.lt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.lt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.lt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.le.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.le.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.le.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.gt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.gt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.gt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.ge.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.ge.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z1.ge.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.lt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.lt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.lt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.le.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.le.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.le.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.gt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.gt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.gt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.ge.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.ge.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.ge.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.lt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.lt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.lt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.le.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.le.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.le.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.gt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.gt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.gt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.ge.and: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.ge.or: icmp predicated */
  0x0000001400000000ULL, /* cmp_z2.ge.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.eq.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ne.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.le: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.le.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.gt: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.gt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ge: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.ge.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.leu: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.leu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.gtu: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.gtu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.geu: icmp predicated */
  0x0000001400000000ULL, /* cmp_i.geu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.eq.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ne.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4.le: icmp predicated */
  0x0000001400000000ULL, /* cmp4.le.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.gt: icmp predicated */
  0x0000001400000000ULL, /* cmp4.gt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ge: icmp predicated */
  0x0000001400000000ULL, /* cmp4.ge.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.leu: icmp predicated */
  0x0000001400000000ULL, /* cmp4.leu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.gtu: icmp predicated */
  0x0000001400000000ULL, /* cmp4.gtu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4.geu: icmp predicated */
  0x0000001400000000ULL, /* cmp4.geu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.lt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.lt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.lt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.le.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.le.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.le.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.gt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.gt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.gt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.ge.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.ge.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z1.ge.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.lt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.lt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.lt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.le.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.le.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.le.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.gt.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.gt.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.gt.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.ge.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.ge.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.ge.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.lt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.lt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.lt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.le.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.le.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.le.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.gt.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.gt.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.gt.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.ge.and: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.ge.or: icmp predicated */
  0x0000001400000000ULL, /* cmp4_z2.ge.or.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.eq.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.andcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ne.and.orcm: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.le: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.le.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.gt: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.gt.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ge: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.ge.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.leu: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.leu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.gtu: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.gtu.unc: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.geu: icmp predicated */
  0x0000001400000000ULL, /* cmp4_i.geu.unc: icmp predicated */
  0x0000001000011000ULL, /* fabs: flop fmisc predicated */
  0x0000001000003000ULL, /* fadd: flop fadd predicated */
  0x0000001000003000ULL, /* fadd.s: flop fadd predicated */
  0x0000001000003000ULL, /* fadd.d: flop fadd predicated */
  0x0000001000000000ULL, /* fclass.nm: predicated */
  0x0000001000000000ULL, /* fclass.nm.unc: predicated */
  0x0000001000001000ULL, /* fcmp.gt: flop predicated */
  0x0000001000001000ULL, /* fcmp.gt.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.ge: flop predicated */
  0x0000001000001000ULL, /* fcmp.ge.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.neq: flop predicated */
  0x0000001000001000ULL, /* fcmp.neq.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.nlt: flop predicated */
  0x0000001000001000ULL, /* fcmp.nlt.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.nle: flop predicated */
  0x0000001000001000ULL, /* fcmp.nle.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.ngt: flop predicated */
  0x0000001000001000ULL, /* fcmp.ngt.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.nge: flop predicated */
  0x0000001000001000ULL, /* fcmp.nge.unc: flop predicated */
  0x0000001000001000ULL, /* fcmp.ord: flop predicated */
  0x0000001000001000ULL, /* fcmp.ord.unc: flop predicated */
  0x0000001000000000ULL, /* fcvt.xuf: predicated */
  0x0000001000000000ULL, /* fcvt.xuf.s: predicated */
  0x0000001000000000ULL, /* fcvt.xuf.d: predicated */
  0x0000001000009000ULL, /* fmpy: flop fmul predicated */
  0x0000001000009000ULL, /* fmpy.s: flop fmul predicated */
  0x0000001000009000ULL, /* fmpy.d: flop fmul predicated */
  0x0000001000011000ULL, /* fneg: flop fmisc predicated */
  0x0000001000011000ULL, /* fnegabs: flop fmisc predicated */
  0x0000001000009000ULL, /* fnmpy: flop fmul predicated */
  0x0000001000009000ULL, /* fnmpy.s: flop fmul predicated */
  0x0000001000009000ULL, /* fnmpy.d: flop fmul predicated */
  0x0000001000001000ULL, /* fnorm: flop predicated */
  0x0000001000001000ULL, /* fnorm.s: flop predicated */
  0x0000001000001000ULL, /* fnorm.d: flop predicated */
  0x0000001000001000ULL, /* fpabs: flop predicated */
  0x0000001000001000ULL, /* fpcmp.gt: flop predicated */
  0x0000001000001000ULL, /* fpcmp.ge: flop predicated */
  0x0000001000001000ULL, /* fpcmp.ngt: flop predicated */
  0x0000001000001000ULL, /* fpcmp.nge: flop predicated */
  0x0000001000009000ULL, /* fpmpy: flop fmul predicated */
  0x0000001000001000ULL, /* fpneg: flop predicated */
  0x0000001000001000ULL, /* fpnegabs: flop predicated */
  0x0000001000009000ULL, /* fpnmpy: flop fmul predicated */
  0x0000001000005000ULL, /* fsub: flop fsub predicated */
  0x0000001000005000ULL, /* fsub.s: flop fsub predicated */
  0x0000001000005000ULL, /* fsub.d: flop fsub predicated */
  0x0000001000000000ULL, /* mov_t_br: predicated */
  0x0000001000011000ULL, /* mov_f: flop fmisc predicated */
  0x0000001020000000ULL, /* mov: privileged predicated */
  0x0000001000000000ULL, /* mov_i: predicated */
  0x0000001000000000ULL, /* shl_i: predicated */
  0x0000001000000000ULL, /* shr_i: predicated */
  0x0000001000000000ULL, /* shr_i.u: predicated */
  0x0000001000000000ULL, /* tbit.nz: predicated */
  0x0000001000000000ULL, /* tbit.nz.unc: predicated */
  0x0000001000000000ULL, /* tnat.nz: predicated */
  0x0000001000000000ULL, /* tnat.nz.unc: predicated */
  0x0000001000000800ULL, /* xma.lu: imul predicated */
  0x0000001000000800ULL, /* xmpy.l: imul predicated */
  0x0000001000000800ULL, /* xmpy.lu: imul predicated */
  0x0000001000000800ULL, /* xmpy.h: imul predicated */
  0x0000001000000800ULL, /* xmpy.hu: imul predicated */
  0x0000001800000010ULL, /* break: xfer simulated predicated */
  0x0000001800000000ULL, /* chk.s: simulated predicated */
  0x0000001c00000000ULL, /* cmp.lt.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.lt.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.lt.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.le.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.le.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.le.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.gt.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.gt.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.gt.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.ge.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.ge.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.ge.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.lt.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.lt.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.lt.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.le.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.le.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.le.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.gt.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.gt.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.gt.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.ge.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.ge.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp.ge.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.lt.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.lt.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.lt.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.le.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.le.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.le.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.gt.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.gt.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.gt.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.ge.and: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.ge.or: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.ge.or.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.lt.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.lt.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.lt.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.le.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.le.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.le.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.gt.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.gt.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.gt.and.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.ge.orcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.ge.andcm: icmp simulated predicated */
  0x0000001c00000000ULL, /* cmp4.ge.and.orcm: icmp simulated predicated */
  0x0000001800000000ULL, /* mov_f_ar: simulated predicated */
  0x0000001800000000ULL, /* mov_t_ar_r: simulated predicated */
  0x0000001800000000ULL, /* mov_t_ar_i: simulated predicated */
  0x0000001800200000ULL, /* nop: noop simulated predicated */
  0x0000010800000000ULL, /* asm: simulated var_opnds */
  0x0000010800000080ULL, /* intrncall: uniq_res simulated var_opnds */
  0x0000001800000200ULL, /* spadjust: iadd simulated predicated */
  0x0000001800000000ULL, /* copy.br: simulated predicated */
  0x0000000000400000ULL, /* begin.pregtn: dummy */
  0x0000000000400000ULL, /* end.pregtn: dummy */
  0x0000000000c00000ULL, /* bwd.bar: dummy unsafe */
  0x0000000000c00000ULL, /* fwd.bar: dummy unsafe */
  0x0000000000400000ULL, /* dfixup: dummy */
  0x0000000000400000ULL, /* ffixup: dummy */
  0x0000000000400000ULL, /* ifixup: dummy */
  0x0000000000400000ULL, /* label: dummy */
  0x0000000000600000ULL, /* noop: noop dummy */
};
