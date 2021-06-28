/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lisp_file_08.c
 */
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
#endif
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#define LISP_AMALGAMATION
#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif

#include <memory.h>
#include <string.h>

#include "lisp_file.h"


/************************************************************
 *  real_plus.c
 ************************************************************/

/*
 *  sign_reverse
 */
int sign_reverse_real_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_common(pos, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_heap(pos, ret);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_heap(pos, ret);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_heap(pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}

int sign_reverse_real_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_local(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_local(local, pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_local(local, pos, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_local(local, pos, ret);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_local(local, pos, ret);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_local(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}


/*
 *  1+, 1-
 */
int oneplus_real_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, 1, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_sv_heap_(value, 1.0f, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dv_heap_(value, 1.0, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_lv_heap_(value, 1.0L, ret);

		default:
			*ret = Nil;
			return TypeError_(value, REAL);
	}
}

int oneminus_real_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, -1, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_sv_heap_(value, -1.0f, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dv_heap_(value, -1.0, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_lv_heap_(value, -1.0L, ret);

		default:
			*ret = Nil;
			return TypeError_(value, REAL);
	}
}


/*
 *  plus
 */
int plus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_single_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_sl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_double_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_dl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_long_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_single_real_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_double_real_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_long_real_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

static int plus_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_fr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_fs_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_fd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_fl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_br_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_bs_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_bd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_bl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_rs_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_rd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_rl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_single_float_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_alloc_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_alloc_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_alloc_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ss_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_sd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_sl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_double_float_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_alloc_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_alloc_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_alloc_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ds_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_dl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_long_float_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_alloc_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_alloc_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_alloc_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ls_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_ld_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_ll_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int plus_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_single_float_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_double_float_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_long_float_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}


/*
 *  minus
 */
int minus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_lf_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int minus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_lb_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int minus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sr_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dr_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int minus_single_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_sl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_single_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rs_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ds_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ls_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int minus_double_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_dl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_double_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rd_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sd_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ld_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int minus_long_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_long_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rl_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sl_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dl_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int minus_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_single_real_common_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_double_real_common_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_long_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

static int minus_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_fr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_fs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_fd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_fl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_br_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_bs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_bd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_bl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_rs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_rd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_rl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_single_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_sd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_sl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_double_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ds_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_dl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_long_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ls_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ld_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int minus_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_single_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_double_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_long_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}


/************************************************************
 *  real_round.c
 ************************************************************/

/*
 *  common
 */
static int round1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_round1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int round1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_round1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int round1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_round1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

int round1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return float_round1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return round1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return round1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int fround1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_round1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int fround1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_round1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int fround1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_round1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

int fround1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(quot, left);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			Return(single_float_bignum_heap_(quot, left));
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return float_fround1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return fround1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int round_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_round_fixnum_(quot, rem, a, b);
}

static int round_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_round_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_round_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_round_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_round_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_round_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_round_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_round_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_round_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int round2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return round_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int fround_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_fround_fixnum_(quot, rem, a, b);
}

static int fround_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fround_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fround_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_fd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fround_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fround_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fround_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fround_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fround_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fround_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int fround2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return fround_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
			break;
	}
}

int round_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return round1_common_(local, ret1, ret2, var);
	else
		return round2_common_(local, ret1, ret2, var, div);
}

int fround_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return fround1_common_(local, ret1, ret2, var);
	else
		return fround2_common_(local, ret1, ret2, var, div);
}


/************************************************************
 *  real_truncate.c
 ************************************************************/

/*
 *  common
 */
static int truncate1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_truncate1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int truncate1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_truncate1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int truncate1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_truncate1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

int truncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return float_truncate1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return truncate1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int ftruncate1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_truncate1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int ftruncate1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_truncate1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int ftruncate1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_truncate1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

int ftruncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(quot, left);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			Return(single_float_bignum_heap_(quot, left));
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return float_ftruncate1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int truncate_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_truncate_fixnum_(quot, rem, a, b);
}

static int truncate_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_truncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_truncate_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_truncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_truncate_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_truncate_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_truncate_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_truncate_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_truncate_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int truncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return truncate_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int ftruncate_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_ftruncate_fixnum_(quot, rem, a, b);
}

static int ftruncate_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ftruncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ftruncate_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_fd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ftruncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ftruncate_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ftruncate_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ftruncate_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ftruncate_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ftruncate_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int ftruncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

int truncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return truncate1_common_(local, ret1, ret2, var);
	else
		return truncate2_common_(local, ret1, ret2, var, div);
}

int ftruncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return ftruncate1_common_(local, ret1, ret2, var);
	else
		return ftruncate2_common_(local, ret1, ret2, var, div);
}


/*
 *  rem
 */
static int rem_ff_common_(addr *ret, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_rem_fixnum_(ret, a, b);
}

static int rem_fb_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_rem_bignum_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_fr_common_(LocalRoot local, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_rem_br_ratio_(local, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_bf_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_rem_bignum_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_rf_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_rem_rb_ratio_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return rem_ff_common_(ret, left, right);

		case LISPTYPE_BIGNUM:
			return rem_fb_common_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return rem_fr_common_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int rem_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return rem_bf_common_(local, ret, left, right);

		case LISPTYPE_BIGNUM:
			return float_rem_bignum_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return float_rem_br_ratio_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int rem_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return rem_rf_common_(local, ret, left, right);

		case LISPTYPE_BIGNUM:
			return float_rem_rb_ratio_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return float_rem_rr_ratio_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

int rem_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return rem_fixnum_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return rem_bignum_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return rem_ratio_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/************************************************************
 *  require.c
 ************************************************************/

/*
 *  provide
 */
int provide_common_(Execute ptr, addr var)
{
	addr symbol, list;

	/* string-designer */
	Return(string_designer_heap_(&var, var, NULL));
	/* push *modules */
	GetConst(SPECIAL_MODULES, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(pushnew_equal_heap_(list, var, &list));
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  require
 */
static int require_function_common_call_(Execute ptr, addr call, addr var, int *ret)
{
	if (symbolp(call)) {
		Return(function_global_restart(ptr, call, &call));
	}
	Return(funcall_control(ptr, call, var, NULL));
	getresult_control(ptr, &var);
	return Result(ret, (var == Nil)? 0: 1);
}

static int require_function_common(Execute ptr, addr var, int *ret)
{
	int check;
	addr list, call, control;

	/* lisp-system::*module-provider-functions* */
	GetConst(SYSTEM_MODULE_PROVIDER_FUNCTIONS, &list);
	Return(getspecialcheck_local_(ptr, list, &list));

	while (list != Nil) {
		Return_getcons(list, &call, &list);
		/* funcall */
		push_control(ptr, &control);
		(void)require_function_common_call_(ptr, call, var, &check);
		Return(pop_control_(ptr, control));
		/* check */
		if (check)
			return Result(ret, 1);
	}

	/* error */
	return fmte_("Cannot require ~S.", var, NULL);
}

static int require_list_common(Execute ptr, addr var, addr list, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(eval_load_(ptr, &check, x, Unbound, Unbound, 1, Unbound));
	}

	*ret = 1;
	return 0;
}

int require_common(Execute ptr, addr var, addr opt)
{
	int check, push;

	Return(string_designer_heap_(&var, var, NULL));
	push = 0;
	if (opt == Unbound || opt == Nil) {
		check = require_function_common(ptr, var, &push);
	}
	else {
		if (! listp(opt))
			conscar_heap(&opt, opt);
		check = require_list_common(ptr, var, opt, &push);
	}
	if (check)
		return 1;
	if (push) {
		Return(provide_common_(ptr, var));
	}

	return 0;
}


/*
 *  build
 */
void build_require(void)
{
	addr symbol;
	GetConst(SYSTEM_MODULE_PROVIDER_FUNCTIONS, &symbol);
	SetValueSymbol(symbol, Nil);
}


/************************************************************
 *  restart.c
 ************************************************************/

enum Restart_Index {
	Restart_Name,
	Restart_Function,
	Restart_Interactive,
	Restart_Report,
	Restart_Test,
	Restart_Condition,
	Restart_Reference,
	Restart_Size
};

#define RefRestart		RefArrayA2
#define GetRestart		GetArrayA2
#define SetRestart		SetArrayA2

int restartp(addr pos)
{
	return GetType(pos) == LISPTYPE_RESTART;
}

void restart_heap(addr *ret, addr name)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_RESTART, Restart_Size);
	SetUser(pos, 0);
	setenable_restart(pos, 1);
	SetRestart(pos, Restart_Name, name);

	*ret = pos;
}

void getname_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Name, ret);
}

void setname_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Name, value);
}

void getfunction_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Function, ret);
}

void setfunction_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Function, value);
}

void getinteractive_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Interactive, ret);
}

void setinteractive_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Interactive, value);
}

void getreport_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Report, ret);
}

void setreport_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Report, value);
}

void gettest_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Test, ret);
}

void settest_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Test, value);
}

void getcondition_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Condition, ret);
}

void setcondition_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Condition, value);
}

void getreference_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Reference, ret);
}

void setreference_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Reference, value);
}

void setescape_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 0, value);
	SetUser(pos, u);
}

int getescape_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 0);
}

void setenable_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 1, value);
	SetUser(pos, u);
}

int getenable_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 1);
}

void setredirect_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 2, value);
	SetUser(pos, u);
}

int getredirect_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 2);
}


/*
 *  initialize
 */
void init_restart(void)
{
	init_restart_value();
}


/************************************************************
 *  restart_value.c
 ************************************************************/

/*
 *  symbol restart
 */
static int restart_symbol_use_function(Execute ptr, addr value)
{
	setresult_control(ptr, value);
	return 0;
}

static int restart_symbol_use_interactive_char_(Execute ptr, const char *str)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, str);
	Return(prompt_for_stream(ptr, T, prompt, &eval));
	Return(eval_result_partial_form_(ptr, eval, &eval));
	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_symbol_use_interactive(Execute ptr)
{
	return restart_symbol_use_interactive_char_(ptr, "Use new value: ");
}

static int restart_symbol_use_test(Execute ptr, addr pos)
{
	setresult_control(ptr, T);
	return 0;
}

static void symbol_use_restart(addr *ret)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_function);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_symbol_use_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Use specific value.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int restart_symbol_store_special(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
	setresult_control(ptr, value);

	return 0;
}

static int restart_symbol_store_interactive(Execute ptr)
{
	return restart_symbol_use_interactive_char_(ptr, "Store new value: ");
}

static void symbol_store_restart(addr *ret, addr symbol)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, symbol);
	setcompiled_var1(value, p_restart_symbol_store_special);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_symbol_store_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Store specific value.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int symbol_restart_call_(Execute ptr, addr symbol, addr *ret)
{
	addr control, restart1, restart2;

	symbol_use_restart(&restart1);
	symbol_store_restart(&restart2, symbol);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	(void)call_unbound_variable_(ptr, symbol);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use-value */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		goto escape;
	}

	/* store-value */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);
	return 0;
}

int symbol_special_restart(Execute ptr, addr symbol, addr *ret)
{
	addr value;

	getspecial_local(ptr, symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return symbol_restart_call_(ptr, symbol, ret);
}


/*
 *  function restart
 */
static int restart_function_use_function(Execute ptr, addr value)
{
	if (! functionp(value))
		return TypeError_(value, FUNCTION);
	setresult_control(ptr, value);

	return 0;
}

static int restart_function_use_interactive_char_(Execute ptr, const char *str)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Use new function: ");
	for (;;) {
		Return(prompt_for_stream(ptr, T, prompt, &eval));
		Return(eval_result_partial_form_(ptr, eval, &eval));
		if (functionp(eval))
			break;
		/* error */
		strvect_char_heap(&prompt, "Please answer FUNCTION type: ");
	}

	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_function_use_interactive(Execute ptr)
{
	return restart_function_use_interactive_char_(ptr, "Use new function: ");
}

static void function_use_restart(addr *ret)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_function_use_function);
	setfunction_restart(pos, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_function_use_interactive);
	setinteractive_restart(pos, value);
	/* report */
	strvect_char_heap(&value, "Use specific function.");
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

static int restart_function_store_function(Execute ptr, addr value)
{
	addr name;

	if (! functionp(value))
		return TypeError_(value, FUNCTION);

	getdata_control(ptr, &name);
	Return(setglobal_callname_(name, value));
	setresult_control(ptr, value);

	return 0;
}

static int restart_function_store_interactive(Execute ptr)
{
	return restart_function_use_interactive_char_(ptr, "Store new function: ");
}

static void function_store_restart(addr *ret, addr name)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, name);
	setcompiled_var1(value, p_restart_function_store_function);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_function_store_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Store specific function.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int function_restart_call(Execute ptr, addr name, addr *ret)
{
	addr control, restart1, restart2;

	function_use_restart(&restart1);
	function_store_restart(&restart2, name);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	name_callname_heap(name, &name);
	(void)call_undefined_function_(ptr, name);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use-value */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		goto escape;
	}

	/* store-value */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);
	return 0;
}

int callname_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! callnamep(name), "type error");
	getglobal_callname(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return function_restart_call(ptr, name, ret);
}

int function_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! symbolp(name), "type error");
	GetFunctionSymbol(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	callname_heap(&name, name, CALLNAME_SYMBOL);
	return function_restart_call(ptr, name, ret);
}

int setf_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! symbolp(name), "type error");
	getsetf_symbol(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	callname_heap(&name, name, CALLNAME_SETF);
	return function_restart_call(ptr, name, ret);
}


/*
 *  fdefinition
 */
static int restart_fdefinition_use_function(Execute ptr, addr value)
{
	if (! functionp(value))
		return TypeError_(value, FUNCTION);
	setresult_control(ptr, value);

	return 0;
}

static int restart_fdefinition_use_interactive_p_(addr pos, addr *ret)
{
	addr value;

	if (functionp(pos))
		return Result(ret, pos);

	/* parse callname */
	if (parse_callname_heap(&pos, pos))
		return Result(ret, Nil); /* parse error */

	/* function */
	getglobal_callname(pos, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* setf */
	if (setfp_callname(pos))
		return Result(ret, Nil); /* error */

	/* macro */
	GetCallName(pos, &value);
	getmacro_symbol(value, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* undefined-function */
	name_callname_heap(pos, &pos);
	return call_undefined_function_(NULL, pos);
}

static int restart_fdefinition_use_interactive_char_(Execute ptr, const char *str)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Use new function: ");
	for (;;) {
		Return(prompt_for_stream(ptr, T, prompt, &eval));
		Return(eval_result_partial_form_(ptr, eval, &eval));
		Return(restart_fdefinition_use_interactive_p_(eval, &eval));
		if (eval != Nil)
			break;
		/* error */
		strvect_char_heap(&prompt, "Please answer FUNCTION-NAME type: ");
	}

	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_fdefinition_use_interactive(Execute ptr)
{
	return restart_fdefinition_use_interactive_char_(ptr, "Use new function: ");
}

static void fdefinition_use_restart(addr *ret)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_fdefinition_use_function);
	setfunction_restart(pos, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_fdefinition_use_interactive);
	setinteractive_restart(pos, value);
	/* report */
	strvect_char_heap(&value, "Use specific function.");
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

static int restart_fdefinition_store_function(Execute ptr, addr value)
{
	addr name;

	if (! functionp(value))
		return TypeError_(value, FUNCTION);

	getdata_control(ptr, &name);
	if (setfp_callname(name) || funcall_function_p(value)) {
		/* normal function */
		Return(setglobal_callname_(name, value));
	}
	else {
		/* macro-function */
		GetCallName(name, &name);
		Return(alldelete_function_(name));
		Return(setmacro_symbol_(name, value));
	}
	setresult_control(ptr, value);

	return 0;
}

static int restart_fdefinition_store_interactive(Execute ptr)
{
	return restart_fdefinition_use_interactive_char_(ptr, "Store new function: ");
}

static void fdefinition_store_restart(addr *ret, addr name)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, name);
	setcompiled_var1(value, p_restart_fdefinition_store_function);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_fdefinition_store_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Store specific function.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int fdefinition_restart_call_(Execute ptr, addr name, addr *ret)
{
	addr control, restart1, restart2;

	fdefinition_use_restart(&restart1);
	fdefinition_store_restart(&restart2, name);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	name_callname_heap(name, &name);
	(void)call_undefined_function_(ptr, name);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use-value */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		goto escape;
	}

	/* store-value */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);
	return 0;
}

int fdefinition_restart_(Execute ptr, addr name, addr *ret)
{
	CallNameType type;
	addr symbol, value;

	Check(! callnamep(name), "type error");
	GetCallNameType(name, &type);
	GetCallName(name, &symbol);

	/* setf */
	if (type == CALLNAME_SETF)
		return setf_global_restart(ptr, symbol, ret);

	/* function */
	GetFunctionSymbol(symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* macro */
	getmacro_symbol(symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* error, restart */
	return fdefinition_restart_call_(ptr, name, ret);
}


/*
 *  abort
 */
void abort_restart_char_heap(addr *ret, const char *str)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_ABORT, &pos);
	restart_heap(&pos, pos);
	/* function */
	GetConst(FUNCTION_NIL, &value);
	setfunction_restart(pos, value);
	/* interactive */
	setinteractive_restart(pos, Nil);
	/* report */
	strvect_char_heap(&value, str);
	setreport_restart(pos, value);
	/* test */
	constantly_common(T, &value);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

void abort_restart_char_control(Execute ptr, const char *str)
{
	addr restart;
	abort_restart_char_heap(&restart, str);
	pushrestart_control(ptr, restart);
}


/*
 *  initialize
 */
void init_restart_value(void)
{
	SetPointerType(var1, restart_symbol_use_function);
	SetPointerType(empty, restart_symbol_use_interactive);
	SetPointerType(var1, restart_symbol_use_test);
	SetPointerType(var1, restart_symbol_store_special);
	SetPointerType(empty, restart_symbol_store_interactive);
	SetPointerType(var1, restart_function_use_function);
	SetPointerType(empty, restart_function_use_interactive);
	SetPointerType(var1, restart_function_store_function);
	SetPointerType(empty, restart_function_store_interactive);
	SetPointerType(var1, restart_fdefinition_use_function);
	SetPointerType(empty, restart_fdefinition_use_interactive);
	SetPointerType(var1, restart_fdefinition_store_function);
	SetPointerType(empty, restart_fdefinition_store_interactive);
}


/************************************************************
 *  rt.c
 ************************************************************/

/* (defpackage rt ...)
 *   (import lisp-system::infobit 'rt)
 *   (export lisp-system::infobit 'rt)
 *   (import lisp-system::infoprint 'rt)
 *   (export lisp-system::infoprint 'rt)
 */
static void import_export_symbol_rt(constindex index)
{
	addr symbol, package;

	GetConstant(index, &symbol);
	GetConst(PACKAGE_RT, &package);
	Error(import_package_(package, symbol));
	Error(export_package_(package, symbol));
}

static void defpackage_rt(void)
{
	import_export_symbol_rt(CONSTANT_SYSTEM_INFOBIT);
	import_export_symbol_rt(CONSTANT_SYSTEM_INFOPRINT);
}


/* (defvar lisp-rt::*entries* [queue])
 *   *entries*  (root . tail)
 */
static void defvar_entries(void)
{
	addr symbol, pos;

	GetConst(RT_ENTRIES, &symbol);
	setspecial_symbol(symbol);
	queue_heap(&pos);
	SetValueSymbol(symbol, pos);
}


/* (defvar lisp-rt::*entries-table* (make-hash-table :test #'eq)) */
static void defvar_entries_table(void)
{
	addr symbol, pos;

	GetConst(RT_ENTRIES_TABLE, &symbol);
	setspecial_symbol(symbol);
	hashtable_heap(&pos);
	SetValueSymbol(symbol, pos);
}


/* (defvar lisp-rt::*entries-warning* [queue])
 *   *entries*  (root . tail)
 */
static void defvar_entries_warning(void)
{
	addr symbol, pos;

	GetConst(RT_ENTRIES_WARNING, &symbol);
	setspecial_symbol(symbol);
	queue_heap(&pos);
	SetValueSymbol(symbol, pos);
}


/* (defun push-entries (name expr values) ...) -> nil
 *   name    symbol
 *   expr    t
 *   values  list
 */
static int rt_push_entries_(Execute ptr, constindex index, addr name)
{
	addr queue;

	GetConstant(index, &queue);
	Return(getspecialcheck_local_(ptr, queue, &queue));
	Check(! consp(queue), "*entries* error");
	pushqueue_heap(queue, name);

	return 0;
}

static int function_push_entries(Execute ptr, addr name, addr expr, addr values)
{
	addr table, pos, list;

	/* check *entries-table* */
	GetConst(RT_ENTRIES_TABLE, &table);
	Return(getspecialcheck_local_(ptr, table, &table));
	Return(find_hashtable_(table, name, &pos));
	if (pos != Unbound) {
		Return(fmtw_("The deftest ~S is already exist.", name, NULL));
		Return(rt_push_entries_(ptr, CONSTANT_RT_ENTRIES_WARNING, name));
	}
	else  {
		/* push *entries* */
		Return(rt_push_entries_(ptr, CONSTANT_RT_ENTRIES, name));
	}

	/* intern *entries-table* */
	cons_heap(&list, expr, values);
	Return(intern_hashheap_(table, name, &pos));
	SetCdr(pos, list);

	/* result */
	setresult_control(ptr, Nil);
	return 0;
}

static void type_push_entries(addr *ret)
{
	addr arg, values, name, expr;

	GetTypeTable(&name, Symbol);
	GetTypeTable(&expr, T);
	GetTypeTable(&values, List);
	typeargs_var3(&arg, name, expr, values);
	GetTypeTable(&values, Null);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_push_entries(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_PUSH_ENTRIES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_push_entries);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_push_entries(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rem-all-tests () -> nil
 *   (export 'rem-all-tests 'lisp-rt)
 */
static void export_symbol_rt(addr symbol)
{
	addr package;
	GetPackageSymbol(symbol, &package);
	Error(export_package_(package, symbol));
}

static int rm_all_tests_clear_(Execute ptr, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	clearqueue(pos);

	return 0;
}

static int rem_all_tests_(Execute ptr)
{
	addr symbol, pos;

	/* (setq *entries* (list nil)) */
	Return(rm_all_tests_clear_(ptr, CONSTANT_RT_ENTRIES));
	/* (setq *entries-warning* (list nil)) */
	Return(rm_all_tests_clear_(ptr, CONSTANT_RT_ENTRIES_WARNING));
	/* (clrhash *entries-table*) */
	GetConst(RT_ENTRIES_TABLE, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	clear_hashtable(pos);

	return 0;
}

static int function_rem_all_tests(Execute ptr)
{
	Return(rem_all_tests_(ptr));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_rem_all_tests(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeTable(&values, Null);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_rem_all_tests(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_REM_ALL_TESTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_rem_all_tests);
	SetFunctionSymbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	type_rem_all_tests(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro deftest (name expr &rest values) ...)
 *   name    symbol
 *   expr    t
 *   values  &rest t
 *   (export 'deftest 'lisp-rt)
 */
static int function_deftest(Execute ptr, addr form, addr env)
{
	addr args, name, expr, quote, push;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! symbolp(name))
		return fmte_("The deftest name ~S must be a symbol.", name, NULL);
	if (! consp(args))
		goto error;
	GetCons(args, &expr, &args);
	/* `(push-entries ',name ',expr ',value) */
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(&expr, quote, expr, NULL);
	list_heap(&args, quote, args, NULL);
	GetConst(RT_PUSH_ENTRIES, &push);
	list_heap(&push, push, name, expr, args, NULL);
	setresult_control(ptr, push);
	return 0;

error:
	return fmte_("The deftest ~S "
			"must be a (deftest name expr . values) form.", form, NULL);
}

static void defmacro_deftest(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest);
	setmacro_symbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro deftest-error (name expr &optional (error error))
 *   `(deftest ,name
 *      (handler-case
 *        ,expr
 *        (,error () 'lisp-rt::error))
 *      ,lisp-rt::error))
 *   name    symbol
 *   expr    t
 *   error   symbol
 *   (export 'deftest-error 'lisp-rt)
 */
static int function_deftest_error(Execute ptr, addr form, addr env)
{
	addr args, name, expr, error, symbol, rterror, deftest, handler_case, quote;

	/* args */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &expr, &args);
	if (args == Nil) {
		GetConst(COMMON_ERROR, &error);
		goto make_deftest;
	}
	if (! consp(args))
		goto error;
	GetCons(args, &error, &args);
	if (args != Nil)
		goto error;
	goto make_deftest;

	/* make body */
make_deftest:
	GetConst(RT_ERROR, &rterror);
	GetConst(RT_DEFTEST, &deftest);
	GetConst(COMMON_HANDLER_CASE, &handler_case);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&symbol, quote, rterror, NULL);
	list_heap(&error, error, Nil, symbol, NULL);
	list_heap(&handler_case, handler_case, expr, error, NULL);
	list_heap(&form, deftest, name, handler_case, rterror, NULL);
	setresult_control(ptr, form);
	return 0;

error:
	return fmte_("Invalid deftest-error form ~S.", form, NULL);
}

static void defmacro_deftest_error(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST_ERROR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest_error);
	setmacro_symbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro deftest-error! (name expr &optional (error error))
 *   `(deftest-error ,name
 *      (handler-bind ((warning (function muffle-warning)))
 *        ,expr)
 *      ,error))
 */
static int function_deftest_error_(Execute ptr, addr form, addr env)
{
	addr args, name, expr, error;
	addr rterror, handler, warning, quote, muffle;

	/* args */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &expr, &args);
	if (args == Nil) {
		GetConst(COMMON_ERROR, &error);
		goto make_deftest;
	}
	if (! consp(args))
		goto error;
	GetCons(args, &error, &args);
	if (args != Nil)
		goto error;
	goto make_deftest;

	/* make body */
make_deftest:
	GetConst(RT_DEFTEST_ERROR, &rterror);
	GetConst(COMMON_HANDLER_BIND, &handler);
	GetConst(COMMON_WARNING, &warning);
	GetConst(COMMON_FUNCTION, &quote);
	GetConst(COMMON_MUFFLE_WARNING, &muffle);
	list_heap(&quote, quote, muffle, NULL);
	list_heap(&warning, warning, quote, NULL);
	list_heap(&warning, warning, NULL);
	list_heap(&handler, handler, warning, expr, NULL);
	list_heap(&form, rterror, name, handler, error, NULL);
	setresult_control(ptr, form);
	return 0;

error:
	return fmte_("Invalid deftest-error! form ~S.", form, NULL);
}

static void defmacro_deftest_error_(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST_ERROR_, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest_error_);
	setmacro_symbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun do-tests (&optional delete) ...) -> boolean
 *   (export 'do-tests 'lisp-rt)
 */
static int do_test_equal_(Execute ptr, addr expr, addr values, addr *rvalues, int *ret)
{
	int check1, check2;
	addr result, pos1, pos2;

	/* (eval expr) */
	Return(eval_execute_partial_(ptr, expr));
	getvalues_list_control_local(ptr, &result);
	*rvalues = result;

	/* values check */
	for (;;) {
		check1 = (values == Nil);
		check2 = (result == Nil);
		if (check1 && check2)
			break;
		if (check1 || check2)
			return Result(ret, 0);
		GetCons(values, &pos1, &values);
		GetCons(result, &pos2, &result);
		Return(equalrt_function_(pos1, pos2, &check1));
		if (! check1)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int do_test_output_loop_(Execute ptr, addr io, const char *str, addr list)
{
	/* format_stream(ptr, io, "  *** Expect:~{ ~S~}~%", values, NULL); */
	addr pos;

	Return(print_ascii_stream_(io, str));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(write_char_stream_(io, ' '));
		Return(prin1_print(ptr, io, pos));
	}

	return terpri_stream_(io);
}

static int do_test_getindex_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(RT_INDEX, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int do_test_output_(Execute ptr, addr name, addr values, addr result, int check)
{
	addr pos, io;
	LocalRoot local;
	LocalStack stack;

	Return(debug_io_stream_(ptr, &io));
	Return(do_test_getindex_(ptr, &pos));

	local = ptr->local;
	push_local(local, &stack);
	if (check) {
		Return(format_stream(ptr, io, "~&[RT] ~6@A: ~A~%", pos, name, NULL));
	}
	else {
		Return(format_stream(ptr, io, "~&[ERROR] ~6@A: ~A~%", pos, name, NULL));
		Return(do_test_output_loop_(ptr, io, "  *** Expect:", values));
		Return(do_test_output_loop_(ptr, io, "  *** Actial:", result));
	}
	rollback_local(local, stack);

	return 0;
}

static int do_test_output_unhandling_(Execute ptr, addr name, addr values)
{
	addr pos, io;
	LocalRoot local;
	LocalStack stack;

	Return(debug_io_stream_(ptr, &io));
	Return(do_test_getindex_(ptr, &pos));

	local = ptr->local;
	push_local(local, &stack);
	Return(format_stream(ptr, io, "~&[ERROR] ~6@A: ~A~%", pos, name, NULL));
	Return(do_test_output_loop_(ptr, io, "  *** Expect:", values));
	Return(format_stream(ptr, io,
				"  *** Actual: #<System error, unhandling signal>~%", NULL));
	rollback_local(local, stack);

	return 0;
}

static int do_test_execute_(Execute ptr, addr name, addr expr, addr values, int *ret)
{
	int check;
	addr result;

	Return(do_test_equal_(ptr, expr, values, &result, &check));
	Return(do_test_output_(ptr, name, values, result, check));

	return Result(ret, check);
}

static int do_test_call_(Execute ptr, addr name, addr expr, addr values, int *ret)
{
	int check, finish;
	lisp_abort_calltype handler;

	check = 0;
	finish = 0;

	handler = set_degrade_setjmp_handler();
	Lisp_degrade_Begin {
		if (do_test_execute_(ptr, name, expr, values, &check))
			abort_execute();
		finish = 1;
	}
	Lisp_degrade_End;
	(void)set_abort_handler(handler);

	if (finish == 0) {
		Return(do_test_output_unhandling_(ptr, name, values));
		check = 0; /* error */
	}

	return Result(ret, check);
}

static int do_test_(Execute ptr, addr name, addr table, int *ret)
{
	int check;
	addr control, expr, values;

	/* table */
	Return(find_hashtable_(table, name, &expr));
	if (expr == Unbound) {
		*ret = 0;
		return fmte_("The deftest ~S is not exist.", name, NULL);
	}
	GetCons(expr, &expr, &values);

	/* test */
	push_control(ptr, &control);
	check = 0;
	(void)do_test_call_(ptr, name, expr, values, &check);
	if (free_control_degrade_(ptr, control)) {
		Return(do_test_output_unhandling_(ptr, name, values));
		check = 0; /* error */
	}

	return Result(ret, check);
}

static void function_do_tests_initindex(Execute ptr)
{
	addr symbol, value;

	GetConst(RT_INDEX, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value == Unbound) {
		fixnum_heap(&value, 0);
		setspecial_local(ptr, symbol, value);
	}
}

static int function_do_tests_variables_(Execute ptr, addr *rlist, addr *rtable)
{
	addr list, table;

	GetConst(RT_ENTRIES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	rootqueue(list, &list);
	GetConst(RT_ENTRIES_TABLE, &table);
	Return(getspecialcheck_local_(ptr, table, &table));

	*rlist = list;
	*rtable = table;

	return 0;
}

static int function_do_tests_output2_(Execute ptr, fixnum count2)
{
	addr io, root2;

	if (count2 == 0)
		return 0;
	Return(debug_io_stream_(ptr, &io));
	make_index_integer_heap(&root2, count2);
	gchold_push_local(ptr->local, root2);
	Return(format_stream(ptr, io, "~%", NULL));
	Return(format_stream(ptr, io, "*************~%", NULL));
	Return(format_stream(ptr, io, "*** ERROR ***~%", NULL));
	Return(format_stream(ptr, io, "*************~2%", NULL));
	Return(format_stream(ptr, io, "ERROR = ~A~%", root2, NULL));

	return 0;
}

static int function_do_tests_duplicated_(Execute ptr)
{
	addr io, pos;

	/* *entries-warning* */
	GetConst(RT_ENTRIES_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	rootqueue(pos, &pos);
	if (pos != Nil) {
		Return(debug_io_stream_(ptr, &io));
		Return(format_stream(ptr, io,
					"~&[DUPLICATED] These testcases is ignored.~%", NULL));
		Return(format_stream(ptr, io,
					"  *** Testcase: ~A~2%", pos, NULL));
	}

	return 0;
}

static void do_tests_increment(Execute ptr)
{
	addr symbol, pos;
	fixnum value;

	GetConst(RT_INDEX, &symbol);
	getspecial_local(ptr, symbol, &pos);
	if (pos == Unbound || ! fixnump(pos)) {
		fixnum_heap(&pos, 1);
	}
	else {
		GetFixnum(pos, &value);
		fixnum_heap(&pos, value + 1);
	}
	setspecial_local(ptr, symbol, pos);
}

static int function_do_tests_execute_(Execute ptr)
{
	int check;
	addr list, table, name, root1, root2;
	fixnum count1, count2;
	LocalRoot local;
	LocalStack stack;

	/* initialize */
	Return(function_do_tests_variables_(ptr, &list, &table));
	root1 = root2 = Nil;
	count1 = count2 = 0;
	local = ptr->local;
	push_local(local, &stack);

	/* loop */
	while (list != Nil) {
		do_tests_increment(ptr);
		GetCons(list, &name, &list);
		check = 0;
		Return(do_test_(ptr, name, table, &check));
		if (check) {
			/* ok */
			cons_local(local, &root1, name, root1);
			count1++;
		}
		else {
			/* error */
			cons_local(local, &root2, name, root2);
			count2++;
		}
	}

	/* output */
	Return(function_do_tests_output2_(ptr, count2));
	Return(function_do_tests_duplicated_(ptr));

	/* result */
	rollback_local(local, stack);
	setbool_control(ptr, count2 == 0);
	GetConst(RT_RESULT, &name);
	setspecial_local(ptr, name, (count2 == 0)? T: Nil);

	return 0;
}

static int function_do_tests(Execute ptr, addr rest)
{
	/* argument */
	if (GetKeyArgs(rest, KEYWORD_TEST, &rest))
		rest = Nil;

	/* do-tests */
	function_do_tests_initindex(ptr);
	Return(function_do_tests_execute_(ptr));

	/* rem-all-tests */
	if (rest != Nil) {
		Return(rem_all_tests_(ptr));
	}

	return 0;
}

static void type_do_tests(addr *ret)
{
	addr arg, values;

	KeyTypeTable(&arg, TEST, T);
	list_heap(&arg, arg, NULL);
	typeargs_key(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_do_tests(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_DO_TESTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_do_tests);
	SetFunctionSymbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	type_do_tests(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equalrt (a b) ...) -> boolean */
static int function_equalrt(Execute ptr, addr a, addr b)
{
	int check;

	Return(equalrt_function_(a, b, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_equalrt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_EQUALRT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_equalrt);
	SetFunctionSymbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  build
 */
void init_rt(void)
{
	SetPointerCall(defun, var3, push_entries);
	SetPointerCall(defun, empty, rem_all_tests);
	SetPointerCall(defmacro, macro, deftest);
	SetPointerCall(defmacro, macro, deftest_error);
	SetPointerCall(defmacro, macro, deftest_error_);
	SetPointerCall(defun, dynamic, do_tests);
	SetPointerCall(defun, var2, equalrt);
}

void build_rt(void)
{
	defpackage_rt();
	defvar_entries();
	defvar_entries_table();
	defvar_entries_warning();
	defun_push_entries();
	defun_rem_all_tests();
	defmacro_deftest();
	defmacro_deftest_error();
	defmacro_deftest_error_();
	defun_do_tests();
	defun_equalrt();
}


/************************************************************
 *  rt_load.c
 ************************************************************/

#ifdef LISP_DEGRADE

/*
 *  Main
 */
static int rtload_execute_(Execute ptr, addr stream)
{
	LocalHold hold;

	hold = LocalHold_local_push(ptr, stream);
	(void)eval_stream_toplevel_(ptr, stream);
	Return(close_stream_unwind_protect_(ptr, stream));
	localhold_end(hold);

	return 0;
}

static int rtload_pathname_(Execute ptr, addr file, int *ret)
{
	addr path, stream, pos;

	/* load name */
	Return(open_input_stream_(ptr, &stream, file));
	if (stream == NULL) {
		/* load "test/" name */
		Return(parse_pathname_char_heap_(ptr, "test/", &path));
		Return(merge_pathnames_clang_(ptr, file, path, Unbound, &file));
		Return(open_input_stream_error_(ptr, &stream, file)); /* force */
	}

	Return(rtload_execute_(ptr, stream));
	getresult_control(ptr, &pos);
	return Result(ret, pos != T);
}

static int loadrt_init_(Execute ptr, const char *name, int *ret)
{
	addr package, symbol, use, file;

	/* title */
	strvect_char_heap(&file, name);
	Return(format_stdout(ptr, "~&[~A]~%", file, NULL));

	/* (let ((*package* (find-package 'common-lisp-user))) ...) */
	Return(find_char_package_(LISP_COMMON_USER, &package));
	GetConst(SPECIAL_PACKAGE, &symbol);
	pushspecial_control(ptr, symbol, package);
	/* (use-package 'lisp-rt *package*) */
	Return(find_char_package_(LISP_RT, &use));
	Return(use_package_(package, use));
	/* load-rt */
	Return(parse_pathname_char_heap_(ptr, name, &file));
	return rtload_pathname_(ptr, file, ret);
}

static void loadrt_disable_debugger(Execute ptr)
{
	addr symbol;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

static void loadrt_declare_optimize(void)
{
	apply_safety_declaim(3);
	apply_speed_declaim(0);
}

static int loadrt_execute_call_(Execute ptr, const char *name)
{
	int check;

	Return(handler_warning_(ptr));
	loadrt_disable_debugger(ptr);
	loadrt_declare_optimize();
	Return(loadrt_init_(ptr, name, &check));
	if (check)
		return fmte_("result code error.", NULL);

	return 0;
}

static int loadrt_execute(Execute ptr, const char *name)
{
	addr control;

	push_control(ptr, &control);
	(void)loadrt_execute_call_(ptr, name);
	return pop_control_(ptr, control);
}

static int loadrt_nickname_(const char *str1, const char *str2)
{
	addr name1, name2;

	strvect_char_heap(&name1, str1);
	strvect_char_heap(&name2, str2);
	conscar_heap(&name2, name2);
	return rename_package_(name1, name1, name2, &name1);
}

static int loadrt_nicknames_(void)
{
	addr symbol, keyword, cons;

	Return(loadrt_nickname_(LISP_SYSTEM, "LISP-SYSTEM"));
	Return(loadrt_nickname_(LISP_CLOS, "LISP-CLOS"));
	Return(loadrt_nickname_(LISP_RT, "LISP-RT"));

	/* push :rt-degrade */
	GetConst(SPECIAL_FEATURES, &symbol);
	Return(internchar_keyword_("RT-DEGRADE", &keyword, NULL));
	GetValueSymbol(symbol, &cons);
	Check(find_list_eq_unsafe(keyword, cons), "push error");
	cons_heap(&cons, keyword, cons);
	SetValueSymbol(symbol, cons);

	return 0;
}

static void loadrt_getindex(Execute ptr)
{
	addr symbol, value;

	GetConst(RT_INDEX, &symbol);
	fixnum_heap(&value, (fixnum)DegradeCount);
	setspecial_local(ptr, symbol, value);
}

static int loadrt_setindex_(Execute ptr)
{
	addr symbol, value;
	fixnum index;

	GetConst(RT_INDEX, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value != Unbound) {
		if (! fixnump(value))
			return fmte_("Invalid fixnum value ~S.", value, NULL);
		GetFixnum(value, &index);
		DegradeCount = (int)index;
	}

	return 0;
}

static int loadrt_body_lisp_(Execute ptr, const char *name)
{
	Return(loadrt_nicknames_());
	loadrt_getindex(ptr);
	Return(loadrt_execute(ptr, name));
	return loadrt_setindex_(ptr);
}

static int loadrt_lisp(const char *name)
{
	int errorp;
	Execute ptr;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;

	buildlisp(ptr);
	errorp = loadrt_body_lisp_(ptr, name);
	freelisp();

	return errorp;
}

#include "load.h"
int test_loadrt(void)
{
	DegradeTitle;
#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = LISP_DEBUG_FORCE_GC;
#endif
	return loadrt_files();
}
#else
int test_loadrt(void)
{
	return 1;
}
#endif


/************************************************************
 *  scope.c
 ************************************************************/

static int eval_scope_call_(Execute ptr, addr *ret, addr eval)
{
	Return(begin_eval_stack_(ptr));
	return scope_eval_lexical(ptr, ret, eval);
}

int eval_scope_(Execute ptr, addr *ret, addr eval)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_scope_call_(ptr, ret, eval);
	free_eval_stack(ptr);
	return pop_control_(ptr, control);
}

void init_scope(void)
{
	init_scope_function();
}


/************************************************************
 *  scope_call.c
 ************************************************************/

/*
 *  symbol
 */
static int warning_global_lexical_(addr symbol)
{
	/*  return call_simple_style_warning_va_(NULL,
	 *      "Undefined variable ~S.", symbol, NULL);
	 */
	return 0;
}

static int symbol_global_tablevalue_(Execute ptr, addr symbol, addr *value, int *ret)
{
	int specialp;
	addr stack;

	Return(getglobal_eval_(ptr, &stack));
	if (! find_tablevalue(stack, symbol, value)) {
		/* heap object */
		Return(push_tablevalue_global_(ptr, stack, symbol, value));
		specialp = getspecialp_tablevalue(*value);
		if (! specialp) {
			Return(warning_global_lexical_(symbol));
		}
		return Result(ret, specialp);
	}
	return Result(ret, getspecialp_tablevalue(*value));
}

static void push_closure_value(addr stack, addr symbol, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tablevalue(value);
	/* destination table */
	copy_tablevalue(&pos, value);
	setclosurep_tablevalue(pos, 1);
	setclosure_tablevalue(pos, lexical);
	setlexical_tablevalue(pos, increment_stack_eval(stack));
	setvalue_lexical_evalstack(stack, pos);
	setvalue_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int symbol_global_special_tablevalue_(Execute ptr, addr symbol, addr *value)
{
	addr stack, pos;

	Return(getglobal_eval_(ptr, &stack));
	if (find_tablevalue(stack, symbol, &pos)) {
		if (getspecialp_tablevalue(pos))
			return Result(value, pos);
	}

	/* heap object */
	Return(push_tablevalue_special_global_(ptr, stack, symbol, value));
	Check(! getspecialp_tablevalue(*value), "special error");

	return 0;
}

static int symbol_special_tablevalue_(Execute ptr,
		addr stack, addr symbol, addr *value)
{
	addr next;

	/* global */
	if (stack == Nil) {
		return symbol_global_special_tablevalue_(ptr, symbol, value);
	}

	/* local */
	if (getvalue_scope_evalstack(stack, symbol, value)) {
		if (getspecialp_tablevalue(*value))
			return 0;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	return symbol_special_tablevalue_(ptr, next, symbol, value);
}

static int symbol_tablevalue_(Execute ptr,
		addr stack, addr symbol, int basep, addr *value, int *ret)
{
	int check;
	addr next;

	/* global */
	if (stack == Nil) {
		return symbol_global_tablevalue_(ptr, symbol, value, ret);
	}

	/* local */
	if (getvalue_scope_evalstack(stack, symbol, value)) {
		if (basep)
			setbasep_tablevalue(*value, 1);
		setreference_tablevalue(*value, 1);
		return Result(ret, getspecialp_tablevalue(*value));
	}

	/* declare special */
	if (find_special_evalstack(stack, symbol)) {
		symbol_special_tablevalue_(ptr, stack, symbol, value);
		return Result(ret, 1);
	}

	/* basep */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		basep = 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	Return(symbol_tablevalue_(ptr, next, symbol, basep, value, &check));
	if (check) {
		return Result(ret, 1); /* special */
	}

	/* global */
	if (getglobalp_tablevalue(*value)) {
		return Result(ret, 0); /* lexical */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_value(stack, symbol, *value, value);
	}

	return Result(ret, 0); /* lexical */
}

static int find_symbol_scope_(Execute ptr, addr symbol, addr *value, int *ret)
{
	int specialp;
	addr stack, pos;

	Return(getstack_eval_(ptr, &stack));
	Return(symbol_tablevalue_(ptr, stack, symbol, 0, &pos, &specialp));
	copy_tablevalue(value, pos);

	return Result(ret, specialp);
}

static int scope_symbol_heap_(Execute ptr, addr *ret, addr type, addr symbol)
{
	return eval_scope_size_(ptr, ret, 1, EVAL_PARSE_SYMBOL, type, symbol);
}

static int make_scope_symbol_(Execute ptr, addr symbol, addr *ret)
{
	int ignore;
	addr value, type, pos;

	Check(! symbolp(symbol), "type error");
	Return(find_symbol_scope_(ptr, symbol, &value, &ignore));
	gettype_tablevalue(value, &type);
	Return(scope_symbol_heap_(ptr, &pos, type, symbol));
	SetEvalScopeIndex(pos, 0, value);

	return Result(ret, pos);
}

static int make_scope_keyword_(Execute ptr, addr symbol, addr *ret)
{
	addr type;
	GetTypeTable(&type, Keyword);
	return scope_symbol_heap_(ptr, ret, type, symbol);
}

int scope_symbol_call(Execute ptr, addr *ret, addr eval)
{
	if (keywordp(eval))
		return make_scope_keyword_(ptr, eval, ret);
	else
		return make_scope_symbol_(ptr, eval, ret);
}


/*
 *  setq
 */
int scope_setq_call(Execute ptr, addr cons, addr *ret, addr *type)
{
	int ignore;
	addr root, var, form;
	LocalHold hold;

	if (cons == Nil) {
		GetTypeTable(type, Null);
		*ret = Nil;
		return 0;
	}

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(var, &var, &form);
		Return(find_symbol_scope_(ptr, var, &var, &ignore));
		Return(scope_eval(ptr, &form, form));
		GetEvalScopeThe(form, type);
		Return(checktype_value_(ptr, var, form));

		cons_heap(&var, var, form);
		cons_heap(&root, var, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}


/*
 *  values
 */
int scope_values_call(Execute ptr, addr args, addr *rargs, addr *rtype)
{
	addr root, var, rest, eval, type;
	LocalHold hold;

	/* progn and typelist */
	hold = LocalHold_array(ptr, 2);
	for (root = var = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(scope_eval(ptr, &eval, eval));
		GetEvalScopeThe(eval, &type);
		/* push */
		cons_heap(&root, eval, root);
		cons_heap(&var, type, var);
		localhold_set(hold, 0, root);
		localhold_set(hold, 1, var);
	}
	localhold_end(hold);
	nreverse(rargs, root);
	nreverse(&var, var);

	/* (values ... &rest null) */
	GetTypeTable(&rest, Null);
	type_values_heap(var, Nil, rest, Nil, rtype);

	return 0;
}


/*
 *  the
 */
static int scope_the_check_warning_(Execute ptr, addr type, addr expected)
{
	Return(type_object_(&type, type));
	Return(type_object_(&expected, expected));
	return call_type_error_va_(ptr, Nil, expected,
			"The special operator THE accept a ~S type, "
			"but actually the form is ~S type.",
			expected, type, NULL);
}

static int scope_the_check_(Execute ptr, addr eval, addr right, addr *ret)
{
	int check, errp;
	addr left;

	GetEvalScopeThe(eval, &left);
	Return(checktype_p_(ptr, left, right, &check, &errp));
	if (errp) {
		Return(scope_the_check_warning_(ptr, left, right));
	}

	return Result(ret, check? T: Nil);
}

int scope_the_call(Execute ptr, addr type, addr form, addr *ret)
{
	addr eval, check;

	Return(scope_eval(ptr, &form, form));
	Return(scope_the_check_(ptr, form, type, &check));
	/* result */
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_THE, type, form));
	SetEvalScopeIndex(eval, 0, check);
	return Result(ret, eval);
}


/*
 *  locally
 */
static int locally_execute(Execute ptr,
		addr decl, addr cons, addr *ret, addr *type, addr *free)
{
	addr stack;

	Return(newstack_nil_(ptr, &stack));
	Return(apply_declare_(ptr, stack, decl, free));
	Return(scope_allcons(ptr, ret, type, cons));

	return freestack_eval_(ptr, stack);
}

int scope_locally_call(Execute ptr, addr decl, addr cons, addr *ret)
{
	addr eval, type, free;

	Return(locally_execute(ptr, decl, cons, &cons, &type, &free));
	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_LOCALLY, type, Nil));
	SetEvalScopeIndex(eval, 0, decl);
	SetEvalScopeIndex(eval, 1, cons);
	SetEvalScopeIndex(eval, 2, free);
	return Result(ret, eval);
}


/*
 *  tagbody
 */
static void push_tabletagbody_lexical(addr stack, addr pos)
{
	addr name;

	Check(stack == Nil, "stack error");
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tabletagbody(pos, &name);
		setlexical_tabletagbody(pos, increment_stack_eval(stack));
		settagbody_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tabletagbody_lexical(stack, pos);
	}
}

static void push_tabletagbody(addr stack, addr tag, addr *ret)
{
	addr pos;

	if (gettagbody_scope_evalstack(stack, tag, ret))
		return;
	make_tabletagbody(&pos, tag);
	settagbody_scope_evalstack(stack, pos);
	push_tabletagbody_lexical(stack, pos);
	*ret = pos;
}

static void tagbody_call_push(addr stack, addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Check(RefEvalParseType(pos) != EVAL_PARSE_TAG, "type error");
		GetEvalParse(pos, 0, &pos);
		push_tabletagbody(stack, pos, &pos);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

static int tagbody_call_find_(addr list, addr pos, addr *ret)
{
	addr value, check;

	GetEvalParse(pos, 0, &pos);
	while (list != Nil) {
		GetCons(list, &value, &list);
		getname_tabletagbody(value, &check);
		if (eql_function(pos, check))
			return Result(ret, value);
	}

	/* error */
	*ret = 0;
	return fmte_("Invalid tag name.", NULL);
}

static int tagbody_allcons(Execute ptr, addr tag, addr body, addr *ret)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; body != Nil; ) {
		GetCons(body, &pos, &body);
		if (RefEvalParseType(pos) == EVAL_PARSE_TAG) {
			Return(tagbody_call_find_(tag, pos, &pos));
			Return(make_eval_scope_(ptr, &pos, EVAL_PARSE_TAG, Nil, pos));
		}
		else {
			Return(scope_eval(ptr, &pos, pos));
		}
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static void tagbody_call_remove(addr stack, addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (getreference_tabletagbody(pos))
			cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

int scope_tagbody_call(Execute ptr, addr tag, addr body, addr *rtag, addr *rbody)
{
	addr stack;

	Return(newstack_nil_(ptr, &stack));
	tagbody_call_push(stack, tag, &tag);
	Return(tagbody_allcons(ptr, tag, body, rbody));
	tagbody_call_remove(stack, tag, rtag);

	return freestack_eval_(ptr, stack);
}


/*
 *  go
 */
static void push_closure_tagbody(addr stack, addr tag, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tabletagbody(value);
	/* destination table */
	copy_tabletagbody(&pos, value);
	setclosurep_tabletagbody(pos, 1);
	setclosure_tabletagbody(pos, lexical);
	setlexical_tabletagbody(pos, increment_stack_eval(stack));
	settagbody_lexical_evalstack(stack, pos);
	settagbody_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int go_tabletagbody(addr stack, addr tag, addr *ret)
{
	addr next;

	/* not found */
	if (stack == Nil) {
		return 0;
	}

	/* local */
	if (gettagbody_scope_evalstack(stack, tag, ret)) {
		setreference_tabletagbody(*ret, 1);
		return 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (! go_tabletagbody(next, tag, ret)) {
		return 0;
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_tagbody(stack, tag, *ret, ret);
	}

	return 1;
}

int scope_go_call_(Execute ptr, addr *ret, addr tag)
{
	addr stack, table;

	Return(getstack_eval_(ptr, &stack));
	if (! go_tabletagbody(stack, tag, &table))
		return fmte_("Tag ~S is not found.", tag, NULL);

	return Result(ret, table);
}


/*
 *  block
 */
static void push_tableblock_lexical(addr stack, addr pos)
{
	addr name;

	Check(stack == Nil, "stack error");
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tableblock(pos, &name);
		setlexical_tableblock(pos, increment_stack_eval(stack));
		setblock_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tableblock_lexical(stack, pos);
	}
}

static void push_tableblock(addr stack, addr name, addr *ret)
{
	addr pos;

	if (getblock_scope_evalstack(stack, name, ret))
		return;
	make_tableblock(&pos, name);
	setblock_scope_evalstack(stack, pos);
	push_tableblock_lexical(stack, pos);
	*ret = pos;
}

static void block_call_remove(addr stack, addr pos, addr *ret)
{
	if (getreference_tableblock(pos))
		*ret = pos;
	else
		*ret = Nil;
}

int scope_block_call(Execute ptr, addr name, addr cons,
		addr *rname, addr *rcons, addr *rtype)
{
	addr stack;

	Return(newstack_nil_(ptr, &stack));
	push_tableblock(stack, name, &name);
	Return(scope_allcons(ptr, rcons, rtype, cons));
	block_call_remove(stack, name, rname);

	return freestack_eval_(ptr, stack);
}


/*
 *  return-from
 */
static void push_closure_block(addr stack, addr tag, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tableblock(value);
	/* destination table */
	copy_tableblock(&pos, value);
	setclosurep_tableblock(pos, 1);
	setclosure_tableblock(pos, lexical);
	setlexical_tableblock(pos, increment_stack_eval(stack));
	setblock_lexical_evalstack(stack, pos);
	setblock_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int name_tableblock(addr stack, addr tag, addr *ret)
{
	addr next;

	/* not found */
	if (stack == Nil) {
		return 0;
	}

	/* local */
	if (getblock_scope_evalstack(stack, tag, ret)) {
		setreference_tableblock(*ret, 1);
		return 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (! name_tableblock(next, tag, ret)) {
		return 0;
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_block(stack, tag, *ret, ret);
	}

	return 1;
}

int scope_return_from_call(Execute ptr,
		addr name, addr form, addr *rname, addr *rexpr)
{
	addr stack;

	Return(getstack_eval_(ptr, &stack));
	if (! name_tableblock(stack, name, rname))
		return fmte_("Cannot find block name ~S.", name, NULL);

	return scope_eval(ptr, rexpr, form);
}


/* multiple-value-bind */
void scope_init_mvbind(struct mvbind_struct *str)
{
	clearpoint(str);
	str->stack = str->args = str->decl = str->doc
		= str->cons = str->free = str->the = str->allocate = Nil;
}

static int mvbind_maketable_(Execute ptr, struct mvbind_struct *str)
{
	int allocate;
	addr stack, decl, args, root, var;

	stack = str->stack;
	decl = str->decl;
	args = str->args;
	allocate = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		Return(check_scope_variable_(var));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		allocate |= getspecialp_tablevalue(var);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);
	str->allocate = allocate? T: Nil;

	return 0;
}

static int mvbind_execute(Execute ptr, struct mvbind_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(mvbind_maketable_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkvalue_(stack));

	return 0;
}

int scope_multiple_value_bind_call(Execute ptr, struct mvbind_struct *str)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &str->expr, str->expr));
	Return(newstack_nil_(ptr, &(str->stack)));
	Return(mvbind_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/* multiple-value-call */
static int scope_multiple_value_call_type(addr expr, addr *ret)
{
	GetEvalScopeThe(expr, &expr);
	if (! type_function_p(expr))
		return 1;
	GetArrayType(expr, 1, ret); /* result */

	return 0;
}

int scope_multiple_value_call_call(Execute ptr, addr expr, addr cons, addr *ret)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_allcons(hold, ptr, &cons, NULL, cons));
	localhold_end(hold);
	if (scope_multiple_value_call_type(expr, &type))
		GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_MULTIPLE_VALUE_CALL, type, Nil));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/************************************************************
 *  scope_check.c
 ************************************************************/

/*
 *  first
 */
static int scope_call_first(Execute ptr, addr *ret, addr first)
{
	switch (RefEvalParseType(first)) {
		case EVAL_PARSE_FUNCTION:
			return scope_function_call_(ptr, ret, first);

		case EVAL_PARSE_LAMBDA:
			return scope_lambda_call_(ptr, ret, first);

		default:
			*ret = Nil;
			return fmte_("Invalid parse object.", NULL);
	}

	return 0;
}


/*
 *  arguments
 */
static int check_tablecall_warning_(Execute ptr, addr name, addr type, addr expected)
{
	Return(type_object_(&type, type));
	Return(type_object_(&expected, expected));
	return call_type_error_va_(ptr, name, expected,
			"The object ~S expected a ~S type but the initialize form is ~S type.",
			name, expected, type, NULL);
}

static int check_tablecall(Execute ptr, addr eval, addr right, addr *ret)
{
	int check, errp;
	addr table, type, name;

	/* tablecall */
	Return(scope_eval(ptr, &eval, eval));
	make_tablecall(&table);
	copylocal_object(NULL, &right, right);
	settype_tablecall(table, right);
	setvalue_tablecall(table, eval);
	/* checktype */
	GetEvalScopeThe(eval, &type);
	Return(checktype_p_(ptr, type, right, &check, &errp));
	if (errp) {
		GetEvalScopeValue(eval, &name);
		Return(check_tablecall_warning_(ptr, name, type, right));
	}
	setcheck_tablecall(table, check);
	/* result */
	return Result(ret, table);
}

static int scope_call_args_loop(Execute ptr, addr *ret, addr type, addr args)
{
	addr root, eval, right;
	LocalHold hold;
	size_t i;

	hold = LocalHold_array(ptr, 1);
	root = Nil;
	for (i = 0; args != Nil; i++) {
		GetCons(args, &eval, &args);
		Return(gettype_ordcall_(type, i, &right));
		Return(check_tablecall(ptr, eval, right, &eval));
		cons_heap(&root, eval, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int scope_call_size_check_(Execute ptr, addr type, addr args)
{
	int check;
	size_t size;

	size = length_list_unsafe(args);
	Return(size_check_ordcall_(type, size, &check));
	if (! check) {
		Return(call_simple_style_warning_va_(NULL, "Invalid function call.", NULL));
	}

	return 0;
}

static int scope_call_args(Execute ptr, addr *ret, addr pos, addr args)
{
	addr type;

	GetEvalScopeThe(pos, &type);
	Return(scope_call_size_check_(ptr, type, args));
	return scope_call_args_loop(ptr, ret, type, args);
}


/*
 *  values
 */
static void scope_call_values(addr *ret, addr pos)
{
	GetEvalScopeThe(pos, &pos);
	CheckType(pos, LISPTYPE_TYPE);
	make_ordvalues_heap(pos, ret);
}


/*
 *  scope
 */
int scope_call_call_(Execute ptr, addr first, addr args, addr *ret)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(scope_call_first(ptr, &first, first));
	localhold_push(hold, first);
	Return(scope_call_args(ptr, &args, first, args));
	localhold_push(hold, args);
	scope_call_values(&type, first);
	localhold_push(hold, type);
	localhold_end(hold);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_CALL, type, Nil));
	SetEvalScopeIndex(eval, 0, first);
	SetEvalScopeIndex(eval, 1, args);
	return Result(ret, eval);
}


/************************************************************
 *  scope_declare.c
 ************************************************************/

/*
 *  apply_declare
 */
static int specialp_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, value;

	/* special */
	GetConst(SYSTEM_TYPE_SPECIAL, &key);
	if (find_plistlist_evalstack(stack, key, symbol)) {
		*ret = 1;
		return 1;
	}

	/* lexical */
	GetConst(SYSTEM_TYPE_LEXICAL, &key);
	if (find_plistlist_evalstack(stack, key, symbol)) {
		*ret = 0;
		return 1;
	}

	/* table value */
	if (getvalue_scope_evalstack(stack, symbol, &value)) {
		*ret = getspecialp_tablevalue(value);
		return 1;
	}

	return 0;
}

int specialp_tablevalue_(Execute ptr, addr stack, addr symbol, int *ret)
{
	int result;
	addr global_stack;

	/* symbol */
	if (specialp_symbol(symbol)) {
		return Result(ret, 1);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &global_stack));
	if (specialp_stack_tablevalue(global_stack, symbol, &result)) {
		if (result)
			return Result(ret, result);
		/* If symbol is lexical scope, find current stack. */
	}

	/* local stack */
	while (stack != Nil) {
		if (specialp_stack_tablevalue(stack, symbol, &result)) {
			return Result(ret, result);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* lexical */
	return Result(ret, 0);
}

int find_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr value;

	if (ret == NULL)
		ret = &value;
	return getvalue_scope_evalstack(stack, symbol, ret);
}

int find_tablefunction(addr stack, addr call, addr *ret)
{
	addr value;

	if (ret == NULL)
		ret = &value;
	return getfunction_scope_evalstack(stack, call, ret);
}

static int check_value_scope_(Execute ptr, addr stack, addr symbol, addr *ret)
{
	int specialp;
	addr pos;

	Return(specialp_tablevalue_(ptr, stack, symbol, &specialp));
	make_tablevalue(&pos, symbol);
	setspecialp_tablevalue(pos, specialp);

	return Result(ret, pos);
}

static int check_value_declare_(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablevalue(stack, key, NULL)) {
			Return(check_value_scope_(ptr, stack, key, &key));
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}

	return 0;
}

static int globalp_stack_tablefunction(addr stack, addr call)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* free declaration */
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		return 1;
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		return 1;
	}

	return 0;
}

int globalp_tablefunction_(Execute ptr, addr stack, addr call, int *ret)
{
	addr value, global_stack;

	/* local scope */
	while (stack != Nil) {
		if (globalp_stack_tablefunction(stack, call)) {
			return Result(ret, globalp_stack_eval(stack));
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global scope */
	Return(getglobal_eval_(ptr, &global_stack));
	if (globalp_stack_tablefunction(global_stack, call)) {
		return Result(ret, 1);
	}

	/* symbol */
	getglobal_callname(call, &value);
	if (value != Unbound) {
		return Result(ret, 1);
	}

	/* global */
	return Result(ret, 1);
}

static int check_function_scope_(Execute ptr, addr stack, addr call, addr *ret)
{
	int globalp;
	addr pos;

	copylocal_object(NULL, &call, call);
	Return(globalp_tablefunction_(ptr, stack, call, &globalp));
	make_tablefunction(&pos, call);
	setglobalp_tablefunction(pos, globalp);

	return Result(ret, pos);
}

static int check_function_declare_(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablefunction(stack, key, NULL)) {
			Return(check_function_scope_(ptr, stack, key, &key));
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}

	return 0;
}

static int check_declare_stack_(Execute ptr, addr stack, addr decl, addr *ret)
{
	addr root, cons;

	/* check */
	root = Nil;
	getall_type_value_declare(decl, &cons);
	Return(check_value_declare_(ptr, stack, cons, &root));
	getall_type_function_declare(decl, &cons);
	Return(check_function_declare_(ptr, stack, cons, &root));
	/* result */
	nreverse(ret, root);

	return 0;
}

int apply_declare_(Execute ptr, addr stack, addr decl, addr *ret)
{
	if (decl == Nil)
		return Result(ret, Nil);
	Return(check_declare_stack_(ptr, stack, decl, ret));
	apply_declare_stack(ptr->local, stack, decl);
	gchold_push_local(ptr->local, *ret);

	return 0;
}


/************************************************************
 *  scope_defun.c
 ************************************************************/

/*
 *  defun
 */
static int scope_defun_update_(Execute ptr, struct lambda_struct *str)
{
	addr stack, table;

	Return(getglobal_eval_(ptr, &stack));
	Return(push_tablefunction_global_(ptr, stack, str->call, &table));
	settype_tablefunction(table, str->the);

	return 0;
}

static int scope_defun_the_(addr eval, struct lambda_struct *str)
{
	addr cdr, type, null, setf, call;

	switch (RefCallNameType(str->call)) {
		case CALLNAME_SYMBOL:
			GetTypeTable(&type, Symbol);
			break;

		case CALLNAME_SETF:
			/* (setf hello) -> (cons (eql setf) (cons (eql hello) null)) */
			GetConst(COMMON_SETF, &setf);
			type_eql_heap(setf, &setf);
			GetCallName(str->call, &call);
			type_eql_heap(call, &call);
			GetTypeTable(&null, Null);
			type2_heap(LISPDECL_CONS, call, null, &cdr);
			type2_heap(LISPDECL_CONS, setf, cdr, &type);
			break;

		default:
			return fmte_("callname error.", NULL);
	}
	SetEvalScopeThe(eval, type);

	return 0;
}

int scope_defun_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_lambda_object_(ptr, str, &eval));
	Return(scope_defun_update_(ptr, str));
	Return(scope_defun_the_(eval, str));

	return Result(ret, eval);
}


/*
 *  deftype
 */
int scope_deftype_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_macro_lambda_object_(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  define-compiler-macro
 */
int scope_define_compiler_macro_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_macro_lambda_object_(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  destructuring-bind
 */
static int scope_bind_lambda_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(scope_macro_lambda_execute_(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

int scope_bind_call_(Execute ptr, addr *ret, addr expr, addr args)
{
	addr lambda, eval;
	struct lambda_struct str;

	/* macro-lambda */
	scope_init_lambda(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(args, 0, &str.args);
	GetEvalParse(args, 1, &str.decl);
	GetEvalParse(args, 2, &str.doc);
	GetEvalParse(args, 3, &str.cons);
	Return(scope_bind_lambda_(ptr, &str, &lambda));

	/* result */
	Return(eval_scope_size_(ptr, &eval, 5,
				EVAL_PARSE_DESTRUCTURING_BIND, str.body_the, Nil));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, str.args);
	SetEvalScopeIndex(eval, 2, str.decl);
	SetEvalScopeIndex(eval, 3, str.cons);
	SetEvalScopeIndex(eval, 4, str.free);
	return Result(ret, eval);
}


/*
 *  flet
 */
static int scope_flet_lambda_(Execute ptr, addr list, addr *call, addr *ret)
{
	addr pos;
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	*call = str.call;
	str.call = Nil;
	Return(scope_lambda_object_(ptr, &str, &pos));
	SetEvalScopeIndex(pos, EvalLambda_Call, *call);

	return Result(ret, pos);
}

static int scope_flet_init_(Execute ptr, struct let_struct *str)
{
	addr args, root, call, eval;

	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(scope_flet_lambda_(ptr, eval, &call, &eval));
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static void scope_flet_maketable(Execute ptr, struct let_struct *str)
{
	addr stack, args, list, call;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &list, &args);
		GetCar(list, &call);
		make_tablefunction_stack(ptr, &call, stack, call);
		SetCar(list, call);
	}
}

static int scope_checktype_function_(Execute ptr, addr table, addr eval)
{
	int check, errorp;
	addr name, type;

	getname_tablefunction(table, &name);
	gettype_tablefunction(table, &type);
	if (type == Nil)
		return 0;
	GetEvalScopeThe(eval, &eval);
	Return(checktype_p_(ptr, type, eval, &check, &errorp));
	if (errorp) {
		GetCallName(name, &name);
		Return(type_object_(&eval, eval));
		Return(type_object_(&type, type));
		Return(fmte_("The function ~S must be ~S type, but ~S.",
					name, eval, type, NULL));
	}

	return 0;
}

static int scope_flet_applytable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, call, eval;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		Return(update_tablefunction_(ptr, stack, call));
		Return(scope_checktype_function_(ptr, call, eval));
	}

	return 0;
}

static int scope_flet_ignore_(addr stack)
{
	enum IgnoreType ignore;
	int reference;
	addr list, pos, call, value;

	GetEvalStackScope(stack, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (gettype_evaltable(pos) != EvalTable_Function)
			continue;
		get_evaltable(pos, &value);
		getname_tablefunction(value, &call);
		/* check ignore */
		ignore = getignore_tablefunction(value);
		reference = getreference_tablefunction(value);

		if (ignore == IgnoreType_None && (! reference)) {
			name_callname_heap(call, &call);
			Return(call_simple_style_warning_va_(NULL,
						"Unused function ~S.", call, NULL));
		}
		if (ignore == IgnoreType_Ignore && reference) {
			name_callname_heap(call, &call);
			Return(call_simple_style_warning_va_(NULL,
						"Ignore function ~S used.", call, NULL));
		}
	}

	return 0;
}

static int scope_flet_execute_(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(scope_flet_init_(ptr, str));
	scope_flet_maketable(ptr, str);
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(scope_flet_applytable_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(scope_flet_ignore_(stack));

	return 0;
}

int scope_flet_call_(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(scope_flet_execute_(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/*
 *  labels
 */
static void scope_labels_table_push(Execute ptr, addr stack, addr list)
{
	addr call, args, decl, pos;

	Lista_bind(list, &call, &args, &decl, &pos, NULL);
	make_tablefunction_stack(ptr, &pos, stack, call);
	apply_declare_function_stack(ptr->local, stack, call, decl);
}

static void scope_labels_table(Execute ptr, struct let_struct *str)
{
	addr args, stack, eval;

	args = str->args;
	stack = str->stack;
	while (args != Nil) {
		GetCons(args, &eval, &args);
		scope_labels_table_push(ptr, stack, eval);
	}
}

static int scope_labels_lambda_(Execute ptr, addr list, addr *ret)
{
	addr eval;
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	Return(scope_lambda_object_(ptr, &str, &eval));
	return Result(ret, eval);
}

static int scope_labels_init_(Execute ptr, struct let_struct *str)
{
	addr stack, args, root, call, eval;

	stack = str->stack;
	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(scope_labels_lambda_(ptr, eval, &eval));
		GetEvalScopeIndex(eval, EvalLambda_Call, &call);
		Return(push_tablefunction_global_(ptr, stack, call, &call));
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static int scope_labels_checktype_(Execute ptr, struct let_struct *str)
{
	addr args, call, eval;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		Return(scope_checktype_function_(ptr, call, eval));
	}

	return 0;
}

static int scope_labels_execute_(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	scope_labels_table(ptr, str);
	Return(scope_labels_init_(ptr, str));
	Return(scope_labels_checktype_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(scope_flet_ignore_(stack));

	return 0;
}

int scope_labels_call_(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(scope_labels_execute_(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/************************************************************
 *  scope_function.c
 ************************************************************/

/*
 *  function
 */
static int dynamic_stack_tablefunction(addr stack, addr call, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_FUNCTION, &key);
	if (getplist(table, key, &value) == 0
			&& find_list_callname_unsafe(call, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getdynamic_tablefunction(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablefunction(addr stack, addr call)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablefunction(addr stack, addr call, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_FUNCTION, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_IGNORE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignore;
			return 1;
		}
		GetConst(COMMON_IGNORABLE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignorable;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getignore_tablefunction(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablefunction(addr stack, addr call)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int inline_stack_tablefunction(addr stack, addr call, enum InlineType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* inline, notinline declaration */
	GetConst(SYSTEM_INLINE, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_INLINE, &check);
		if (check == value) {
			*ret = InlineType_Inline;
			return 1;
		}
		GetConst(COMMON_NOTINLINE, &check);
		if (check == value) {
			*ret = InlineType_NotInline;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getinline_tablefunction(value);
		return 1;
	}

	return 0;
}
static int inline_tablefunction_(Execute ptr,
		addr stack, addr call, enum InlineType *ret)
{
	enum InlineType result;

	/* local stack */
	while (stack != Nil) {
		if (inline_stack_tablefunction(stack, call, &result)) {
			return Result(ret, result);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	if (inline_stack_tablefunction(stack, call, &result)) {
		return Result(ret, result);
	}

	/* not inline or notinline */
	return Result(ret, InlineType_None);
}

static void gettype_global_callname(LocalRoot local, addr call, addr *ret)
{
	CallNameType check;

	GetCallNameType(call, &check);
	GetCallName(call, &call);
	if (check == CALLNAME_SYMBOL)
		gettype_function_symbol(call, ret);
	else
		gettype_setf_symbol(call, ret);
}

static int type_free_tablefunction(addr stack, addr call, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	return getpplist_callname(stack, key, call, ret) == 0;
}

static int type_boundary_tablefunction(addr stack, addr call, addr *ret)
{
	if (! find_tablefunction(stack, call, &call))
		return 0;
	gettype_tablefunction(call, ret);
	return *ret != Nil;
}

static int type_tablefunction_(Execute ptr, LocalRoot local,
		addr stack, addr call, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablefunction(stack, call, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablefunction(stack, call, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	/* free declaration */
	if (type_free_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_global_callname(local, call, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse(ret, root);
	return 0;
}

static void push_tablefunction_lexical(Execute ptr, addr stack, addr pos)
{
	addr name;

	if (stack == Nil) {
		setglobalp_tablefunction(pos, 1);
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tablefunction(pos, &name);
		setlexical_tablefunction(pos, increment_stack_eval(stack));
		setfunction_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablefunction_lexical(ptr, stack, pos);
	}
}

static void tablefunction_update(Execute ptr, addr stack, addr call)
{
	push_tablefunction_lexical(ptr, stack, call);
}

int make_tablefunction_stack(Execute ptr, addr *ret, addr stack, addr call)
{
	addr pos, aster;

	CheckType(call, LISPTYPE_CALLNAME);
	if (getfunction_scope_evalstack(stack, call, ret))
		return 0;
	copyheap(&call, call);
	make_tablefunction(&pos, call);
	GetTypeTable(&aster, Function);
	settype_tablefunction(pos, aster);
	setfunction_scope_evalstack(stack, pos);
	tablefunction_update(ptr, stack, pos);
	*ret = pos;

	return 1;
}

static int function_type_and_array(addr cons, addr *ret)
{
	addr array, type, pos;
	size_t size;

	/* array size */
	size = 0;
	type = Nil;
	for (array = cons; array != Nil; ) {
		GetCons(array, &pos, &array);
		if (! type_function_aster_p(pos)) {
			type = pos;
			size++;
		}
	}
	if (size == 0) {
		return 1;
	}
	if (size == 1) {
		CheckType(type, LISPTYPE_TYPE);
		copylocal_object(NULL, ret, type);
		return 0;
	}

	/* type-and */
	vector4_heap(&array, size);
	for (size = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		CheckType(pos, LISPTYPE_TYPE);
		if (! type_function_aster_p(pos)) {
			copylocal_object(NULL, &pos, pos);
			SetArrayA4(array, size++, pos);
		}
	}
	type1_heap(LISPDECL_AND, array, ret);
	return 0;
}

int update_tablefunction_(Execute ptr, addr stack, addr pos)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr name, type;

	/* scope */
	getname_tablefunction(pos, &name);
	Return(globalp_tablefunction_(ptr, stack, name, &globalp));
	dynamic = dynamic_tablefunction(stack, name);
	ignore = ignore_tablefunction(stack, name);
	Return(inline_tablefunction_(ptr, stack, name, &Inline));
	Return(type_tablefunction_(ptr, NULL, stack, name, &type));
	if (function_type_and_array(type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);

	return 0;
}

int push_tablefunction_global_(Execute ptr, addr stack, addr call, addr *ret)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr pos, type;

	/* scope */
	Return(globalp_tablefunction_(ptr, stack, call, &globalp));
	dynamic = dynamic_tablefunction(stack, call);
	ignore = ignore_tablefunction(stack, call);
	Return(inline_tablefunction_(ptr, stack, call, &Inline));
	Return(type_tablefunction_(ptr, NULL, stack, call, &type));
	if (function_type_and_array(type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	make_tablefunction_stack(ptr, &pos, stack, call);
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);

	return Result(ret, pos);
}

static int callname_global_tablefunction_(Execute ptr, addr *ret, addr call)
{
	addr stack;

	Return(getglobal_eval_(ptr, &stack));
	if (! find_tablefunction(stack, call, ret)) {
		Return(push_tablefunction_global_(ptr, stack, call, ret));
	}

	return 0;
}

static void push_closure_function(addr stack, addr call, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tablefunction(value);
	/* destination table */
	make_redirect_tablefunction(&pos, value);
	setclosurep_tablefunction(pos, 1);
	setclosure_tablefunction(pos, lexical);
	setlexical_tablefunction(pos, increment_stack_eval(stack));
	setfunction_lexical_evalstack(stack, pos);
	setfunction_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int callname_tablefunction_(Execute ptr,
		addr stack, addr call, addr *value, int *ret)
{
	int check;
	addr next;

	/* global */
	if (stack == Nil) {
		Return(callname_global_tablefunction_(ptr, value, call));
		return Result(ret, 1); /* global-scope */
	}

	/* local */
	if (getfunction_scope_evalstack(stack, call, value)) {
		setreference_tablefunction(*value, 1);
		return Result(ret, getglobalp_tablefunction(*value));
	}

	/* next */
	GetEvalStackNext(stack, &next);
	Return(callname_tablefunction_(ptr, next, call, value, &check));
	if (check) {
		return Result(ret, 1); /* global-scope */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_function(stack, call, *value, value);
	}

	return Result(ret, 0); /* local-scope */
}

static int scope_function_object_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	gettype_function(eval, &type);
	if (type == Nil)
		GetTypeTable(&type, Function);

	return make_eval_scope_(ptr, ret, EVAL_PARSE_FUNCTION, type, eval);
}

static int scope_function_callname_(Execute ptr, addr *ret, addr call)
{
	int ignore;
	addr value, type, stack;

	Return(getstack_eval_(ptr, &stack));
	Return(callname_tablefunction_(ptr, stack, call, &value, &ignore));
	gettype_tablefunction(value, &type);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_FUNCTION, type, value);
}

int scope_function_call_(Execute ptr, addr *ret, addr eval)
{
	GetEvalParse(eval, 0, &eval);
	if (functionp(eval))
		return scope_function_object_(ptr, ret, eval);
	if (callnamep(eval))
		return scope_function_callname_(ptr, ret, eval);

	return fmte_("Invalid object type ~S", eval, NULL);
}


/************************************************************
 *  scope_lambda.c
 ************************************************************/

void scope_init_lambda(struct lambda_struct *str, EvalParse eval, int globalp)
{
	clearpoint(str);
	str->stack = str->call = str->table = str->lexical =
		str->args = str->decl = str->doc = str->cons =
		str->clos = str->free = str->the =
		str->form = str->defun = str->body_the = Nil;
	str->globalp = globalp;
	str->eval = eval;
}

void localhold_lambda_struct(LocalRoot local, struct lambda_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->call, str->table, str->lexical,
			str->args, str->decl, str->doc, str->cons,
			str->clos, str->free, str->the,
			str->form, str->defun, str->body_the, NULL);
}

void lambda_lexical_heap(addr stack, addr *ret)
{
	addr index, list, root, pos;

	getlexical_index_heap(stack, &index);
	if (index == Nil) {
		*ret = Nil;
		return;
	}

	/* closure index */
	GetEvalStackLexical(stack, &list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (gettype_evaltable(pos) == EvalTable_Value) {
			get_evaltable(pos, &pos);
			if (getbasep_tablevalue(pos)) {
				index_heap(&pos, getlexical_tablevalue(pos));
				cons_heap(&root, pos, root);
			}
		}
	}

	/* result */
	cons_heap(ret, index, root);
}


/*
 *  lambda
 */
static int scope_lambda_init_var_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr var, list;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &var, &args);
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		cons_heap(&list, var, list);
	}
	nreverse(ret, list);

	return 0;
}

static int scope_lambda_init_opt_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		Return(ifdeclvalue_(ptr, stack, svar, decl, &svar));
		list_heap(&var, var, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_lambda_init_key_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		Return(ifdeclvalue_(ptr, stack, svar, decl, &svar));
		list_heap(&var, var, name, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_lambda_init_aux_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		list_heap(&var, var, init, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_lambda_init_(Execute ptr, struct lambda_struct *str)
{
	addr stack, decl, args, var, opt, rest, key, allow, aux;

	stack = str->stack;
	decl = str->decl;
	args = str->args;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* scope */
	Return(scope_lambda_init_var_(ptr, stack, var, decl, &var));
	Return(scope_lambda_init_opt_(ptr, stack, opt, decl, &opt));
	Return(ifdeclvalue_(ptr, stack, rest, decl, &rest));
	Return(scope_lambda_init_key_(ptr, stack, key, decl, &key));
	Return(scope_lambda_init_aux_(ptr, stack, aux, decl, &aux));
	list_heap(&str->args, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

static void scope_lambda_tablevalue_force(addr args)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		setcheck_tablevalue(pos, 1);
	}
}

static void scope_lambda_tablevalue_single(addr pos)
{
	if (pos != Nil) {
		setcheck_tablevalue(pos, 1);
	}
}

static int scope_lambda_tablevalue_opt_(Execute ptr, addr args)
{
	addr list, var, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int scope_lambda_tablevalue_key_(Execute ptr, addr args)
{
	addr list, var, name, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int scope_lambda_tablevalue_aux_(Execute ptr, addr args)
{
	addr list, var, init;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int scope_lambda_tablevalue_(Execute ptr, addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	scope_lambda_tablevalue_force(var);
	Return(scope_lambda_tablevalue_opt_(ptr, opt));
	scope_lambda_tablevalue_single(rest);
	Return(scope_lambda_tablevalue_key_(ptr, key));
	Return(scope_lambda_tablevalue_aux_(ptr, aux));

	return 0;
}

static void scope_lambda_type_var(addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		gettype_tablevalue(var, &var);
		copyheap(&var, var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void scope_lambda_type_opt(addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCar(var, &var);
		gettype_tablevalue(var, &var);
		copyheap(&var, var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void scope_lambda_type_rest(addr rest, addr *ret)
{
	if (rest != Nil)
		GetTypeTable(ret, T);
}

static void scope_lambda_type_key(addr args, addr allow, addr *ret)
{
	addr root, var, name;

	if (allow != Nil) {
		/* &allow-other-keys */
		*ret = T;
		return;
	}
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &name);
		GetCar(name, &name);
		gettype_tablevalue(var, &var);
		copyheap(&var, var);
		cons_heap(&var, name, var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void scope_lambda_type_make(addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, array;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);

	/* arg-typespec */
	scope_lambda_type_var(var, &var);
	scope_lambda_type_opt(opt, &opt);
	scope_lambda_type_rest(rest, &rest);
	scope_lambda_type_key(key, allow, &key);

	/* type-function vector */
	vector2_heap(&array, 4);
	SetArrayA2(array, 0, var);
	SetArrayA2(array, 1, opt);
	SetArrayA2(array, 2, rest);
	SetArrayA2(array, 3, key); /* &key or &allow-other-keys */
	*ret = array;
}

static void scope_lambda_type_incomplete(addr args, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	scope_lambda_type_make(args, &args);
	type3_heap(LISPDECL_FUNCTION, args, aster, Nil, ret);
}

static void scope_lambda_type_table(Execute ptr, struct lambda_struct *str, addr type)
{
	addr table;

	CheckType(str->call, LISPTYPE_CALLNAME);
	make_tablefunction_stack(ptr, &table, str->stack, str->call);
	setglobalp_tablefunction(table, 0);
	settype_tablefunction(table, type);
	str->table = table;
}

static void scope_lambda_declare(Execute ptr, struct lambda_struct *str)
{
	addr type;

	/* incomplete type */
	scope_lambda_type_incomplete(str->args, &type);
	str->the = type;

	/* tablefunction */
	if (str->call != Nil)
		scope_lambda_type_table(ptr, str, type);
}

static int scope_lambda_progn_(Execute ptr, struct lambda_struct *str)
{
	addr the, type;

	Return(scope_allcons(ptr, &str->cons, &type, str->cons));
	gchold_pushva_local(ptr->local, str->cons, type, NULL);
	/* (function [args] *) -> (function [args] [values]) */
	the = str->the;
	SetArrayType(the, 1, type);
	copylocal_object(NULL, &the, the);
	if (str->table != Nil)
		settype_tablefunction(str->table, the);
	str->the = the;

	return 0;
}

static void scope_lambda_closure_list(addr stack, addr *ret)
{
	addr list, root, pos;

	GetEvalStackLexical(stack, &list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (getclosurep_evaltable(pos))
			cons_heap(&root, pos, root);
	}
	*ret = root;
}

static int scope_lambda_closure_(Execute ptr, struct lambda_struct *str)
{
	addr stack, list;

	Return(getstack_eval_(ptr, &stack));
	scope_lambda_closure_list(stack, &list);
	str->clos = list;

	return 0;
}

static void scope_lambda_lexical(struct lambda_struct *str)
{
	lambda_lexical_heap(str->stack, &str->lexical);
}

static int scope_lambda_heap_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(eval_scope_size_(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	SetEvalScopeIndex(eval, EvalLambda_Table, str->table);
	SetEvalScopeIndex(eval, EvalLambda_Args, str->args);
	SetEvalScopeIndex(eval, EvalLambda_Decl, str->decl);
	SetEvalScopeIndex(eval, EvalLambda_Doc, str->doc);
	SetEvalScopeIndex(eval, EvalLambda_Cons, str->cons);
	SetEvalScopeIndex(eval, EvalLambda_Clos, str->clos);
	SetEvalScopeIndex(eval, EvalLambda_The, str->the);
	SetEvalScopeIndex(eval, EvalLambda_Free, str->free);
	SetEvalScopeIndex(eval, EvalLambda_Form, str->form);
	SetEvalScopeIndex(eval, EvalLambda_Defun, str->defun);
	SetEvalScopeIndex(eval, EvalLambda_Lexical, str->lexical);

	return Result(ret, eval);
}

static int scope_lambda_execute_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack;

	stack = str->stack;
	Return(scope_lambda_init_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(scope_lambda_tablevalue_(ptr, str->args));
	scope_lambda_declare(ptr, str);
	Return(scope_lambda_progn_(ptr, str));
	Return(ignore_checkvalue_(stack));
	Return(scope_lambda_closure_(ptr, str));
	scope_lambda_lexical(str);
	Return(scope_lambda_heap_(ptr, str, ret));

	return 0;
}

int scope_lambda_object_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_lambda_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(scope_lambda_execute_(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

int scope_lambda_call_(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_LAMBDA, 0);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	GetEvalParse(eval, 4, &str.form);
	return scope_lambda_object_(ptr, &str, ret);
}


/*
 *  macro-lambda
 */
static int macro_lambda_init_args_(Execute ptr, addr, addr, addr, addr *);
static int macro_lambda_init_var_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			Return(macro_lambda_init_args_(ptr, stack, var, decl, &var));
		}
		else {
			Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		}
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int macro_lambda_init_rest_(Execute ptr,
		addr stack, addr rest, addr decl, addr *ret)
{
	addr var, type;

	if (rest == Nil) {
		*ret = Nil;
	}
	else {
		GetCons(rest, &var, &type);
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		cons_heap(ret, var, type);
	}

	return 0;
}

static int macro_lambda_init_args_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_lambda_init_var_(ptr, stack, var, decl, &var));
	Return(scope_lambda_init_opt_(ptr, stack, opt, decl, &opt));
	Return(macro_lambda_init_rest_(ptr, stack, rest, decl, &rest));
	Return(scope_lambda_init_key_(ptr, stack, key, decl, &key));
	Return(scope_lambda_init_aux_(ptr, stack, aux, decl, &aux));
	Return(ifdeclvalue_(ptr, stack, whole, decl, &whole));
	Return(ifdeclvalue_(ptr, stack, env, decl, &env));
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int macro_lambda_init_(Execute ptr, struct lambda_struct *str)
{
	return macro_lambda_init_args_(ptr, str->stack, str->args, str->decl, &str->args);
}

static int macro_lambda_tablevalue_(Execute ptr, addr args);
static int macro_lambda_tablevalue_var_(Execute ptr, addr args)
{
	addr var;

	while (args != Nil) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			Return(macro_lambda_tablevalue_(ptr, var));
		}
		else {
			scope_lambda_tablevalue_single(var);
		}
	}

	return 0;
}

static void macro_lambda_tablevalue_rest(addr rest)
{
	if (rest != Nil) {
		GetCar(rest, &rest);
		setcheck_tablevalue(rest, 1);
	}
}

static int macro_lambda_tablevalue_(Execute ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_lambda_tablevalue_var_(ptr, var));
	Return(scope_lambda_tablevalue_opt_(ptr, opt));
	macro_lambda_tablevalue_rest(rest);
	Return(scope_lambda_tablevalue_key_(ptr, key));
	Return(scope_lambda_tablevalue_aux_(ptr, aux));
	scope_lambda_tablevalue_single(whole);
	scope_lambda_tablevalue_single(env);

	return 0;
}

static int macro_lambda_progn_(Execute ptr, struct lambda_struct *str)
{
	return scope_allcons(ptr, &str->cons, &str->body_the, str->cons);
}

static int scope_macro_lambda_heap_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(eval_scope_size_(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	SetEvalScopeIndex(eval, EvalLambda_Args, str->args);
	SetEvalScopeIndex(eval, EvalLambda_Decl, str->decl);
	SetEvalScopeIndex(eval, EvalLambda_Doc, str->doc);
	SetEvalScopeIndex(eval, EvalLambda_Cons, str->cons);
	SetEvalScopeIndex(eval, EvalLambda_Clos, str->clos);
	SetEvalScopeIndex(eval, EvalLambda_The, str->the);
	SetEvalScopeIndex(eval, EvalLambda_Free, str->free);
	SetEvalScopeIndex(eval, EvalLambda_Lexical, str->lexical);

	return Result(ret, eval);
}

int scope_macro_lambda_execute_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack;

	stack = str->stack;
	Return(macro_lambda_init_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(macro_lambda_tablevalue_(ptr, str->args));
	Return(macro_lambda_progn_(ptr, str));
	Return(ignore_checkvalue_(stack));
	Return(scope_lambda_closure_(ptr, str));
	scope_lambda_lexical(str);

	return scope_macro_lambda_heap_(ptr, str, ret);
}

int scope_macro_lambda_object_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_lambda_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(scope_macro_lambda_execute_(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

static void scope_macro_lambda_the(addr eval)
{
	addr type;
	GetTypeCompiled(&type, MacroFunction);
	SetEvalScopeThe(eval, type);
}

int scope_macro_lambda_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_macro_lambda_object_(ptr, str, &eval));
	scope_macro_lambda_the(eval);
	return Result(ret, eval);
}


/************************************************************
 *  scope_let.c
 ************************************************************/

/*
 *  let
 */
void scope_init_let(struct let_struct *str)
{
	clearpoint(str);
	str->stack = str->args = str->decl = str->doc
		= str->cons = str->free = str->the = str->allocate = Nil;
}

int check_scope_variable_(addr symbol)
{
	Check(! symbolp(symbol), "type error");
	if (keywordp(symbol))
		return fmte_("Keyword ~S can't be use a variable.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		return fmte_("The constant of symbol ~S can't use a variable.", symbol, NULL);

	return 0;
}

static int let_init(Execute ptr, struct let_struct *str)
{
	addr args, root, var, init;

	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(check_scope_variable_(var));
		Return(scope_eval(ptr, &init, init));
		cons_heap(&var, var, init);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static void push_tablevalue_lexical(Execute ptr, addr stack, addr pos, int doublep)
{
	if (stack == Nil) {
		setglobalp_tablevalue(pos, 1);
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		setlexical_tablevalue(pos, increment_stack_eval(stack));
		setvalue_lexical_evalstack(stack, pos);
		if (doublep) {
			setlet_tablevalue(pos, increment_stack_eval(stack));
			setvalue_lexical_evalstack(stack, pos);
		}
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablevalue_lexical(ptr, stack, pos, doublep);
	}
}

static void push_tablevalue_special(Execute ptr, addr stack, addr pos)
{
	if (stack == Nil) {
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		setlet_tablevalue(pos, increment_stack_eval(stack));
		setvalue_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablevalue_special(ptr, stack, pos);
	}
}

static void tablevalue_update(Execute ptr, addr stack, addr var, int doublep)
{
	if (var == Nil)
		return;
	if (! getspecialp_tablevalue(var)) {
		push_tablevalue_lexical(ptr, stack, var, doublep);
		return;
	}
	if (doublep) {
		push_tablevalue_special(ptr, stack, var);
		return;
	}
}

static int make_tablevalue_stack(Execute ptr,
		addr *ret, addr stack, addr symbol, int doublep)
{
	addr pos, aster;

	CheckType(symbol, LISPTYPE_SYMBOL);
	if (getvalue_scope_evalstack(stack, symbol, ret))
		return 0;
	make_tablevalue(&pos, symbol);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	setvalue_scope_evalstack(stack, pos);
	tablevalue_update(ptr, stack, pos, doublep);
	*ret = pos;

	return 1;
}

static int make_tablevalue_special_stack(Execute ptr,
		addr *ret, addr stack, addr symbol)
{
	addr pos, aster;

	CheckType(symbol, LISPTYPE_SYMBOL);
	if (find_global_special_evalstack(stack, symbol, ret))
		return 0;
	make_tablevalue(&pos, symbol);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	push_global_special_evalstack(stack, pos);
	*ret = pos;

	return 1;
}

static int let_maketable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, list, var, eval;

	stack = str->stack;
	decl = str->decl;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &list, &args);
		GetCar(list, &var);
		make_tablevalue_stack(ptr, &eval, stack, var, 1);
		Return(apply_declare_let_stack_(ptr, stack, var, decl));
		SetCar(list, eval);
	}

	return 0;
}

static int dynamic_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, value;

	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_VALUE, &key);
	if (find_plistlist_evalstack(stack, key, symbol)) {
		*ret = 1;
		return 1;
	}

	/* table value */
	if (getvalue_scope_evalstack(stack, symbol, &value)) {
		*ret = getdynamic_tablevalue(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablevalue(addr stack, addr symbol)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablevalue(addr stack, addr symbol, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_VALUE, &key);
	if (getpplist(table, key, symbol, &value) == 0) {
		GetConst(COMMON_IGNORE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignore;
			return 1;
		}
		GetConst(COMMON_IGNORABLE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignorable;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getvalue_scope_evalstack(stack, symbol, &value)) {
		*ret = getignore_tablevalue(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablevalue(addr stack, addr symbol)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int type_free_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_VALUE, &key);
	return getpplist(stack, key, symbol, ret) == 0;
}

static int type_boundary_tablevalue(addr stack, addr symbol, addr *ret)
{
	if (! find_tablevalue(stack, symbol, &symbol))
		return 0;
	gettype_tablevalue(symbol, ret);
	return 1;
}

static int type_tablevalue_(Execute ptr, LocalRoot local,
		addr stack, addr symbol, int specialp, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablevalue(stack, symbol, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablevalue(stack, symbol, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			if (! specialp)
				goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	/* free declaration */
	if (type_free_tablevalue(stack, symbol, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablevalue(stack, symbol, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_value_symbol(symbol, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse(ret, root);
	return 0;
}

static int type_and_array(addr cons, addr *ret)
{
	addr array, type, pos;
	size_t size;

	/* array size */
	size = 0;
	type = Nil;
	for (array = cons; array != Nil; ) {
		GetCons(array, &pos, &array);
		if (! type_astert_p(pos)) {
			type = pos;
			size++;
		}
	}
	if (size == 0) {
		return 1;
	}
	if (size == 1) {
		CheckType(type, LISPTYPE_TYPE);
		copylocal_object(NULL, ret, type);
		return 0;
	}

	/* type-and */
	vector4_heap(&array, size);
	for (size = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		CheckType(pos, LISPTYPE_TYPE);
		if (! type_astert_p(pos)) {
			copylocal_object(NULL, &pos, pos);
			SetArrayA4(array, size++, pos);
		}
	}
	type1_heap(LISPDECL_AND, array, ret);
	return 0;
}

static int update_tablevalue_(Execute ptr, addr stack, addr pos)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr name, type;

	/* scope */
	getname_tablevalue(pos, &name);
	Return(specialp_tablevalue_(ptr, stack, name, &specialp));
	dynamic = dynamic_tablevalue(stack, name);
	ignore = ignore_tablevalue(stack, name);
	Return(type_tablevalue_(ptr, NULL, stack, name, specialp, &type));
	if (type_and_array(type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);

	return 0;
}

int push_tablevalue_global_(Execute ptr, addr stack, addr symbol, addr *ret)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr pos, type;

	/* scope */
	Return(specialp_tablevalue_(ptr, stack, symbol, &specialp));
	dynamic = dynamic_tablevalue(stack, symbol);
	ignore = ignore_tablevalue(stack, symbol);
	Return(type_tablevalue_(ptr, NULL, stack, symbol, specialp, &type));
	if (type_and_array(type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	make_tablevalue_stack(ptr, &pos, stack, symbol, 0);
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);

	return Result(ret, pos);
}

int push_tablevalue_special_global_(Execute ptr, addr stack, addr symbol, addr *ret)
{
	addr pos, type;

	/* scope */
	Return(type_tablevalue_(ptr, NULL, stack, symbol, 1, &type));
	if (type_and_array(type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	make_tablevalue_special_stack(ptr, &pos, stack, symbol);
	setspecialp_tablevalue(pos, 1);
	setdynamic_tablevalue(pos, 0);
	setignore_tablevalue(pos, IgnoreType_None);
	settype_tablevalue(pos, type);

	return Result(ret, pos);
}

static int checktype_subtypep_(Execute ptr, addr x, addr y, int *check, int *errp)
{
	SubtypepResult value;

	CheckType(x, LISPTYPE_TYPE);
	CheckType(y, LISPTYPE_TYPE);
	Return(subtypep_scope_(ptr, x, y, Nil, &value));
	switch (value) {
		case SUBTYPEP_INCLUDE:
			/* type check can skip. */
			*check = 0;
			return Result(errp, 0);

		case SUBTYPEP_EXCLUDE:
			/* error, output to warning mesasge. */
			*check = 1;
			return Result(errp, 1);

		case SUBTYPEP_FALSE:
		case SUBTYPEP_INVALID:
		default:
			/* type check must execute. */
			*check = 1;
			return Result(errp, 0);
	}
}

int checktype_p_(Execute ptr, addr x, addr y, int *check, int *errp)
{
	int value;

	/* type-error */
	Return(check_error_type_(ptr, x, &value));
	if (! value)
		goto type_error;
	Return(check_error_type_(ptr, y, &value));
	if (! value)
		goto type_error;

	/* subtypep */
	return checktype_subtypep_(ptr, x, y, check, errp);

type_error:
	*check = 1;
	*errp = 0;
	return 0;
}

static int checktype_warning_(Execute ptr, addr name, addr type, addr expected)
{
	Return(type_object_(&type, type));
	Return(type_object_(&expected, expected));
	return call_type_error_va_(ptr, name, expected,
			"The object ~S must be a ~S type but ~S type.",
			name, expected, type, NULL);
}

int checktype_value_(Execute ptr, addr value, addr init)
{
	int check, errp;
	addr type, name;

	gettype_tablevalue(value, &type);
	GetEvalScopeThe(init, &init);
	Return(checktype_p_(ptr, init, type, &check, &errp));
	if (errp) {
		getname_tablevalue(value, &name);
		Return(checktype_warning_(ptr, name, type, init));
	}
	setcheck_tablevalue(value, check);

	return 0;
}

static int let_applytable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, var, init;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(update_tablevalue_(ptr, stack, var));
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

int ignore_checkvalue_(addr stack)
{
	enum IgnoreType ignore;
	int reference, special;
	addr list, pos, name, value;

	GetEvalStackScope(stack, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (gettype_evaltable(pos) != EvalTable_Value)
			continue;
		get_evaltable(pos, &value);
		getname_tablevalue(value, &name);
		/* check ignore */
		ignore = getignore_tablevalue(value);
		reference = getreference_tablevalue(value);
		special = getspecialp_tablevalue(value);

		if (ignore == IgnoreType_None && (! reference) && (! special)) {
			Return(call_simple_style_warning_va_(NULL,
						"Unused variable ~S.", name, NULL));
		}
		if (ignore == IgnoreType_Ignore && reference) {
			Return(call_simple_style_warning_va_(NULL,
						"Ignore variable ~S used.", name, NULL));
		}
	}

	return 0;
}

static int let_allocate_args(struct let_struct *str)
{
	addr list, var;

	list = str->args;
	while (list != Nil) {
		GetCons(list, &var, &list);
		GetCar(var, &var);
		if (getspecialp_tablevalue(var))
			return 1;
	}

	return 0;
}

static void let_allocate(struct let_struct *str)
{
	str->allocate = let_allocate_args(str)? T: Nil;
}

static int let_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(let_init(ptr, str));
	Return(let_maketable_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(let_applytable_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkvalue_(stack));
	let_allocate(str);

	return 0;
}

void localhold_let_struct(LocalRoot local, struct let_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->args, str->decl,
			str->doc, str->cons, str->free, str->the, str->allocate, NULL);
}

int scope_let_call(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(let_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/*
 *  let*
 */
int ifdeclvalue_(Execute ptr, addr stack, addr var, addr decl, addr *ret)
{
	addr pos, aster;

	if (var == Nil)
		return Result(ret, Nil);
	make_tablevalue_stack(ptr, &pos, stack, var, 0);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	Return(apply_declare_value_stack_(ptr, stack, var, decl));
	return push_tablevalue_global_(ptr, stack, var, ret);
}

static int leta_init(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, root, var, init;

	stack = str->stack;
	args = str->args;
	decl = str->decl;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(check_scope_variable_(var));
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		cons_heap(&var, var, init);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static int leta_checktype_(Execute ptr, struct let_struct *str)
{
	addr args, var, init;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int leta_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(leta_init(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(leta_checktype_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkvalue_(stack));
	let_allocate(str);

	return 0;
}

int scope_leta_call(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(leta_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/************************************************************
 *  scope_object.c
 ************************************************************/

/*
 *  memory
 */
static void optimize_eval_stack_update(OptimizeType *optimize, addr stack)
{
	int i;
	struct eval_stack *str;
	OptimizeType value;

	str = StructEvalStack(stack);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		/* already set */
		if (0 <= optimize[i])
			continue;
		/* stack is not set */
		value = str->optimize[i];
		if (value < 0)
			continue;
		optimize[i] = value;
	}
}

static int optimize_eval_stack_(Execute ptr, OptimizeType *optimize)
{
	int i;
	addr stack;

	/* initial value */
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		optimize[i] = -1;

	/* stack */
	Return(getstack_eval_(ptr, &stack));
	while (stack != Nil) {
		optimize_eval_stack_update(optimize, stack);
		GetEvalStackNext(stack, &stack);
	}

	/* global */
	Return(getglobal_eval_(ptr, &stack));
	optimize_eval_stack_update(optimize, stack);

	return 0;
}

int eval_scope_heap_(Execute ptr, addr *ret, size_t size)
{
	addr pos;
	struct scope_struct *a;

	Check(0xFF < sizeoft(struct scope_struct), "struct size error");
	Check(0xFF < 2UL + size, "size argument error");
	eval_heap(&pos, EVAL_TYPE_SCOPE,
			(byte)(2UL + size),
			(byte)sizeoft(struct scope_struct));
	a = StructEvalScope(pos);
	Return(optimize_eval_stack_(ptr, a->optimize));

	return Result(ret, pos);
}

int eval_scope_size_(Execute ptr, addr *ret, size_t size,
		EvalParse parse, addr type, addr value)
{
	addr pos;

	Return(eval_scope_heap_(ptr, &pos, size));
	SetEvalScopeType(pos, parse);
	SetEvalScopeThe(pos, type);
	SetEvalScopeValue(pos, value);

	return Result(ret, pos);
}

int make_eval_scope_(Execute ptr,
		addr *ret, EvalParse parse, addr type, addr value)
{
	return eval_scope_size_(ptr, ret, 0, parse, type, value);
}


/*
 *  eval-scope
 */
struct scope_struct *structevalscope(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return StructEvalScope_Low(pos);
}
EvalParse refevalscopetype(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeType_Low(pos);
}
void getevalscopetype(addr pos, EvalParse *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeType_Low(pos, ret);
}
void setevalscopetype(addr pos, EvalParse value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeType_Low(pos, value);
}
addr refevalscopethe(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeThe_Low(pos);
}
void getevalscopethe(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeThe_Low(pos, ret);
}
void setevalscopethe(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeThe_Low(pos, value);
}
addr refevalscopevalue(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeValue_Low(pos);
}
void getevalscopevalue(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue_Low(pos, ret);
}
void setevalscopevalue(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeValue_Low(pos, value);
}
addr refevalscopeindex(addr pos, size_t index)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeIndex_Low(pos, index);
}
void getevalscopeindex(addr pos, size_t index, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeIndex_Low(pos, index, ret);
}
void setevalscopeindex(addr pos, size_t index, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeIndex_Low(pos, index, value);
}


/*
 *  table
 */
eval_scope_calltype EvalScopeTable[EVAL_PARSE_SIZE];

int scope_eval(Execute ptr, addr *ret, addr eval)
{
	EvalParse type;
	eval_scope_calltype call;

	GetEvalParseType(eval, &type);
	call = EvalScopeTable[type];
	if (call == NULL)
		return fmte_("Invalid eval-parse type.", NULL);

	return (*call)(ptr, ret, eval);
}

int scope_allcons(Execute ptr, addr *retcons, addr *rettype, addr cons)
{
	addr root, expr;
	LocalHold hold;

	/* cons */
	hold = LocalHold_array(ptr, 1);
	for (root = expr = Nil; cons != Nil; ) {
		GetCons(cons, &expr, &cons);
		Return(scope_eval(ptr, &expr, expr));
		cons_heap(&root, expr, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(retcons, root);

	/* type */
	if (rettype) {
		if (expr == Nil)
			type_value_nil(rettype);
		else
			GetEvalScopeThe(expr, rettype);
	}

	return 0;
}

int localhold_scope_eval(LocalHold hold, Execute ptr, addr *ret, addr eval)
{
	Return(scope_eval(ptr, ret, eval));
	localhold_push(hold, *ret);
	return 0;
}

int localhold_scope_allcons(LocalHold hold,
		Execute ptr, addr *retcons, addr *rettype, addr cons)
{
	Return(scope_allcons(ptr, retcons, rettype, cons));
	if (rettype)
		localhold_pushva(hold, *retcons, *rettype, NULL);
	else
		localhold_push(hold, *retcons);

	return 0;
}


/*
 *  lexical
 */
static int scope_eval_lexical_object_(Execute ptr, addr stack, addr eval, addr *ret)
{
	addr type, pos;

	lambda_lexical_heap(stack, &pos);
	GetEvalScopeThe(eval, &type);
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_LEXICAL, type, eval));
	SetEvalScopeIndex(eval, 0, pos);

	return Result(ret, eval);
}

int scope_eval_lexical(Execute ptr, addr *ret, addr eval)
{
	addr stack;

	Return(newstack_lexical_(ptr, &stack));
	Return(scope_eval(ptr, &eval, eval));
	Return(scope_eval_lexical_object_(ptr, stack, eval, ret));

	return freestack_eval_(ptr, stack);
}


/************************************************************
 *  scope_table.c
 ************************************************************/

/* nil */
static int scope_nil(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_nil(&eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_NIL, eval, Nil);
}


/* t */
static int scope_t(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_t(&eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_T, eval, T);
}


/* clos */
static int scope_clos(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_clos_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_CLOS, type, eval);
}


/* integer */
static int scope_integer(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_integer(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_INTEGER, type, eval);
}


/* rational */
static int scope_rational(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_rational(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_RATIONAL, type, eval);
}


/* complex */
static int scope_complex(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_complex_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_COMPLEX, type, eval);
}


/* character */
static int scope_character(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_character(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_CHARACTER, type, eval);
}


/* array */
static int scope_array(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_array_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_ARRAY, type, eval);
}


/* vector */
static int scope_vector(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_vector(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_VECTOR, type, eval);
}


/* bitvector */
static int scope_bitvector(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_bitvector(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_BITVECTOR, type, eval);
}


/* string */
static int scope_string(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_STRING, type, eval);
}


/* symbol */
static int scope_symbol(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	return scope_symbol_call(ptr, ret, eval);
}


/* float */
static int scope_float(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_float(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_FLOAT, type, eval);
}


/* declaim */
static int scope_declaim(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(apply_declaim_stack_(ptr, eval));
	type_value_nil(&type);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_DECLAIM, type, eval);
}


/* package */
static int scope_package(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_package(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PACKAGE, type, eval);
}


/* random-state */
static int scope_random_state(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_random_state(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_RANDOM_STATE, type, eval);
}


/* pathname */
static int scope_pathname(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_pathname(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PATHNAME, type, eval);
}


/* environment */
static int scope_environment(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_environment(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_ENVIRONMENT, type, eval);
}


/* progn */
int scope_progn(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(scope_allcons(ptr, &eval, &type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PROGN, type, eval);
}


/* let */
static int scope_let(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_let_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LET, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	SetEvalScopeIndex(eval, 4, str.allocate);

	return Result(ret, eval);
}


/* let* */
static int scope_leta(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_leta_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LETA, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	SetEvalScopeIndex(eval, 4, str.allocate);

	return Result(ret, eval);
}


/* setq */
static int scope_setq(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &cons);
	Return(scope_setq_call(ptr, cons, &cons, &type));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_SETQ, type, cons);
}


/* function */
static int scope_function(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	return scope_function_call_(ptr, ret, eval);
}


/* lambda */
static int scope_lambda(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	return scope_lambda_call_(ptr, ret, eval);
}


/* defun */
static int scope_defun(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFUN, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	GetEvalParse(eval, 5, &str.defun);
	return scope_defun_call_(ptr, &str, ret);
}


/* macro-lambda */
static int scope_macro_lambda(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	GetEvalParse(eval, 4, &str.call);
	return scope_macro_lambda_call_(ptr, &str, ret);
}


/* defmacro */
static int scope_defmacro(Execute ptr, addr *ret, addr eval)
{
	addr symbol, lambda, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &lambda);

	Return(scope_eval(ptr, &lambda, lambda));
	GetTypeTable(&type, Symbol);
	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_DEFMACRO, type, Nil));
	SetEvalScopeIndex(eval, 0, symbol);
	SetEvalScopeIndex(eval, 1, lambda);
	return Result(ret, eval);
}


/* deftype */
static int scope_deftype(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFTYPE, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	return scope_deftype_call_(ptr, &str, ret);
}


/* define-compiler-macro */
static int scope_define_compiler_macro(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFINE_COMPILER_MACRO, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	return scope_define_compiler_macro_call_(ptr, &str, ret);
}


/* destructuring-bind */
static int scope_destructuring_bind(Execute ptr, addr *ret, addr eval)
{
	addr args, expr;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &args);

	hold = LocalHold_array(ptr, 1);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_set(hold, 0, expr);
	Return(scope_bind_call_(ptr, &eval, expr, args));
	localhold_end(hold);

	return Result(ret, eval);
}


/* quote */
static int scope_quote(Execute ptr, addr *ret, addr eval)
{
	addr value, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &value);
	Return(type_value_(&type, value));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_QUOTE, type, value);
}


/* flet */
static int scope_flet(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_flet_call_(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 4, EVAL_PARSE_FLET, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	return Result(ret, eval);
}


/* labels */
static int scope_labels(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_labels_call_(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 4, EVAL_PARSE_LABELS, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	return Result(ret, eval);
}


/* call */
static int scope_call(Execute ptr, addr *ret, addr eval)
{
	addr first, args;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &first);
	GetEvalParse(eval, 1, &args);
	return scope_call_call_(ptr, first, args, ret);
}


/* values */
static int scope_values(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(scope_values_call(ptr, eval, &cons, &type));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_VALUES, type, cons);
}


/* the */
static int scope_the(Execute ptr, addr *ret, addr eval)
{
	addr type, form;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &type);
	GetEvalParse(eval, 1, &form);
	return scope_the_call(ptr, type, form, ret);
}


/* locally */
int scope_locally(Execute ptr, addr *ret, addr eval)
{
	addr decl, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &decl);
	GetEvalParse(eval, 1, &cons);
	return scope_locally_call(ptr, decl, cons, ret);
}


/* if */
static int scope_if(Execute ptr, addr *ret, addr eval)
{
	addr expr, then, last, type1, type2, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &then);
	GetEvalParse(eval, 2, &last);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_eval(hold, ptr, &then, then));
	Return(localhold_scope_eval(hold, ptr, &last, last));
	localhold_end(hold);

	GetEvalScopeThe(then, &type1);
	GetEvalScopeThe(last, &type2);
	type2or_heap(type1, type2, &type);

	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_IF, type, eval));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, then);
	SetEvalScopeIndex(eval, 2, last);
	return Result(ret, eval);
}


/* unwind-protect */
static int scope_unwind_protect(Execute ptr, addr *ret, addr eval)
{
	addr protect, cleanup, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &protect);
	GetEvalParse(eval, 1, &cleanup);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &protect, protect));
	Return(localhold_scope_allcons(hold, ptr, &cleanup, NULL, cleanup));
	localhold_end(hold);
	GetEvalScopeThe(protect, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_UNWIND_PROTECT, type, eval));
	SetEvalScopeIndex(eval, 0, protect);
	SetEvalScopeIndex(eval, 1, cleanup);
	return Result(ret, eval);
}


/* tagbody */
static int scope_tagbody(Execute ptr, addr *ret, addr eval)
{
	addr tag, body, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &body);

	Return(scope_tagbody_call(ptr, tag, body, &tag, &body));
	GetTypeTable(&type, Null);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_TAGBODY, type, Nil));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, body);
	return Result(ret, eval);
}


/* go */
static int scope_go(Execute ptr, addr *ret, addr eval)
{
	addr tag, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	Return(scope_go_call_(ptr, &tag, tag));
	GetTypeTable(&type, Nil);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_GO, type, tag);
}


/* block */
static int scope_block(Execute ptr, addr *ret, addr eval)
{
	addr name, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &cons);

	Return(scope_block_call(ptr, name, cons, &name, &cons, &type));
	/* type -> (or block return-from1 return-from2 ...) */
	GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_BLOCK, type, Nil));
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* return-from */
static int scope_return_from(Execute ptr, addr *ret, addr eval)
{
	addr name, form, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &form);

	Return(scope_return_from_call(ptr, name, form, &name, &form));
	GetTypeTable(&type, Nil);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_RETURN_FROM, type, Nil));
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, form);
	return Result(ret, eval);
}


/* catch */
static int scope_catch(Execute ptr, addr *ret, addr eval)
{
	addr tag, cons, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &tag, tag));
	Return(localhold_scope_allcons(hold, ptr, &cons, &type, cons));
	localhold_end(hold);
	GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_CATCH, type, Nil));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* throw */
static int scope_throw(Execute ptr, addr *ret, addr eval)
{
	addr tag, form, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &form);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &tag, tag));
	Return(localhold_scope_eval(hold, ptr, &form, form));
	localhold_end(hold);
	GetTypeTable(&type, Nil);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_THROW, type, eval));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, form);
	return Result(ret, eval);
}


/* eval-when */
int scope_eval_when(Execute ptr, addr *ret, addr eval)
{
	addr progn, type, compile, load, exec, toplevel, mode;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &progn);
	GetEvalParse(eval, 1, &compile);
	GetEvalParse(eval, 2, &load);
	GetEvalParse(eval, 3, &exec);
	GetEvalParse(eval, 4, &toplevel);
	GetEvalParse(eval, 5, &mode);

	Return(scope_allcons(ptr, &progn, &type, progn));

	Return(eval_scope_size_(ptr, &eval, 6, EVAL_PARSE_EVAL_WHEN, type, Nil));
	SetEvalScopeIndex(eval, 0, progn);
	SetEvalScopeIndex(eval, 1, compile);
	SetEvalScopeIndex(eval, 2, load);
	SetEvalScopeIndex(eval, 3, exec);
	SetEvalScopeIndex(eval, 4, toplevel);
	SetEvalScopeIndex(eval, 5, mode);
	return Result(ret, eval);
}

/* multiple-value-bind */
static int scope_multiple_value_bind(Execute ptr, addr *ret, addr eval)
{
	struct mvbind_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_mvbind(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.expr);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);

	Return(scope_multiple_value_bind_call(ptr, &str));

	Return(eval_scope_size_(ptr, &eval, 7, EVAL_PARSE_MULTIPLE_VALUE_BIND, str.the, Nil));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.expr);
	SetEvalScopeIndex(eval, 2, str.decl);
	SetEvalScopeIndex(eval, 3, str.doc);
	SetEvalScopeIndex(eval, 4, str.cons);
	SetEvalScopeIndex(eval, 5, str.free);
	SetEvalScopeIndex(eval, 6, str.allocate);
	return Result(ret, eval);
}


/* multiple-value-call */
static int scope_multiple_value_call(Execute ptr, addr *ret, addr eval)
{
	addr expr, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &cons);
	return scope_multiple_value_call_call(ptr, expr, cons, ret);
}


/* multiple-value-prog1 */
static int scope_multiple_value_prog1(Execute ptr, addr *ret, addr eval)
{
	addr expr, cons, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_allcons(hold, ptr, &cons, NULL, cons));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_MULTIPLE_VALUE_PROG1, type, eval));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* nth-value */
static int scope_nth_value(Execute ptr, addr *ret, addr eval)
{
	addr nth, expr, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &nth);
	GetEvalParse(eval, 1, &expr);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &nth, nth));
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_NTH_VALUE, type, eval));
	SetEvalScopeIndex(eval, 0, nth);
	SetEvalScopeIndex(eval, 1, expr);
	return Result(ret, eval);
}


/* progv */
static int scope_progv(Execute ptr, addr *ret, addr eval)
{
	addr symbols, values, body, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbols);
	GetEvalParse(eval, 1, &values);
	GetEvalParse(eval, 2, &body);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &symbols, symbols));
	Return(localhold_scope_eval(hold, ptr, &values, values));
	Return(localhold_scope_allcons(hold, ptr, &body, &type, body));
	localhold_end(hold);

	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_PROGV, type, eval));
	SetEvalScopeIndex(eval, 0, symbols);
	SetEvalScopeIndex(eval, 1, values);
	SetEvalScopeIndex(eval, 2, body);
	return Result(ret, eval);
}


/*
 *  initialize
 */
void init_scope_function(void)
{
	EvalScopeTable[EVAL_PARSE_NIL] = scope_nil;
	EvalScopeTable[EVAL_PARSE_T] = scope_t;
	EvalScopeTable[EVAL_PARSE_CLOS] = scope_clos;
	EvalScopeTable[EVAL_PARSE_INTEGER] = scope_integer;
	EvalScopeTable[EVAL_PARSE_RATIONAL] = scope_rational;
	EvalScopeTable[EVAL_PARSE_COMPLEX] = scope_complex;
	EvalScopeTable[EVAL_PARSE_CHARACTER] = scope_character;
	EvalScopeTable[EVAL_PARSE_ARRAY] = scope_array;
	EvalScopeTable[EVAL_PARSE_VECTOR] = scope_vector;
	EvalScopeTable[EVAL_PARSE_BITVECTOR] = scope_bitvector;
	EvalScopeTable[EVAL_PARSE_STRING] = scope_string;
	EvalScopeTable[EVAL_PARSE_SYMBOL] = scope_symbol;
	EvalScopeTable[EVAL_PARSE_FLOAT] = scope_float;
	EvalScopeTable[EVAL_PARSE_DECLAIM] = scope_declaim;
	EvalScopeTable[EVAL_PARSE_PACKAGE] = scope_package;
	EvalScopeTable[EVAL_PARSE_RANDOM_STATE] = scope_random_state;
	EvalScopeTable[EVAL_PARSE_PATHNAME] = scope_pathname;
	EvalScopeTable[EVAL_PARSE_ENVIRONMENT] = scope_environment;
	EvalScopeTable[EVAL_PARSE_PROGN] = scope_progn;
	EvalScopeTable[EVAL_PARSE_LET] = scope_let;
	EvalScopeTable[EVAL_PARSE_LETA] = scope_leta;
	EvalScopeTable[EVAL_PARSE_SETQ] = scope_setq;
	EvalScopeTable[EVAL_PARSE_DEFUN] = scope_defun;
	EvalScopeTable[EVAL_PARSE_DEFMACRO] = scope_defmacro;
	EvalScopeTable[EVAL_PARSE_MACRO_LAMBDA] = scope_macro_lambda;
	EvalScopeTable[EVAL_PARSE_DEFTYPE] = scope_deftype;
	EvalScopeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = scope_define_compiler_macro;
	EvalScopeTable[EVAL_PARSE_DESTRUCTURING_BIND] = scope_destructuring_bind;
	EvalScopeTable[EVAL_PARSE_QUOTE] = scope_quote;
	EvalScopeTable[EVAL_PARSE_FUNCTION] = scope_function;
	EvalScopeTable[EVAL_PARSE_LAMBDA] = scope_lambda;
	EvalScopeTable[EVAL_PARSE_IF] = scope_if;
	EvalScopeTable[EVAL_PARSE_UNWIND_PROTECT] = scope_unwind_protect;
	EvalScopeTable[EVAL_PARSE_TAGBODY] = scope_tagbody;
	EvalScopeTable[EVAL_PARSE_GO] = scope_go;
	EvalScopeTable[EVAL_PARSE_BLOCK] = scope_block;
	EvalScopeTable[EVAL_PARSE_RETURN_FROM] = scope_return_from;
	EvalScopeTable[EVAL_PARSE_CATCH] = scope_catch;
	EvalScopeTable[EVAL_PARSE_THROW] = scope_throw;
	EvalScopeTable[EVAL_PARSE_FLET] = scope_flet;
	EvalScopeTable[EVAL_PARSE_LABELS] = scope_labels;
	EvalScopeTable[EVAL_PARSE_THE] = scope_the;
	EvalScopeTable[EVAL_PARSE_EVAL_WHEN] = scope_eval_when;
	EvalScopeTable[EVAL_PARSE_VALUES] = scope_values;
	EvalScopeTable[EVAL_PARSE_LOCALLY] = scope_locally;
	EvalScopeTable[EVAL_PARSE_CALL] = scope_call;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = scope_multiple_value_bind;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = scope_multiple_value_call;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = scope_multiple_value_prog1;
	EvalScopeTable[EVAL_PARSE_NTH_VALUE] = scope_nth_value;
	EvalScopeTable[EVAL_PARSE_PROGV] = scope_progv;
	EvalScopeTable[EVAL_PARSE_LOAD_TIME_VALUE] = scope_load_time_value_;
	EvalScopeTable[EVAL_PARSE_STEP] = scope_step;
}


/************************************************************
 *  sequence.c
 ************************************************************/

int sequencep(addr pos)
{
	enum LISPTYPE check;

	check = GetType(pos);
	if (check == LISPTYPE_ARRAY)
		return array_vector_p(pos);
	return
		check == LISPTYPE_NIL ||
		check == LISPTYPE_CONS ||
		check == LISPTYPE_VECTOR ||
		check == LISPTYPE_STRING ||
		check == LISPTYPE_BITVECTOR;
}

int listp_sequence_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return Result(ret, 1);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

int vectorp_sequence_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return Result(ret, 0);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 1);

		case LISPTYPE_ARRAY:
			return Result(ret, array_vector_p(pos));

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

int vectorp_sequence_debug(addr pos)
{
	int check;
	check = 0;
	Error(vectorp_sequence_(pos, &check));
	return check;
}


/*
 *  size-check
 */
static int vector_error_sequence_(addr type, addr arg, size_t size)
{
	size_t check;

	if (type_asterisk_p(arg))
		return 0;
	if (! integerp(arg)) {
		return call_type_error_va_(NULL,
				Nil, Nil, "Invalid type-specifier ~S.", type, NULL);
	}
	if (GetIndex_integer(arg, &check))
		return fmte_("Index size ~S is too large.", arg, NULL);
	if (check != size) {
		return call_type_error_va_(NULL, Nil, Nil,
				"The argument size ~S don't match type-spec ~S.",
				intsizeh(size), type, NULL);
	}

	return 0;
}

int vector_check_sequence_(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 1, &arg);
	return vector_error_sequence_(type, arg, size);
}

int simple_vector_check_sequence_(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 0, &arg);
	return vector_error_sequence_(type, arg, size);
}

int array_check_sequence_(addr type, size_t size)
{
	addr arg;
	size_t check;

	GetArrayType(type, 1, &arg);

	/* asterisk */
	if (type_asterisk_p(arg)) {
		return 0;
	}

	/* integer */
	if (integerp(arg)) {
		if (GetIndex_integer(arg, &check))
			return fmte_("Index size ~S is too large.", arg, NULL);
		if (check != 1) {
			return call_type_error_va_(NULL, Nil, Nil,
					"Array ~S dimension must be 1.", type, NULL);
		}
		return 0;
	}

	/* multiple dimension */
	if (GetType(arg) == LISPTYPE_VECTOR) {
		LenArrayA4(arg, &check);
		if (check != 1) {
			return call_type_error_va_(NULL, Nil, Nil,
					"Array ~S dimension must be 1.", type, NULL);
		}
		GetArrayA4(arg, 0, &arg);
		if (GetIndex_integer(arg, &check))
			return fmte_("Index size ~S is too large.", arg, NULL);
		if (check != size) {
			return call_type_error_va_(NULL, Nil, Nil,
					"The argument size ~S don't match type-spec ~S.",
					intsizeh(size), type, NULL);
		}
		return 0;
	}

	/* error */
	return fmte_("Invalid array-type ~S.", type, NULL);
}


/*
 *  make-vector-from-list
 */
int make_vector_from_list_(addr *ret, addr cons)
{
	addr pos, array;
	size_t i, size;

	/* length */
	pos = cons;
	for (size = 0; pos != Nil; size++) {
		if (GetType(pos) != LISPTYPE_CONS) {
			*ret = Nil;
			return fmte_("The tail of list must be a Nil.", NULL);
		}
		GetCdr(pos, &pos);
	}

	/* make vector */
	vector_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		setarray(array, i, pos);
	}

	return Result(ret, array);
}

int make_vector4_from_list_(addr *ret, addr cons)
{
	addr pos, array;
	size_t i, size;

	/* length */
	pos = cons;
	for (size = 0; pos != Nil; size++) {
		if (GetType(pos) != LISPTYPE_CONS) {
			*ret = Nil;
			return fmte_("The tail of list must be a Nil.", NULL);
		}
		GetCdr(pos, &pos);
	}

	/* make vector */
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}

	return Result(ret, array);
}


/*
 *  start-end
 */
int list_start_end_sequence_(addr *list, addr *prev,
		addr start, addr end, size_t *ret1, size_t *ret2)
{
	addr temp;
	size_t index1, index2, i, size;

	/* argument */
	if (GetIndex_integer(start, &index1)) {
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A is too large.", start, NULL);
	}
	if (end != Nil && end != Unbound) {
		if (GetIndex_integer(end, &index2)) {
			if (prev)
				*prev = Nil;
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A is too large.", end, NULL);
		}
		if (index2 < index1) {
			if (prev)
				*prev = Nil;
			*ret1 = *ret2 = 0;
			return fmte_(":START ~A "
					"must be less than equal to :END ~A.", start, end, NULL);
		}
		length_list_p(*list, &size);
		if (size < index2) {
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A must be less than sequence length.", end, NULL);
		}
	}
	else {
		index2 = 0;
	}

	/* start */
	temp = Nil;
	for (i = 0; i < index1; i++) {
		if (*list == Nil) {
			if (prev)
				*prev = Nil;
			*ret1 = *ret2 = 0;
			return fmte_(":START ~A "
					"must be less than equal to list length.", start, NULL);
		}
		temp = *list;
		Return_getcdr(*list, list);
	}

	if (prev)
		*prev = temp;
	*ret1 = index1;
	*ret2 = index2;
	return 0;
}

static int size_start_end_sequence_call_(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2, int *ret)
{
	size_t index1, index2;

	if (GetIndex_integer(start, &index1)) {
		*ret = 0;
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A is too large.", start, NULL);
	}
	if (end != Nil && end != Unbound) {
		if (GetIndex_integer(end, &index2)) {
			*ret = 0;
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A is too large.", end, NULL);
		}
		if (size < index2) {
			*ret = 0;
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A must be less than sequence length.", end, NULL);
		}
	}
	else {
		index2 = size;
	}
	if (size < index1) {
		*ret = 0;
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A must be less than sequence length.", start, NULL);
	}
	if (index2 < index1) {
		*ret = 0;
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A must be less than equal to :END ~A.", start, end, NULL);
	}

	*ret1 = index1;
	*ret2 = index2;
	return Result(ret, 0);
}

int size_start_end_sequence_(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2, int *ret)
{
	int ignore;

	if (ret)
		return size_start_end_sequence_call_(start, end, size, ret1, ret2, ret);
	else
		return size_start_end_sequence_call_(start, end, size, ret1, ret2, &ignore);
}


/*
 *  common
 */
int length_sequence_(addr pos, int fill, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return Result(ret, 0);

		case LISPTYPE_CONS:
			return length_list_safe_(pos, ret);

		case LISPTYPE_VECTOR:
			*ret = lenarrayr(pos);
			return 0;

		case LISPTYPE_STRING:
			strvect_length(pos, ret);
			return 0;

		case LISPTYPE_ARRAY:
			return array_get_vector_length_(pos, fill, ret);

		case LISPTYPE_BITVECTOR:
			return bitvector_length_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}


/*
 *  elt
 */
static int getelt_list_(addr pos, size_t index, addr *ret)
{
	for (;;) {
		if (pos == Nil) {
			*ret = Nil;
			return fmte_("Index ~S is too large.", intsizeh(index), NULL);
		}
		if (! consp(pos)) {
			*ret = Nil;
			return fmte_("The list ~S must be a list type.", pos, NULL);
		}
		if (index == 0)
			break;
		GetCdr(pos, &pos);
		index--;
	}
	GetCar(pos, ret);
	return 0;
}

static int getelt_vector_(addr pos, size_t index, addr *ret)
{
	if (lenarrayr(pos) <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}
	getarray(pos, index, ret);
	return 0;
}

static int getelt_string_(addr pos, size_t index, unicode *ret)
{
	size_t size;

	strvect_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}
	strvect_getc(pos, index, ret);
	return 0;
}

static int getelt_bitvector_(addr pos, size_t index, int *ret)
{
	size_t size;

	Return(bitvector_length_(pos, &size));
	if (size <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return bitmemory_getint_(pos, index, ret);
}

int getelt_inplace_sequence_(addr pos, size_t index, struct array_value *str)
{
	int bit;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			Return(getelt_list_(pos, index, &(str->value.object)));
			str->type = ARRAY_TYPE_T;
			break;

		case LISPTYPE_VECTOR:
			Return(getelt_vector_(pos, index, &(str->value.object)));
			str->type = ARRAY_TYPE_T;
			break;

		case LISPTYPE_STRING:
			Return(getelt_string_(pos, index, &(str->value.character)));
			str->type = ARRAY_TYPE_CHARACTER;
			break;

		case LISPTYPE_ARRAY:
			return arrayinplace_get_(pos, index, str);

		case LISPTYPE_BITVECTOR:
			Return(getelt_bitvector_(pos, index, &bit));
			str->value.bit = bit? 1: 0;
			str->type = ARRAY_TYPE_BIT;
			break;

		default:
			return TypeError_(pos, SEQUENCE);
	}

	return 0;
}

static int setelt_bit_t_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	addr value;
	fixnum check;

	value = str->value.object;
	GetFixnum(value, &check);
	if (check == 0)
		return bitmemory_setint_(pos, index, 0);
	if (check == 1)
		return bitmemory_setint_(pos, index, 1);

	/* error */
	return fmte_("The bit-vector cannot set an integer ~A.", value, NULL);
}

static int setelt_bit_signed_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	int check;
	int8_t v8;
	int16_t v16;
	int32_t v32;
#ifdef LISP_64BIT
	int64_t v64;
#endif

	/* signed check */
	switch (str->size) {
		case 8:
			v8 = str->value.signed8;
			if (v8 == 0) check = 0;
			else if (v8 == 1) check = 1;
			else check = -1;
			break;

		case 16:
			v16 = str->value.signed16;
			if (v16 == 0) check = 0;
			else if (v16 == 1) check = 1;
			else check = -1;
			break;

		case 32:
			v32 = str->value.signed32;
			if (v32 == 0) check = 0;
			else if (v32 == 1) check = 1;
			else check = -1;
			break;

#ifdef LISP_64BIT
		case 64:
			v64 = str->value.signed64;
			if (v64 == 0) check = 0;
			else if (v64 == 1) check = 1;
			else check = -1;
			break;
#endif

		default:
			return fmte_("Invalid array value.", NULL);
	}

	/* result */
	if (check == 0)
		return bitmemory_setint_(pos, index, 0);
	if (check == 1)
		return bitmemory_setint_(pos, index, 1);

	/* error */
	Return(arrayvalue_heap_(&pos, str));
	return fmte_("The bit-vector cannot set an integer ~A.", pos, NULL);
}

static int setelt_bit_unsigned_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	int check;
	uint8_t v8;
	uint16_t v16;
	uint32_t v32;
#ifdef LISP_64BIT
	uint64_t v64;
#endif

	/* unsigned check */
	switch (str->size) {
		case 8:
			v8 = str->value.unsigned8;
			if (v8 == 0) check = 0;
			else if (v8 == 1) check = 1;
			else check = -1;
			break;

		case 16:
			v16 = str->value.unsigned16;
			if (v16 == 0) check = 0;
			else if (v16 == 1) check = 1;
			else check = -1;
			break;

		case 32:
			v32 = str->value.unsigned32;
			if (v32 == 0) check = 0;
			else if (v32 == 1) check = 1;
			else check = -1;
			break;

#ifdef LISP_64BIT
		case 64:
			v64 = str->value.unsigned64;
			if (v64 == 0) check = 0;
			else if (v64 == 1) check = 1;
			else check = -1;
			break;
#endif

		default:
			return fmte_("Invalid array value.", NULL);
	}

	/* result */
	if (check == 0)
		return bitmemory_setint_(pos, index, 0);
	if (check == 1)
		return bitmemory_setint_(pos, index, 1);

	/* error */
	Return(arrayvalue_heap_(&pos, str));
	return fmte_("The bit-vector cannot set an integer ~A.", pos, NULL);
}

static int setelt_bit_sequence_(addr pos, size_t index, const struct array_value *str)
{
	switch (str->type) {
		case ARRAY_TYPE_T:
			return setelt_bit_t_sequence_(pos, index, str);

		case ARRAY_TYPE_BIT:
			return bitmemory_setint_(pos, index, (int)str->value.bit);

		case ARRAY_TYPE_SIGNED:
			return setelt_bit_signed_sequence_(pos, index, str);

		case ARRAY_TYPE_UNSIGNED:
			return setelt_bit_unsigned_sequence_(pos, index, str);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int setelt_string_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	addr value;
	unicode c;

	/* t */
	if (str->type == ARRAY_TYPE_T) {
		value = str->value.object;
		if (! characterp(value))
			return fmte_("The object ~S must be a character type,", value, NULL);
		GetCharacter(value, &c);
		return strvect_setc_(pos, index, c);
	}

	/* character */
	if (str->type == ARRAY_TYPE_CHARACTER)
		return strvect_setc_(pos, index, str->value.character);

	/* others */
	return fmte_("The element of sequence  ~S must be a character type.", pos, NULL);
}

int setelt_inplace_sequence_(LocalRoot local,
		addr pos, size_t index, const struct array_value *str)
{
	addr value;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			Return(arrayvalue_alloc_(local, &value, str));
			return setnth_(pos, index, value);

		case LISPTYPE_VECTOR:
			Return(arrayvalue_alloc_(local, &value, str));
			Return(vector_setelt_(pos, index, value));
			return 0;

		case LISPTYPE_STRING:
			return setelt_string_sequence_(pos, index, str);

		case LISPTYPE_ARRAY:
			return arrayinplace_set_(pos, index, str);

		case LISPTYPE_BITVECTOR:
			return setelt_bit_sequence_(pos, index, str);

		default:
			return TypeError_(pos, SEQUENCE);
	}
}

static int getelt_string_alloc_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;

	Return(getelt_string_(pos, index, &c));
	character_alloc(local, ret, c);

	return 0;
}

static int getelt_array_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	size_t size;

	if (! array_vector_p(pos)) {
		*ret = Nil;
		return TypeError_(pos, SEQUENCE);
	}
	Return(array_get_vector_length_(pos, 1, &size));
	if (size <= index) {
		*ret = Nil;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return array_get_(local, pos, index, ret);
}

static int getelt_bitvector_alloc_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	size_t size;

	Return(bitvector_length_(pos, &size));
	if (size <= index) {
		*ret = Nil;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return bitmemory_get_(local, pos, index, ret);
}

int getelt_sequence_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return getelt_list_(pos, index, ret);

		case LISPTYPE_VECTOR:
			return getelt_vector_(pos, index, ret);

		case LISPTYPE_STRING:
			return getelt_string_alloc_(local, pos, index, ret);

		case LISPTYPE_ARRAY:
			return getelt_array_(local, pos, index, ret);

		case LISPTYPE_BITVECTOR:
			return getelt_bitvector_alloc_(local, pos, index, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, SEQUENCE);
	}
}

static int setelt_array_(addr pos, size_t index, addr value)
{
	size_t size;

	if (! array_vector_p(pos))
		return TypeError_(pos, SEQUENCE);
	Return(array_get_vector_length_(pos, 1, &size));
	if (size <= index)
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);

	return array_set_(pos, index, value);
}

int setelt_sequence_(addr pos, size_t index, addr value)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return setnth_(pos, index, value);

		case LISPTYPE_VECTOR:
			return vector_setelt_(pos, index, value);

		case LISPTYPE_STRING:
			return strvect_set_(pos, index, value);

		case LISPTYPE_ARRAY:
			return setelt_array_(pos, index, value);

		case LISPTYPE_BITVECTOR:
			return bitmemory_set_(pos, index, value);

		default:
			return TypeError_(pos, SEQUENCE);
	}
}


/*
 *  reverse / nreverse
 */
int reverse_sequence_heap_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return Result(ret, Nil);

		case LISPTYPE_CONS:
			return reverse_list_heap_safe_(ret, pos);

		case LISPTYPE_VECTOR:
			vector_reverse(NULL, ret, pos);
			return 0;

		case LISPTYPE_STRING:
			return strvect_reverse_(NULL, ret, pos);

		case LISPTYPE_ARRAY:
			return array_reverse_(ret, pos);

		case LISPTYPE_BITVECTOR:
			return bitmemory_reverse_(NULL, ret, pos);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

int nreverse_sequence_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return Result(ret, Nil);

		case LISPTYPE_CONS:
			return nreverse_list_safe_(ret, pos);

		case LISPTYPE_VECTOR:
			vector_nreverse(ret, pos);
			return 0;

		case LISPTYPE_STRING:
			return strvect_nreverse_(ret, pos);

		case LISPTYPE_ARRAY:
			return array_nreverse_(ret, pos);

		case LISPTYPE_BITVECTOR:
			return bitmemory_nreverse_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, SEQUENCE);
	}
}


/************************************************************
 *  sequence_iterator.c
 ************************************************************/

/*
 *  sequence-iterator
 */
int make_sequence_iterator_local_(LocalRoot local,
		addr pos, int fill, struct sequence_iterator **ret)
{
	struct sequence_iterator *ptr;

	Check(local == NULL, "local error");
	ptr = (struct sequence_iterator *)lowlevel_local(local,
			sizeoft(struct sequence_iterator));
	ptr->pos = pos;
	ptr->root = pos;
	if (listp(pos)) {
		ptr->listp = 1;
		ptr->size = 0;
	}
	else {
		ptr->listp = 0;
		Return(length_sequence_(pos, fill, &(ptr->size)));
	}
	ptr->index = 0;

	return Result(ret, ptr);
}

int end_sequence_iterator(struct sequence_iterator *ptr)
{
	if (ptr->listp)
		return ptr->pos == Nil;
	else
		return ptr->size <= ptr->index;
}

int length_sequence_iterator_(struct sequence_iterator *ptr, size_t *ret)
{
	if (ptr->listp)
		return length_list_safe_(ptr->root, ret);
	else
		return Result(ret, ptr->size);
}

int object_sequence_iterator_(struct sequence_iterator *iter, addr *value, int *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return Result(ret, 0);
		Return_getcons(iter->pos, value, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return Result(ret, 0);
		Return(getelt_sequence_(NULL, iter->pos, iter->index, value));
	}
	iter->index++;

	return Result(ret, 1);
}

static int next_sequence_iterator_(struct sequence_iterator *iter, addr *value, int *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return Result(ret, 0);
		Return_getcons(iter->pos, value, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return Result(ret, 0);
	}
	iter->index++;

	return Result(ret, 1);
}

int set_sequence_iterator_(struct sequence_iterator *iter, addr value, int *ret)
{
	if (iter->listp) {
		Return_setcar(iter->pos, value);
		Return_getcdr(iter->pos, &(iter->pos));
		iter->index++;
		return Result(ret, iter->pos == Nil);
	}
	else {
		Return(setelt_sequence_(iter->pos, iter->index, value));
		iter->index++;
		return Result(ret, iter->size <= iter->index);
	}
}


/*
 *  sequence-group
 */
int make_sequence_group_local_(
		LocalRoot local, addr rest, int fill, struct sequence_group **ret)
{
	struct sequence_group *ptr;
	struct sequence_iterator **data;
	addr pos;
	size_t size, i;

	Return(length_list_safe_(rest, &size));
	ptr = (struct sequence_group *)lowlevel_local(local,
			sizeoft(struct sequence_group));
	data  = (struct sequence_iterator **)lowlevel_local(local,
			sizeoft(struct sequence_iterator *) * size);

	for (i = 0; i < size; i++) {
		GetCons(rest, &pos, &rest);
		Return(make_sequence_iterator_local_(local, pos, fill, &(data[i])));
	}
	ptr->data = data;
	ptr->size = size;
	ptr->callsize = 0;
	ptr->list = NULL;

	return Result(ret, ptr);
}

void list_sequence_group_local(LocalRoot local,
		addr *ret, struct sequence_group *group)
{
	addr root;
	size_t size, i;

	size = group->size;
	root = Nil;
	for (i = 0; i < size; i++)
		conscdr_local(local, &root, root);
	group->list = root;
	if (ret)
		*ret = root;
}

int set_sequence_group_(struct sequence_group *group, addr list, int *ret)
{
	int check;
	struct sequence_iterator **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		Return(object_sequence_iterator_(data[i], &temp, &check));
		if (! check)
			return Result(ret, 0);
		Check(! consp(list), "list error");
		SetCar(list, temp);
		GetCdr(list, &list);
	}

	return Result(ret, 1);
}

void clear_sequence_group(struct sequence_group *group)
{
	struct sequence_iterator **data, *ptr;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		ptr = data[i];
		ptr->pos = ptr->root;
		ptr->index = 0;
	}
}

static int next_sequence_group_(struct sequence_group *group, int *ret)
{
	int check;
	struct sequence_iterator **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		Return(next_sequence_iterator_(data[i], &temp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int count_sequence_group_(struct sequence_group *group, size_t *ret)
{
	int check;
	size_t size;

	for (size = 0; ; size++) {
		Return(next_sequence_group_(group, &check));
		if (! check)
			break;
	}
	group->callsize = size;
	if (ret)
		*ret = size;

	return 0;
}


/************************************************************
 *  sequence_range.c
 ************************************************************/

/*
 *  save/load
 */
void save_sequence_range(struct sequence_range *ptr)
{
	ptr->save_pos = ptr->pos;
	ptr->save_list = ptr->list;
	ptr->save_prev = ptr->prev;
	ptr->save_size = ptr->size;
	ptr->save_end = ptr->end;
	ptr->save_index = ptr->index;
}

void load_sequence_range(struct sequence_range *ptr)
{
	ptr->pos = ptr->save_pos;
	ptr->list = ptr->save_list;
	ptr->prev = ptr->save_prev;
	ptr->size = ptr->save_size;
	ptr->end = ptr->save_end;
	ptr->index = ptr->save_index;
}


/*
 *  build
 */
int build_sequence_range_(struct sequence_range *ptr,
		addr pos, addr start, addr end)
{
	int listp;
	addr list, prev;
	size_t index1, index2, size;

	clearpoint(ptr);
	Return(listp_sequence_(pos, &listp));
	ptr->pos = list = pos;
	ptr->listp = listp;
	if (start == Nil || start == Unbound)
		start = fixnumh(0);
	if (end == Nil)
		end = Unbound;

	if (listp) {
		Return(list_start_end_sequence_(&list, &prev, start, end, &index1, &index2));
		ptr->list = list;
		ptr->prev = prev;
		ptr->start = index1;
		ptr->endp = (end != Unbound);
		if (ptr->endp) {
			ptr->end = index2;
			ptr->size = index2 - index1;
		}
		else {
			ptr->end = 0;
			ptr->size = 0;
		}
		ptr->index = index1;
	}
	else {
		Return(length_sequence_(pos, 1, &size));
		Return(size_start_end_sequence_(start, end, size, &index1, &index2, NULL));
		ptr->prev = Nil;
		ptr->start = index1;
		ptr->endp = 1;
		ptr->end = index2;
		ptr->size = index2 - index1;
		ptr->index = index1;
	}
	save_sequence_range(ptr);

	return 0;
}

static int build_sequence_range_force_vector_(struct sequence_range *ptr,
		addr pos, addr start, addr end)
{
	size_t index1, index2, size;

	clearpoint(ptr);
	ptr->pos = pos;
	ptr->listp = 0;
	if (start == Nil || start == Unbound)
		start = fixnumh(0);
	if (end == Nil)
		end = Unbound;

	Return(length_sequence_(pos, 1, &size));
	Return(size_start_end_sequence_(start, end, size, &index1, &index2, NULL));
	ptr->prev = Nil;
	ptr->start = index1;
	ptr->endp = 1;
	ptr->end = index2;
	ptr->size = index2 - index1;
	ptr->index = index1;
	save_sequence_range(ptr);

	return 0;
}

static struct sequence_range *sequence_range_local(LocalRoot local)
{
	return (struct sequence_range *)lowlevel_local(local,
			sizeoft(struct sequence_range));
}

int make_sequence_range_(LocalRoot local,
		addr pos, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_(ptr, pos, start, end));

	return Result(ret, ptr);
}

static int start_end_sequence_range_(
		addr list, size_t index1, size_t index2, addr end, size_t *ret)
{
	size_t size;

	for (size = 0; ; size++, index1++) {
		if (end != Nil && end != Unbound) {
			if (index2 <= index1)
				break;
			if (list == Nil) {
				*ret = 0;
				return fmte_(":END ~A "
						"must be less than equal to list length.", end, NULL);
			}
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list)) {
			*ret = 0;
			return fmte_("Don't accept the dotted list ~S.", list, NULL);
		}
		GetCdr(list, &list);
	}

	return Result(ret, size);
}

int build_sequence_range_endp_(struct sequence_range *ptr,
		addr list, addr start, addr end)
{
	Return(build_sequence_range_(ptr, list, start, end));
	if (! ptr->endp) {
		Return(length_list_safe_(ptr->list, &(ptr->size)));
		ptr->end = ptr->start + ptr->size;
		ptr->endp = 1;
	}

	return 0;
}

int make_sequence_range_endp_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_endp_(ptr, list, start, end));

	return Result(ret, ptr);
}

int build_sequence_range_vector2_(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end,
		addr *root, addr *tail)
{
	int check;
	addr pos, value;
	size_t index1, index2, size, i;

	/* vector */
	Return(listp_sequence_(list, &check));
	if (! check)
		return build_sequence_range_(ptr, list, start, end);

	/* list */
	Return(list_start_end_sequence_(&list, NULL, start, end, &index1, &index2));
	Return(start_end_sequence_range_(list, index1, index2, end, &size));
	if (root)
		*root = list;
	vector_local(local, &pos, size);
	for (i = 0; i < size; i++) {
		GetCons(list, &value, &list);
		setarray(pos, i, value);
	}
	if (tail)
		*tail = list;

	return build_sequence_range_(ptr, pos, fixnumh(0), Nil);
}

int build_sequence_range_vector_(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end)
{
	return build_sequence_range_vector2_(local, ptr, list, start, end, NULL, NULL);
}

int make_sequence_range_vector_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_vector_(local, ptr, list, start, end));

	return Result(ret, ptr);
}

int make_sequence_range_mismatch_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_force_vector_(ptr, list, start, end));

	return Result(ret, ptr);
}

static int getlist_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	if (! ptr->endp) {
		if (ptr->list == Nil)
			return Result(ret, 1);
		else
			goto normal;
	}
	if (ptr->index < ptr->end) {
		if (ptr->list == Nil)
			goto error;
		else
			goto normal;
	}
	return Result(ret, 1);

normal:
	Return_getcar(ptr->list, value);
	return Result(ret, 0);

error:
	*ret = 0;
	return fmte_(":END ~A must be less than equal to list length.",
			intsizeh(ptr->end), NULL);
}


/*
 *  access
 */
int get_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	/* list */
	if (ptr->listp)
		return getlist_sequence_range_(ptr, value, ret);

	/* vector */
	if (ptr->end <= ptr->index)
		return Result(ret, 1);
	Return(getelt_sequence_(NULL, ptr->pos, ptr->index, value));

	return Result(ret, 0);
}

int getnext_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	int check;

	if (ptr->listp) {
		Return(getlist_sequence_range_(ptr, value, &check));
		if (! check) {
			ptr->prev = ptr->list;
			Return_getcons(ptr->list, value, &(ptr->list));
			ptr->index++;
		}
		return Result(ret, check);
	}
	else {
		if (ptr->end <= ptr->index)
			return Result(ret, 1);
		Return(getelt_sequence_(NULL, ptr->pos, ptr->index++, value));

		return Result(ret, 0);
	}
}

int next_sequence_range_(struct sequence_range *ptr, int *ret)
{
	addr temp;
	return getnext_sequence_range_(ptr, &temp, ret);
}

int endp_sequence_range(struct sequence_range *ptr)
{
	if (ptr->endp)
		return ptr->end <= ptr->index;
	else
		return ptr->list == Nil;
}

int set_sequence_range_(struct sequence_range *ptr, addr value)
{
	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		SetCar(ptr->list, value);
		return 0;
	}
	else {
		return setelt_sequence_(ptr->pos, ptr->index, value);
	}
}

int getinplace_sequence_range_(struct sequence_range *ptr, struct array_value *ret)
{
	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		ret->type = ARRAY_TYPE_T;
		GetCar(ptr->list, &(ret->value.object));
		return 0;
	}
	else {
		return getelt_inplace_sequence_(ptr->pos, ptr->index, ret);
	}
}

int setinplace_sequence_range_(LocalRoot local,
		struct sequence_range *ptr, const struct array_value *str)
{
	addr value;

	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		Return(arrayvalue_alloc_(local, &value, str));
		SetCar(ptr->list, value);
		return 0;
	}
	else {
		return setelt_inplace_sequence_(local, ptr->pos, ptr->index, str);
	}
}


/*
 *  reverse
 */
void reverse_sequence_range(struct sequence_range *ptr)
{
	Check(ptr->listp, "type error");
	ptr->index = ptr->end;
}

int endp_reverse_sequence_range(struct sequence_range *ptr)
{
	Check(ptr->listp, "type error");
	return ptr->index <= ptr->start;
}

int next_reverse_sequence_range_(struct sequence_range *ptr, int *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr))
		return Result(ret, 1);
	ptr->index--;

	return Result(ret, 0);
}

int get_reverse_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr))
		return Result(ret, 1);
	Return(getelt_sequence_(NULL, ptr->pos, ptr->index - 1, value));

	return Result(ret, 0);
}

int getnext_reverse_sequence_range_(
		struct sequence_range *ptr, addr *value, int *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr))
		return Result(ret, 1);
	ptr->index--;
	Return(getelt_sequence_(NULL, ptr->pos, ptr->index, value));

	return Result(ret, 0);
}

int set_reverse_sequence_range_(struct sequence_range *ptr, addr value)
{
	Check(endp_reverse_sequence_range(ptr), "endp error");
	Check(ptr->listp, "type error");
	return setelt_sequence_(ptr->pos, ptr->index - 1, value);
}


/*
 *  remove
 */
int remove_sequence_range_(struct sequence_range *ptr)
{
	Check(! ptr->listp, "type error");
	Check(ptr->list == Nil, "list error");

	if (ptr->endp) {
		if (ptr->end <= ptr->index)
			return fmte_(":end size error", NULL);
		ptr->end--;
		ptr->size--;
	}
	if (ptr->prev == Nil) {
		Return_getcdr(ptr->list, &(ptr->list));
		ptr->pos = ptr->list;
	}
	else {
		Return_getcdr(ptr->list, &(ptr->list));
		Return_setcdr(ptr->prev, ptr->list);
	}

	return 0;
}


/************************************************************
 *  sequence_safe.c
 ************************************************************/

/* array */
static int get_unsigned8_t_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	return GetByte_integer(pos->value.object, ret);
}

static int get_unsigned8_bit_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	*ret = pos->value.bit;
	return 0;
}

static int get_unsigned8_signed_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_ARCH_64BIT
	int64_t s64;
#endif

	switch (pos->size) {
		case 8:
			s8 = pos->value.signed8;
			if (s8 < 0)
				return 1;
			*ret = (byte)s8;
			break;

		case 16:
			s16 = pos->value.signed16;
			if (! IsByteSign(s16))
				return 1;
			*ret = (byte)s16;
			break;

		case 32:
			s32 = pos->value.signed32;
			if (! IsByteSign(s32))
				return 1;
			*ret = (byte)s32;
			break;

#ifdef LISP_ARCH_64BIT
		case 64:
			s64 = pos->value.signed64;
			if (! IsByteSign(s64))
				return 1;
			*ret = (byte)s64;
			break;
#endif
		default:
			return 1;
	}

	return 0;
}

static int get_unsigned8_unsigned_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_ARCH_64BIT
	uint64_t u64;
#endif

	switch (pos->size) {
		case 8:
			*ret = pos->value.unsigned8;
			break;

		case 16:
			u16 = pos->value.unsigned16;
			if (! IsByteUnsign(u16))
				return 1;
			*ret = (byte)u16;
			break;

		case 32:
			u32 = pos->value.unsigned32;
			if (! IsByteUnsign(u32))
				return 1;
			*ret = (byte)u32;
			break;

#ifdef LISP_ARCH_64BIT
		case 64:
			u64 = pos->value.unsigned64;
			if (! IsByteUnsign(u64))
				return 1;
			*ret = (byte)u64;
			break;
#endif
		default:
			return 1;
	}

	return 0;
}

static int get_unsigned8_array(addr pos, size_t index, byte *ret)
{
	struct array_value value;

	if (arrayinplace_get_safe(pos, index, &value))
		goto error;
	switch (value.type) {
		case ARRAY_TYPE_T:
			return get_unsigned8_t_array(&value, index, ret);

		case ARRAY_TYPE_BIT:
			return get_unsigned8_bit_array(&value, index, ret);

		case ARRAY_TYPE_SIGNED:
			return get_unsigned8_signed_array(&value, index, ret);

		case ARRAY_TYPE_UNSIGNED:
			return get_unsigned8_unsigned_array(&value, index, ret);

		default:
			goto error;
	}
error:
	*ret = 0;
	return 1;
}

/* list */
static int getelt_list_safe(addr pos, size_t index, addr *ret)
{
	for (;;) {
		if (! consp(pos)) {
			*ret = Nil;
			return 1;
		}
		if (index == 0)
			break;
		GetCdr(pos, &pos);
		index--;
	}
	GetCar(pos, ret);
	return 0;
}

static int get_unsigned8_list(addr pos, size_t index, byte *ret)
{
	if (getelt_list_safe(pos, index, &pos))
		goto error;
	if (GetByte_integer(pos, ret))
		goto error;
	return 0;

error:
	*ret = 0;
	return 1;
}

/* vector */
static int getelt_vector_safe(addr pos, size_t index, addr *ret)
{
	if (lenarrayr(pos) <= index) {
		*ret = 0;
		return 1;
	}
	getarray(pos, index, ret);
	return 0;
}

static int get_unsigned8_vector(addr pos, size_t index, byte *ret)
{
	if (getelt_vector_safe(pos, index, &pos))
		goto error;
	if (GetByte_integer(pos, ret))
		goto error;
	return 0;

error:
	*ret = 0;
	return 1;
}

/* bit-vector */
static int getelt_bitvector_safe(addr pos, size_t index, int *ret)
{
	size_t size;

	Return(bitvector_length_(pos, &size));
	if (size <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return bitmemory_getint_(pos, index, ret);
}

static int get_unsigned8_bitvector(addr pos, size_t index, byte *ret)
{
	int bit;

	if (getelt_bitvector_safe(pos, index, &bit)) {
		*ret = 0;
		return 1;
	}
	*ret = (byte)bit;
	return 0;
}

int get_unsigned8_sequence(addr pos, size_t index, byte *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return get_unsigned8_list(pos, index, ret);

		case LISPTYPE_VECTOR:
			return get_unsigned8_vector(pos, index, ret);

		case LISPTYPE_BITVECTOR:
			return get_unsigned8_bitvector(pos, index, ret);

		case LISPTYPE_ARRAY:
			return get_unsigned8_array(pos, index, ret);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}


/************************************************************
 *  sequence_write.c
 ************************************************************/

/*
 *  sequence write
 */
void build_sequence_write_list(struct sequence_write *ptr)
{
	ptr->listp = 1;
	ptr->reverse = 0;
	ptr->pos = Nil;
	ptr->index = 0;
	ptr->size = 0;
}

void build_sequence_write_result(struct sequence_write *ptr, addr pos)
{
	ptr->listp = 0;
	ptr->reverse = 0;
	ptr->pos = pos;
	ptr->index = 0;
	ptr->size = 0;
}

int build_sequence_write_(struct sequence_write *ptr, addr pos)
{
	int check;
	size_t size;

	Return(listp_sequence_(pos, &check));
	if (check) {
		build_sequence_write_list(ptr);
	}
	else {
		Return(length_sequence_(pos, 1, &size));
		ptr->listp = 0;
		ptr->reverse = 0;
		ptr->pos = pos;
		ptr->size = size;
		ptr->index = 0;
	}

	return 0;
}

static addr nreverse_sequence_write(struct sequence_write *ptr)
{
	nreverse(&ptr->pos, ptr->pos);
	return ptr->pos;
}

addr result_sequence_write(struct sequence_write *ptr)
{
	if (ptr->listp)
		return nreverse_sequence_write(ptr);
	else
		return ptr->pos;
}

int push_sequence_write_(struct sequence_write *ptr, addr pos)
{
	size_t index;
	if (ptr->listp) {
		cons_heap(&ptr->pos, pos, ptr->pos);
		return 0;
	}
	if (ptr->reverse) {
		ptr->revsize--;
		index = ptr->revbase + ptr->revsize;
		Return(setelt_sequence_(ptr->pos, index, pos));
		if (ptr->revsize == 0)
			ptr->reverse = 0;
	}
	else {
		Return(setelt_sequence_(ptr->pos, ptr->index, pos));
	}
	ptr->index++;

	return 0;
}

int before_sequence_write_(struct sequence_write *ptr, struct sequence_range *range)
{
	addr pos, value;
	size_t size, i;

	size = range->start;
	pos = range->pos;
	if (range->listp) {
		for (i = 0; i < size; i++) {
			Return_getcons(pos, &value, &pos);
			Return(push_sequence_write_(ptr, value));
		}
	}
	else {
		for (i = 0; i < size; i++) {
			Return(getelt_sequence_(NULL, pos, i, &value));
			Return(push_sequence_write_(ptr, value));
		}
	}

	return 0;
}

int after_sequence_write_(struct sequence_write *ptr, struct sequence_range *range)
{
	addr pos, value;
	size_t size, i;

	if (range->listp) {
		pos = range->list;
		while (pos != Nil) {
			Return_getcons(pos, &value, &pos);
			Return(push_sequence_write_(ptr, value));
		}
	}
	else {
		pos = range->pos;
		Return(length_sequence_(pos, 1, &size));
		for (i = range->end; i < size; i++) {
			Return(getelt_sequence_(NULL, pos, i, &value));
			Return(push_sequence_write_(ptr, value));
		}
	}

	return 0;
}

void reverse_sequence_write(struct sequence_write *ptr, size_t size)
{
	Check(ptr->listp, "type error");
	if (size <= 1)
		return;
	ptr->reverse = 1;
	ptr->revsize = size;
	ptr->revbase = ptr->index;
}


/************************************************************
 *  setf.c
 ************************************************************/

static int setf_atom(addr pos)
{
	if (keywordp(pos))
		return 1;
	if (symbolp(pos))
		return 0;
	return ! consp(pos);
}


/*
 *  setf-the
 *
 *  (define-setf-expander the (type expr &environment env)
 *    (multiple-value-bind (a b g w r) (get-setf-expansion expr env)
 *      (values a b g
 *        `(multiple-value-bind ,g (the ,type (values ,@g)) ,w)
 *        `(the ,type ,r))))
 */
int function_setf_the(Execute ptr, addr form, addr env)
{
	addr args, type, expr, a, b, g, w, r;
	addr mvbind, the, values;

	Return_getcdr(form, &args);
	a = b = g = w = r = Nil;

	if (! consp_getcons(args, &type, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_THE, &the);
	GetConst(COMMON_VALUES, &values);
	Return(get_setf_expansion(ptr, expr, env, &a, &b, &g, &w, &r));
	/* read */
	list_heap(&r, the, type, r, NULL);
	/* write */
	cons_heap(&values, values, g);
	list_heap(&the, the, type, values, NULL);
	list_heap(&w, mvbind, g, the, w, NULL);
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return 0;

error:
	return fmte_("The form ~S must be a (the type expr) form.", form, NULL);
}


/*
 *  setf-values
 *
 *  (define-setf-expander values (&rest form &environment env) ...)
 */
int function_setf_values(Execute ptr, addr form, addr env)
{
	addr args, a, b, g, w, r, v, car, cdr, values;
	addr a1, b1, g1, w1, r1;

	Return_getcdr(form, &args);
	a = b = g = w = r = Nil;
	while (args != Nil) {
		Return_getcons(args, &v, &args);
		Return(get_setf_expansion(ptr, v, env, &a1, &b1, &g1, &w1, &r1));
		/* vars */
		while (a1 != Nil) {
			Return_getcons(a1, &v, &a1);
			cons_heap(&a, v, a);
		}
		/* vals */
		while (b1 != Nil) {
			Return_getcons(b1, &v, &b1);
			cons_heap(&b, v, b);
		}
		/* store */
		if (g1 != Nil) {
			Return_getcons(g1, &car, &cdr);
			cons_heap(&g, car, g);
			while (cdr != Nil) {
				Return_getcons(cdr, &v, &cdr);
				cons_heap(&a, v, a);
				cons_heap(&b, Nil, b);
			}
		}
		/* writer */
		cons_heap(&w, w1, w);
		/* reader */
		cons_heap(&r, r1, r);
	}

	/* result */
	GetConst(COMMON_VALUES, &values);
	nreverse(&a, a);
	nreverse(&b, b);
	nreverse(&g, g);
	nreverse(&w, w);
	nreverse(&r, r);
	cons_heap(&w, values, w);
	cons_heap(&r, values, r);
	setvalues_control(ptr, a, b, g, w, r, NULL);

	return 0;
}


/*
 *  setf-getf
 *
 *  (define-setf-expander getf (place indicator &optional default) ...)
 *  (get-setf-expansion '(getf x y))
 *    (g2)  ;; (indicator)
 *    (y)
 *    (g1)
 *    (let ((g3 (system::setplist g2 g1 r)))  ;; key, value, plist
 *      (setq x g3)
 *      g1)
 *    (getf r g2 g4)
 */
int function_setf_getf(Execute ptr, addr form, addr env)
{
	addr args, place, indicator, value;
	addr a, b, g, w, r, g1, g2, g3, g4;
	addr let, setplist, getf;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &place, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &indicator, &args);
	if (args == Nil)
		value = Nil;
	else {
		if (! consp(args))
			goto error;
		GetCons(args, &value, &args);
		if (args != Nil)
			goto error;
	}

	/* expander */
	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	Return(make_gensym_(ptr, &g1)); /* store */
	Return(make_gensym_(ptr, &g2)); /* indicator */
	Return_getcar(g, &g3);			/* temporary */
	Return(make_gensym_(ptr, &g4)); /* default */
	/* `(,g2 ,g4 ,@a) */
	cons_heap(&a, g4, a);
	cons_heap(&a, g2, a);
	/* `(,indicator ,default ,@b) */
	cons_heap(&b, value, b);
	cons_heap(&b, indicator, b);
	/* `(,g1) */
	conscar_heap(&g, g1);
	/* `(let ((,g3 (system::setplist ,g2 ,g1 ,r))) ,w ,g1) */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_SETPLIST, &setplist);
	list_heap(&setplist, setplist, g2, g1, r, NULL);
	list_heap(&setplist, g3, setplist, NULL);
	conscar_heap(&setplist, setplist);
	list_heap(&w, let, setplist, w, g1, NULL);
	/* `(getf ,r ,g2 ,g4) */
	GetConst(COMMON_GETF, &getf);
	list_heap(&r, getf, r, g2, g4, NULL);
	/* result */
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return 0;

error:
	return fmte_("(setf getf) argument ~S must be "
			"(place indicator &optional default) form.", form, NULL);
}


/*
 *  setf-apply
 *
 *  (define-setf-expander apply (call &rest args) ...)
 *  (get-setf-expansion '(apply call x y z))
 *    (g1 g2 g3)
 *    (x y z)
 *    (g)
 *    (apply (function (setf call)) g g1 g2 g3)
 *    (apply (function call) g1 g2 g3)
 */
int function_setf_apply(Execute ptr, addr form, addr env)
{
	addr args, call, pos, list, a, b, g, w, r;
	addr apply, funct, setf;

	GetConst(COMMON_APPLY, &apply);
	GetConst(COMMON_FUNCTION, &funct);
	GetConst(COMMON_SETF, &setf);

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &call, &args))
		goto error;

	/* call check */
	if (! consp_getcons(call, &call, &list))
		goto error_call;
	if (call != funct)
		goto error_call;
	if (! consp_getcons(list, &call, &list))
		goto error_call;
	if (! symbolp(call))
		goto error_call;
	if (list != Nil)
		goto error_call;

	/* gensym */
	Return(make_gensym_(ptr, &g));
	/* a */
	a = Nil;
	list = args;
	while (list != Nil) {
		Return_getcdr(list, &list);
		Return(make_gensym_(ptr, &pos));
		cons_heap(&a, pos, a);
	}
	/* b */
	b = args;
	/* w */
	list_heap(&pos, setf, call, NULL);
	list_heap(&pos, funct, pos, NULL);
	lista_heap(&w, apply, pos, g, a, NULL);
	/* r */
	list_heap(&pos, funct, call, NULL);
	list_heap(&r, apply, pos, a, NULL);
	/* g */
	conscar_heap(&g, g);
	/* result */
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return 0;

error:
	return fmte_("(setf apply) argument ~S "
			"must be (call &rest args) form.", form, NULL);

error_call:
	return fmte_("APPLY argument ~S must be a (FUNCTION symbol) form.", call, NULL);
}


/*
 *  setf-symbol
 *
 *  nil nil (#:g) (setq x #:g) x
 */
static int setf_symbol_(Execute ptr, addr form,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr gensym, setq;

	Return(make_gensym_(ptr, &gensym));
	*vars = *vals = Nil;
	conscar_heap(store, gensym);
	GetConst(COMMON_SETQ, &setq);
	list_heap(writer, setq, form, gensym, NULL);
	*reader = form;

	return 0;
}


/*
 *  setf-function
 *    (aaa x y z) ->
 *      (#:x #:y #:z)
 *      (x y z)
 *      (#:g)
 *      (funcall #'(setf aaa) #:g #:x #:y #:z)
 *      (aaa #:x #:y #:z)
 */
static int setf_function_(Execute ptr, addr symbol, addr args,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr a, b, c, d, e, funcall, function, setf, var, gen;

	/* (#:g) */
	Return(make_gensym_(ptr, &c));
	conscar_heap(store, c);
	/* (function #'(setf aaa)) */
	GetConst(COMMON_FUNCALL, &funcall);
	GetConst(COMMON_FUNCTION, &function);
	GetConst(COMMON_SETF, &setf);
	conscar_heap(&d, funcall);
	list_heap(&setf, setf, symbol, NULL);
	list_heap(&function, function, setf, NULL);
	cons_heap(&d, function, d);
	cons_heap(&d, c, d);
	/* (aaa) */
	conscar_heap(&e, symbol);
	/* loop */
	for (a = b = Nil; args != Nil; ) {
		Return_getcons(args, &var, &args);
		if (setf_atom(var)) {
			cons_heap(&d, var, d);
			cons_heap(&e, var, e);
		}
		else {
			Return(make_gensym_(ptr, &gen));
			cons_heap(&a, gen, a);
			cons_heap(&b, var, b);
			cons_heap(&d, gen, d);
			cons_heap(&e, gen, e);
		}
	}
	/* result */
	nreverse(vars, a);
	nreverse(vals, b);
	nreverse(writer, d);
	nreverse(reader, e);

	return 0;
}


/*
 *  get-setf-expansion
 */
static void getvalues_nil_control(Execute ptr, size_t index, addr *ret)
{
	getvalues_control(ptr, index, ret);
	if (*ret == Unbound)
		*ret = Nil;
}

static int setf_expander_call_(Execute ptr, LocalHold hold,
		addr call, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	Return(funcall_control(ptr, call, form, env, NULL));
	getvalues_nil_control(ptr, 0, vars);
	getvalues_nil_control(ptr, 1, vals);
	getvalues_nil_control(ptr, 2, store);
	getvalues_nil_control(ptr, 3, writer);
	getvalues_nil_control(ptr, 4, reader);
	localhold_set(hold, 0, *vars);
	localhold_set(hold, 1, *vals);
	localhold_set(hold, 2, *store);
	localhold_set(hold, 3, *writer);
	localhold_set(hold, 4, *reader);

	return 0;
}

static int setf_expander(Execute ptr, addr call, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 5);
	push_control(ptr, &control);
	(void)setf_expander_call_(ptr, hold,
			call, form, env, vars, vals, store, writer, reader);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int get_setf_expansion(Execute ptr, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	int result;
	addr pos, symbol, args, check;

	/* macroexpand */
	Return(macroexpand_(ptr, &pos, form, env, &result));
	if (result)
		form = pos;

	/* symbol */
	if (symbolp(form))
		return setf_symbol_(ptr, form, vars, vals, store, writer, reader);

	/* define-setf-expander */
	if (! consp(form))
		goto error;
	GetCons(form, &symbol, &args);
	if (! symbolp(symbol))
		goto error;
	getsetfmacro_symbol(symbol, &check);
	if (check != Unbound) {
		return setf_expander(ptr, check, form, env, vars, vals, store, writer, reader);
	}

	/* #'(setf form) */
	return setf_function_(ptr, symbol, args, vars, vals, store, writer, reader);

error:
	return fmte_("The form ~S is not setf place.", form, NULL);
}


/************************************************************
 *  sort.c
 ************************************************************/

#ifdef LISP_DEBUG
#define LISP_SORT_LIMIT 2
#else
#define LISP_SORT_LIMIT 16
#endif


/*
 *  unsafe
 */
int simplesort_cons_unsafe_(addr *ret,
		addr cons, int (*call_)(addr left, addr right, int *ret))
{
	int check;
	addr value, left, right;

	*ret = cons;
	while (cons != Nil) {
		GetCons(cons, &left, &value);
		while (value != Nil) {
			GetCar(value, &right);
			Return((*call_)(left, right, &check));
			if (! check) {
				/* swap */
				SetCar(cons, right);
				SetCar(value, left);
				left = right;
			}
			GetCdr(value, &value);
		}
		GetCdr(cons, &cons);
	}

	return 0;
}

int simplesort_info_cons_unsafe_(addr *ret, addr cons, addr info,
		int (*call_)(addr info, addr left, addr right, int *ret))
{
	int check;
	addr value, left, right;

	*ret = cons;
	while (cons != Nil) {
		GetCons(cons, &left, &value);
		while (value != Nil) {
			GetCar(value, &right);
			Return((*call_)(info, left, right, &check));
			if (! check) {
				/* swap */
				SetCar(cons, right);
				SetCar(value, left);
				left = right;
			}
			GetCdr(value, &value);
		}
		GetCdr(cons, &cons);
	}

	return 0;
}


/*
 *  simple-sort
 */
struct sort_struct {
	unsigned listp : 1;
	Execute ptr;
	LocalRoot local;
	addr pos, call, key, mem;
	size_t size;
};

static int key_sort_sequence_(struct sort_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	else
		return Result(ret, value);
}

static int call_sort_sequence_(struct sort_struct *str, int *ret, addr a2, addr b2)
{
	Return(callclang_funcall(str->ptr, &a2, str->call, a2, b2, NULL));
	return Result(ret, (a2 != Nil));
}

static void swap_list_sort_sequence(addr a, addr b)
{
	addr c1, c2;

	GetCar(a, &c1);
	GetCar(b, &c2);
	SetCar(a, c2);
	SetCar(b, c1);
}

static int swap_vector_sort_sequence_(addr pos, size_t a, size_t b)
{
	struct array_value v1, v2;

	Return(getelt_inplace_sequence_(pos, a, &v1));
	Return(getelt_inplace_sequence_(pos, b, &v2));
	Return(setelt_inplace_sequence_(NULL, pos, a, &v2));
	Return(setelt_inplace_sequence_(NULL, pos, b, &v1));

	return 0;
}

static int simple_sort_list_sequence_(struct sort_struct *str, addr p1, addr p2)
{
	int check;
	addr a1, a2, a3, a4, b1, b2, b3, b4, c;

	for (a1 = p1; a1 != p2; a1 = a4) {
		Return_getcons(a1, &a2, &a4);
		Return(key_sort_sequence_(str, &a3, a2));
		Return_getcdr(a1, &b1);
		for (; b1 != p2; b1 = b4) {
			Return_getcons(b1, &b2, &b4);
			Return(key_sort_sequence_(str, &b3, b2));
			Return(call_sort_sequence_(str, &check, a3, b3));
			if (! check) {
				swap_list_sort_sequence(a1, b1);
				c = a2; a2 = b2; b2 = c;
				c = a3; a3 = b3; b3 = c;
			}
		}
	}

	return 0;
}

static int simple_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check;
	size_t a1, b1;
	addr a2, a3, b2, b3, c;

	for (a1 = p1; a1 < p2; a1++) {
		Return(getelt_sequence_(NULL, pos, a1, &a2));
		Return(key_sort_sequence_(str, &a3, a2));
		for (b1 = a1 + 1; b1 < p2; b1++) {
			Return(getelt_sequence_(NULL, pos, b1, &b2));
			Return(key_sort_sequence_(str, &b3, b2));
			Return(call_sort_sequence_(str, &check, a3, b3));
			if (! check) {
				Return(swap_vector_sort_sequence_(pos, a1, b1));
				c = a2; a2 = b2; b2 = c;
				c = a3; a3 = b3; b3 = c;
			}
		}
	}

	return 0;
}

int simple_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		Return(simple_sort_list_sequence_(&str, pos, Nil));
	}
	else {
		Return(length_sequence_(pos, 1, &(str.size)));
		Return(simple_sort_vector_sequence_(&str, pos, 0, str.size));
	}

	return 0;
}


/*
 *  bubble-sort
 */
static int bubble_sort_list_sequence_(struct sort_struct *str, addr p1, addr p2)
{
	int check, swap;
	addr a1, a2, b1, b2, b3;

	while (p1 != p2) {
		a1 = p1;
		Return_getcons(a1, &a2, &b1);
		if (b1 == p2)
			break;
		Return(key_sort_sequence_(str, &a2, a2));
		for (swap = 0; b1 != p2; b1 = b3) {
			Return_getcons(b1, &b2, &b3);
			Return(key_sort_sequence_(str, &b2, b2));
			Return(call_sort_sequence_(str, &check, b2, a2));
			if (check) {
				swap_list_sort_sequence(a1, b1);
				swap = 1;
			}
			else {
				a2 = b2;
			}
			a1 = b1;
		}
		if (swap == 0)
			break;
		p2 = a1;
	}

	return 0;
}

static int bubble_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check, swap;
	size_t a1, b1;
	addr a2, b2;

	if (p2 - p1 <= 1)
		return 0;
	for (p2--; p1 < p2; p2--) {
		a1 = p1;
		Return(getelt_sequence_(NULL, pos, a1, &a2));
		Return(key_sort_sequence_(str, &a2, a2));
		for (swap = 0; a1 < p2; a1++) {
			b1 = a1 + 1;
			Return(getelt_sequence_(NULL, pos, b1, &b2));
			Return(key_sort_sequence_(str, &b2, b2));
			Return(call_sort_sequence_(str, &check, b2, a2));
			if (check) {
				Return(swap_vector_sort_sequence_(pos, a1, b1));
				swap = 1;
			}
			else {
				a2 = b2;
			}
		}
		if (swap == 0)
			break;
	}

	return 0;
}

int bubble_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		Return(bubble_sort_list_sequence_(&str, pos, Nil));
	}
	else {
		Return(length_sequence_(pos, 1, &(str.size)));
		Return(bubble_sort_vector_sequence_(&str, pos, 0, str.size));
	}

	return 0;
}


/*
 *  quick-sort
 */
static int quick_sort_list_length_sequence_(addr p1, addr p2, addr *ret, size_t *rsize)
{
	addr x, y, z;
	size_t i, next;

	next = 1;
	x = y = z = p1;
	for (i = 0; p1 != p2; i++) {
		Return_getcdr(p1, &x);
		if (next <= i) {
			z = y; y = p1;
			next <<= 1ULL;
			if (next < i)
				goto error;
		}
		p1 = x;
	}
	*ret = z;
	*rsize = i;
	return 0;

error:
	*ret = Nil;
	*rsize = 0;
	return fmte_("Too long list.", NULL);
}

static int quick_sort_list_sequence_(struct sort_struct *str, addr p1, addr p2)
{
	int check;
	addr a2, a3, a4, b1, b2, b3, b4, c1, c5, cons;
	size_t s0;

	/* initialize */
	Return(quick_sort_list_length_sequence_(p1, p2, &cons, &s0));
	if (s0 < LISP_SORT_LIMIT)
		return simple_sort_list_sequence_(str, p1, p2);
	swap_list_sort_sequence(p1, cons);

	/* sort */
	Return_getcons(p1, &a2, &a3);
	if (a3 == p2)
		return 0;
	Return(key_sort_sequence_(str, &a4, a2));
	c1 = a3;
	c5 = Nil;

	/* loop */
	for (b1 = c1; b1 != p2; b1 = b3) {
		Return_getcons(b1, &b2, &b3);
		Return(key_sort_sequence_(str, &b4, b2));
		Return(call_sort_sequence_(str, &check, a4, b4));
		if (! check) {
			swap_list_sort_sequence(c1, b1);
			c5 = c1;
			Return_getcdr(c1, &c1);
		}
	}

	/* recursive call */
	if (c1 == a3) {
		Return(quick_sort_list_sequence_(str, a3, p2));
	}
	else if (c1 == p2) {
		swap_list_sort_sequence(p1, c5);
		Return(quick_sort_list_sequence_(str, p1, c5));
	}
	else {
		swap_list_sort_sequence(p1, c5);
		Return(quick_sort_list_sequence_(str, p1, c5));
		Return(quick_sort_list_sequence_(str, c1, p2));
	}

	return 0;
}

static int quick_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check;
	addr a2, a4, b2, b4;
	size_t a3, b1, c1, c5, s0;

	/* simple-sort */
	Check(p2 < p1, "index error");
	s0 = (p2 - p1);
	if (s0 < LISP_SORT_LIMIT)
		return simple_sort_vector_sequence_(str, pos, p1, p2);

	/* sort */
	s0 = p1 + (s0 / 2);
	Return(swap_vector_sort_sequence_(pos, p1, s0));
	Return(getelt_sequence_(NULL, pos, p1, &a2));
	Return(key_sort_sequence_(str, &a4, a2));
	c1 = a3 = p1 + 1;
	c5 = 0;

	/* loop */
	for (b1 = c1; b1 < p2; b1++) {
		Return(getelt_sequence_(NULL, pos, b1, &b2));
		Return(key_sort_sequence_(str, &b4, b2));
		Return(call_sort_sequence_(str, &check, a4, b4));
		if (! check) {
			Return(swap_vector_sort_sequence_(pos, c1, b1));
			c5 = c1;
			c1++;
		}
	}

	/* recursive call */
	if (c1 == a3) {
		Return(quick_sort_vector_sequence_(str, pos, a3, p2));
	}
	else if (c1 == p2) {
		Return(swap_vector_sort_sequence_(pos, p1, c5));
		Return(quick_sort_vector_sequence_(str, pos, p1, c5));
	}
	else {
		Return(swap_vector_sort_sequence_(pos, p1, c5));
		Return(quick_sort_vector_sequence_(str, pos, p1, c5));
		Return(quick_sort_vector_sequence_(str, pos, c1, p2));
	}

	return 0;
}

int quick_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		Return(quick_sort_list_sequence_(&str, pos, Nil));
	}
	else {
		Return(length_sequence_(pos, 1, &(str.size)));
		Return(quick_sort_vector_sequence_(&str, pos, 0, str.size));
	}

	return 0;
}


/*
 *  merge-sort
 */
static int memory_copy_list_merge_sequence_(addr vector, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return_getcons(pos, &value, &pos);
		setarray(vector, i, value);
	}

	return 0;
}

static int merge_sort_list_merge_sequence_(struct sort_struct *str,
		addr a, size_t s1, addr b, size_t s2)
{
	int check;
	addr a1, a2, b1, b2, mem, c;
	size_t ai, bi;

	/* variable */
	mem = str->mem;
	Return(memory_copy_list_merge_sequence_(mem, a, s1));
	ai = bi = 0;
	Return_getcar(a, &a1);
	Return_getcar(b, &b1);
	Return(key_sort_sequence_(str, &a2, a1));
	Return(key_sort_sequence_(str, &b2, b1));

	/* merge */
loop:
	Return(call_sort_sequence_(str, &check, b2, a2));
	if (check) {
		Return_setcar(a, b1);
		Return_getcdr(a, &a);
		bi++;
		if (s2 <= bi)
			goto tail2;
		Return_getcdr(b, &b);
		Return_getcar(b, &b1);
		Return(key_sort_sequence_(str, &b2, b1));
	}
	else {
		Return_setcar(a, a1);
		Return_getcdr(a, &a);
		ai++;
		if (s1 <= ai)
			goto tail1;
		getarray(mem, ai, &a1);
		Return(key_sort_sequence_(str, &a2, a1));
	}
	goto loop;

tail1:
	Return_setcar(a, b1);
	for (bi++; bi < s2; bi++) {
		Return_getcdr(a, &a);
		Return_getcdr(b, &b);
		Return_getcar(b, &c);
		Return_setcar(a, c);
	}
	return 0;

tail2:
	Return_setcar(a, a1);
	for (ai++; ai < s1; ai++) {
		Return_getcdr(a, &a);
		getarray(mem, ai, &c);
		Return_setcar(a, c);
	}
	return 0;
}

static int merge_sort_list_sequence_(struct sort_struct *str,
		addr a, addr c, size_t s0)
{
	int check;
	addr b;
	size_t s1, s2;
	LocalStack stack;

	/* bubble-sort */
	if (s0 < LISP_SORT_LIMIT)
		return bubble_sort_list_sequence_(str, a, c);

	/* index */
	s1 = s0 / 2;
	s2 = s0 - s1;
	Return(getnthcdr_(a, s1, &b));

	/* memory */
	stack = NULL;
	check = (str->mem == Nil);
	if (check) {
		push_local(str->local, &stack);
		vector_local(str->local, &(str->mem), s1);
	}

	/* merge-sort */
	Return(merge_sort_list_sequence_(str, a, b, s1));
	Return(merge_sort_list_sequence_(str, b, c, s2));
	Return(merge_sort_list_merge_sequence_(str, a, s1, b, s2));

	/* rollback */
	if (check) {
		rollback_local(str->local, stack);
	}

	return 0;
}

static int memory_copy_vector_merge_sequence_(addr vector,
		addr pos, size_t index, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(getelt_sequence_(NULL, pos, index++, &value));
		setarray(vector, i, value);
	}

	return 0;
}

static int merge_sort_vector_merge_sequence_(struct sort_struct *str,
		addr pos, size_t a, size_t s1, size_t b, size_t s2)
{
	int check;
	addr a1, a2, b1, b2, mem, c;
	size_t ai, bi;

	/* variable */
	mem = str->mem;
	Return(memory_copy_vector_merge_sequence_(mem, pos, a, s1));
	ai = bi = 0;
	getarray(mem, 0, &a1);
	Return(getelt_sequence_(NULL, pos, b, &b1));
	Return(key_sort_sequence_(str, &a2, a1));
	Return(key_sort_sequence_(str, &b2, b1));

	/* merge */
loop:
	Return(call_sort_sequence_(str, &check, b2, a2));
	if (check) {
		Return(setelt_sequence_(pos, a, b1));
		a++;
		bi++;
		if (s2 <= bi)
			goto tail2;
		b++;
		Return(getelt_sequence_(NULL, pos, b, &b1));
		Return(key_sort_sequence_(str, &b2, b1));
	}
	else {
		Return(setelt_sequence_(pos, a, a1));
		a++;
		ai++;
		if (s1 <= ai)
			goto tail1;
		getarray(mem, ai, &a1);
		Return(key_sort_sequence_(str, &a2, a1));
	}
	goto loop;

tail1:
	Return(setelt_sequence_(pos, a, b1));
	for (bi++; bi < s2; bi++) {
		a++;
		b++;
		Return(getelt_sequence_(NULL, pos, b, &c));
		Return(setelt_sequence_(pos, a, c));
	}
	return 0;

tail2:
	Return(setelt_sequence_(pos, a, a1));
	for (ai++; ai < s1; ai++) {
		a++;
		getarray(mem, ai, &c);
		Return(setelt_sequence_(pos, a, c));
	}
	return 0;
}

static int merge_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t a, size_t c)
{
	int check;
	size_t s0, s1, s2, b;
	LocalStack stack;

	/* bubble-sort */
	Check(c < a, "index error");
	s0 = c - a;
	if (s0 < LISP_SORT_LIMIT)
		return bubble_sort_vector_sequence_(str, pos, a, c);

	/* index */
	s1 = s0 / 2;
	s2 = s0 - s1;
	b = a + s1;

	/* memory */
	stack = NULL;
	check = (str->mem == Nil);
	if (check) {
		push_local(str->local, &stack);
		vector_local(str->local, &(str->mem), s1);
	}

	/* merge-sort */
	Return(merge_sort_vector_sequence_(str, pos, a, b));
	Return(merge_sort_vector_sequence_(str, pos, b, c));
	Return(merge_sort_vector_merge_sequence_(str, pos, a, s1, b, s2));

	/* rollback */
	if (check) {
		rollback_local(str->local, stack);
	}

	return 0;
}

int merge_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	str.mem = Nil;
	Return(length_sequence_(pos, 1, &(str.size)));
	if (listp)
		return merge_sort_list_sequence_(&str, pos, Nil, str.size);
	else
		return merge_sort_vector_sequence_(&str, pos, 0, str.size);
}


/************************************************************
 *  step.c
 ************************************************************/

/*
 *  step macro
 */
int step_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, eval, let, special, step;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &eval, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(let ((system::*step-break* t))
	 *    (system::step ,expr))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_STEP_BREAK, &special);
	GetConst(SYSTEM_STEP, &step);
	list_heap(&special, special, T, NULL);
	list_heap(&special, special, NULL);
	list_heap(&step, step, eval, NULL);
	list_heap(ret, let, special, step, NULL);
	return 0;

error:
	return fmte_("The form ~S must be (eval).", form, NULL);
}


/*
 *  parse
 */
static void parse_step_symbol(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_STEP_PARSE, ret);
}

void init_parse_step(Execute ptr)
{
	addr symbol;
	parse_step_symbol(ptr, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

static int parse_step_p_(Execute ptr, int *ret)
{
	addr symbol, value;
	parse_step_symbol(ptr, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	return Result(ret, value != Nil);
}

int parse_step(Execute ptr, addr *ret, addr form)
{
	addr args, expr, symbol, value;

	if (! consp_getcons(form, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* step */
	parse_step_symbol(ptr, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	setspecial_local(ptr, symbol, T);
	Return(parse_execute_(ptr, ret, expr));
	setspecial_local(ptr, symbol, value);
	return 0;

error:
	return fmte_("The form ~S must be (eval).", form, NULL);
}

int parse_step_object_(Execute ptr, addr *ret, addr value, addr expr)
{
	int check;
	addr eval;

	Return(parse_step_p_(ptr, &check));
	if (! check)
		return Result(ret, expr);

	eval_parse_heap(&eval, EVAL_PARSE_STEP, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, value);

	return Result(ret, eval);
}


/*
 *  copy-eval
 */
void copy_eval_step(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr expr, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_STEP, "parse error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &value);

	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}


/*
 *  scope
 */
int scope_step(Execute ptr, addr *ret, addr eval)
{
	addr expr, value, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &value);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_STEP, type, eval));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, value);
	return Result(ret, eval);
}


/************************************************************
 *  step_prompt.c
 ************************************************************/

/*
 *  query
 */
static int step_prompt_exit_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret,
			"Q", "QUIT", "E", "EXIT", "S", "STEP", NULL);
}

static int step_prompt_over_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "O", "OVER", NULL);
}

static void step_prompt_over(Execute ptr)
{
	addr symbol;
	GetConst(SYSTEM_STEP_VALUE, &symbol);
	setspecial_local(ptr, symbol, Nil);
}

static int step_prompt_help_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "?", "H", "HELP", NULL);
}

static int step_prompt_help_(Execute ptr, addr io)
{
	static const char *const message[] = {
		"Step help.",
		"---",
		"step  Step in",
		"over  Step over",
		"---",
		"quit  Quit step.",
		"help  Output this message.",
		"---",
		NULL
	};
	int i;
	const char *str;

	for (i = 0; ; i++) {
		str = message[i];
		if (str == NULL)
			break;
		Return(print_ascii_stream_(io, str));
		Return(terpri_stream_(io));
		Return(force_output_stream_(io));
	}

	return 0;
}

static int step_prompt_loop_(Execute ptr, addr io, addr pos, int *exit, int *exec)
{
	int check;

	Return(step_prompt_exit_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		return 0;
	}
	Return(step_prompt_over_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		step_prompt_over(ptr);
		return 0;
	}
	Return(step_prompt_help_p_(pos, &check));
	if (check) {
		*exit = 0;
		*exec = 0;
		return step_prompt_help_(ptr, io);
	}
	*exit = 0;
	*exec = 1;
	return 0;
}

static int step_prompt_query_call_(Execute ptr, addr io, addr value, int *ret)
{
	addr symbol, prompt;

	GetConst(SYSTEM_STEP_VALUE, &symbol);
	strvect_char_heap(&prompt, "Step> ");
	pushspecial_control(ptr, symbol, T);
	push_prompt(ptr, prompt, prompt_step);
	Return(eval_custom_loop_(ptr, io, step_prompt_loop_));
	getspecial_local(ptr, symbol, &value);

	return Result(ret, (value != Nil));
}

static int step_prompt_query_(Execute ptr, addr value, int *ret)
{
	addr io, control;

	Return(debug_io_stream_(ptr, &io));
	Return(format_stream(ptr, io, "~&STEP: ~S~%", value, NULL));

	push_control(ptr, &control);
	(void)step_prompt_query_call_(ptr, io, value, ret);
	return pop_control_(ptr, control);
}


/*
 *  code
 */
int execute_step_code(Execute ptr, addr expr, addr value)
{
	int stepin;
	addr symbol, check, control;

	/* *step-break* */
	GetConst(SYSTEM_STEP_BREAK, &symbol);
	getspecial_local(ptr, symbol, &check);
	if (check == Nil) {
		/* step throw */
		return runcode_control_(ptr, expr);
	}

	/* query */
	stepin = 0;
	Return(step_prompt_query_(ptr, value, &stepin));
	if (stepin) {
		/* step in */
		return runcode_control_(ptr, expr);
	}

	/* step over */
	push_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	(void)runcode_control_(ptr, expr);
	return pop_control_(ptr, control);
}


/************************************************************
 *  stream.c
 ************************************************************/

/*
 *  stream
 */
int open_stream_p(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->closed == 0;
}

int copyleft_stream_(addr stream, addr src)
{
	size_t size;
	Return(getleft_stream_(src, &size));
	return setleft_stream_(stream, size);
}

int pageout_stream_(addr stream)
{
	return write_char_stream_(stream, '\f');
}

int print_ascii_stream_(addr stream, const char *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *(const byte *)data;
		if (c == 0)
			break;
		Return(write_char_stream_(stream, (unicode)c));
		data++;
	}

	return 0;
}

int print_unicode_stream_(addr stream, const unicode *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *data;
		if (c == 0)
			break;
		Return(write_char_stream_(stream, c));
		data++;
	}

	return 0;
}

int print_string_stream_(addr stream, addr pos)
{
	unicode c;
	size_t size, i;

	CheckType(stream, LISPTYPE_STREAM);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int stream_designer_(Execute ptr, addr stream, addr *ret, int inputp)
{
	addr type;

	/* default */
	if (stream == Unbound || stream == Nil) {
		if (inputp)
			return standard_input_stream_(ptr, ret);
		else
			return standard_output_stream_(ptr, ret);
	}

	/* stream */
	if (streamp(stream))
		return Result(ret, stream);

	/* boolean */
	if (stream == T)
		return terminal_io_stream_(ptr, ret);

	/* error */
	*ret = Nil;
	GetTypeTable(&type, StreamDesigner);
	return call_type_error_(ptr, stream, type);
}

int input_stream_designer_(Execute ptr, addr stream, addr *ret)
{
	return stream_designer_(ptr, stream, ret, 1);
}

int output_stream_designer_(Execute ptr, addr stream, addr *ret)
{
	return stream_designer_(ptr, stream, ret, 0);
}


/*
 *  special variable
 */
static int specialvalue_(Execute ptr, constindex index, addr *ret)
{
	addr symbol;
	GetConstant(index, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

int standard_input_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_STANDARD_INPUT, ret);
}

int standard_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_STANDARD_OUTPUT, ret);
}

int error_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_ERROR_OUTPUT, ret);
}

int trace_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_TRACE_OUTPUT, ret);
}

int terminal_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_TERMINAL_IO, ret);
}

int debug_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_DEBUG_IO, ret);
}

int query_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_QUERY_IO, ret);
}


/*
 *  wrapper
 */
int read_unsigned8_stream_(addr stream, byte *value, int *ret)
{
	int check;
	addr pos, type;
	fixnum v;

	Return(read_byte_stream_(stream, &pos, &check));
	if (check) {
		*value = 0;
		return Result(ret, 1);
	}
	if (GetFixnum_signed(pos, &v) || ! IsByteSign(v)) {
		Return(element_type_stream_(stream, &type));
		return call_type_error_(NULL, pos, type);
	}
	*value = (byte)v;
	return Result(ret, 0);
}

int write_unsigned8_stream_(addr stream, byte value)
{
	addr pos;

	fixnum_heap(&pos, (fixnum)value);
	Return(write_byte_stream_(stream, pos));
	return exitpoint_stream_(stream);
}

#define REDIRECT_UNSIGNED8_SIZE		4096
static int read_redirect_unsigned8_stream_(Execute ptr,
		addr stream, byte *data, int *ret)
{
	int i, check;
	byte c;

	c = 0;
	for (i = 0; i < REDIRECT_UNSIGNED8_SIZE; i++) {
		Return(read_unsigned8_stream_(stream, &c, &check));
		if (check)
			break;
		data[i] = c;
	}

	return Result(ret, i);
}

static int write_redirect_unsigned8_stream_(Execute ptr,
		addr stream, byte *data, int size)
{
	int i;
	addr value;

	for (i = 0; i < size; i++) {
		fixnum_heap(&value, (fixnum)data[i]);
		Return(write_byte_stream_(stream, value));
	}

	return 0;
}

int redirect_unsigned8_stream_(Execute ptr, addr src, addr dst)
{
	byte data[REDIRECT_UNSIGNED8_SIZE];
	int size;

	for (;;) {
		Return(read_redirect_unsigned8_stream_(ptr, src, data, &size));
		if (size == 0)
			break;
		Return(write_redirect_unsigned8_stream_(ptr, dst, data, size));
	}

	return 0;
}

int close_stream_unwind_protect_(Execute ptr, addr stream)
{
	addr control, save, ignore;

	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (close_stream_(stream, &ignore))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}


/*
 *  core
 */
void update_standard_stream(void)
{
	addr pos;

	/* stdin */
	GetConst(STREAM_STDIN, &pos);
	update_standard_input(pos);
	/* stdout */
	GetConst(STREAM_STDOUT, &pos);
	update_standard_output(pos);
	/* stderr */
	GetConst(STREAM_STDERR, &pos);
	update_standard_error(pos);
}

int save_stream(addr pos)
{
	switch (PtrStructStream(pos)->type) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_CharacterInput:
		case StreamType_CharacterOutput:
		case StreamType_CharacterIO:
		case StreamType_Probe:
			return save_stream_file(pos);

		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
			return save_stream_system(pos);

		default:
			break;
	}

	return 0;
}


/************************************************************
 *  stream_broadcast.c
 ************************************************************/

#define CheckBroadCastStream(stream) { \
	Check(! broadcast_stream_p(stream), "type error"); \
}

int open_broadcast_stream_(addr *stream, addr list)
{
	addr pos;

	if (! listp(list))
		return TypeError_(list, LIST);
	stream_heap(&pos, StreamType_BroadCast, 0);
	SetInfoStream(pos, list);
	force_open_stream(pos);

	return Result(stream, pos);
}

void push_broadcast_stream(addr stream, addr output)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	cons_heap(&list, output, list);
	SetInfoStream(stream, list);
}

void get_broadcast_stream(addr stream, addr *ret)
{
	CheckBroadCastStream(stream);
	GetInfoStream(stream, ret);
}

static int write_byte_BroadCast(addr stream, addr value)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(write_byte_stream_(pos, value));
	}

	return 0;
}

static int write_char_BroadCast(addr stream, unicode c)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(write_char_stream_(pos, c));
	}
	charleft_default_stream(stream, c);

	return 0;
}

static int getleft_BroadCast(addr stream, size_t *ret)
{
	CheckBroadCastStream(stream);
	return getleft_default_stream(stream, ret);
}

static int setleft_BroadCast(addr stream, size_t value)
{
	CheckBroadCastStream(stream);
	return setleft_default_stream(stream, value);
}

static int characterp_BroadCast(addr stream, int *ret)
{
	int value, check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (value = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(characterp_stream_(pos, &check));
		value = value && check;
	}

	return Result(ret, value);
}

static int binaryp_BroadCast(addr stream, int *ret)
{
	int value, check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (value = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(binaryp_stream_(pos, &check));
		value = value && check;
	}

	return Result(ret, value);
}

static int last_component_BroadCast(addr stream, addr *ret)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	if (list == Nil)
		return 1;
	while (list != Nil) {
		if (! consp_getcons(list, ret, &list))
			break;
	}
	return 0;
}

static int element_type_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream))
		return Result(ret, T);
	else
		return element_type_stream_(stream, ret);
}

static int external_format_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		GetConst(KEYWORD_DEFAULT, ret);
		return 0;
	}

	return external_format_stream_(stream, ret);
}

static int file_length_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	else {
		return file_length_stream_(stream, ret);
	}
}

static int file_position_BroadCast(addr stream, size_t *value, int *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*value = 0;
		return Result(ret, 0);
	}
	else {
		return file_position_stream_(stream, value, ret);
	}
}

static int file_position_start_BroadCast(addr stream, int *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(file_position_start_stream_(pos, &check));
	}

	return Result(ret, check);
}

static int file_position_end_BroadCast(addr stream, int *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(file_position_end_stream_(pos, &check));
	}

	return Result(ret, check);
}

static int file_position_set_BroadCast(addr stream, size_t value, int *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(file_position_set_stream_(pos, value, &check));
	}

	return Result(ret, check);
}

static int file_charlen_BroadCast(addr stream,
		unicode u, size_t *value, int *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*value = 1;
		return Result(ret, 0);
	}
	else {
		return file_charlen_stream_(stream, u, value, ret);
	}
}

static int file_strlen_BroadCast(addr stream, addr pos, size_t *value, int *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*value = 1;
		return Result(ret, 0);
	}
	else {
		return file_strlen_stream_(stream, pos, value, ret);
	}
}

static int finish_output_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(finish_output_stream_(pos));
	}

	return 0;
}

static int force_output_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(force_output_stream_(pos));
	}

	return 0;
}

static int clear_output_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clear_output_stream_(pos));
	}

	return 0;
}

static int exitpoint_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(exitpoint_stream_(pos));
	}

	return 0;
}

void init_stream_broadcast(void)
{
	DefineStreamDef(BroadCast, close);
	DefineStream___(BroadCast, read_byte);
	DefineStream___(BroadCast, unread_byte);
	DefineStreamSet(BroadCast, write_byte);
	DefineStream___(BroadCast, read_char);
	DefineStream___(BroadCast, read_hang);
	DefineStream___(BroadCast, unread_char);
	DefineStreamSet(BroadCast, write_char);
	DefineStreamSet(BroadCast, getleft);
	DefineStreamSet(BroadCast, setleft);
	DefineStreamChk(BroadCast, inputp, false);
	DefineStreamChk(BroadCast, outputp, true);
	DefineStreamChk(BroadCast, interactivep, false);
	DefineStreamSet(BroadCast, characterp);
	DefineStreamSet(BroadCast, binaryp);
	DefineStreamSet(BroadCast, element_type);
	DefineStreamSet(BroadCast, external_format);
	DefineStreamSet(BroadCast, file_length);
	DefineStreamSet(BroadCast, file_position);
	DefineStreamSet(BroadCast, file_position_start);
	DefineStreamSet(BroadCast, file_position_end);
	DefineStreamSet(BroadCast, file_position_set);
	DefineStreamSet(BroadCast, file_charlen);
	DefineStreamSet(BroadCast, file_strlen);
	DefineStream___(BroadCast, listen);
	DefineStream___(BroadCast, clear_input);
	DefineStreamSet(BroadCast, finish_output);
	DefineStreamSet(BroadCast, force_output);
	DefineStreamSet(BroadCast, clear_output);
	DefineStreamSet(BroadCast, exitpoint);
	DefineStream___(BroadCast, termsize);
}


/************************************************************
 *  stream_common.c
 ************************************************************/

/*
 *  binary
 */
static int get_binary_stream(addr stream, addr *ret)
{
	struct StructStream *str;

	str = PtrStructStream(stream);
	switch (str->type) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
		case StreamType_Probe:
			*ret = stream;
			return 1;

		case StreamType_Synonym:
			get_synonym_stream(stream, &stream);
			return get_binary_stream(stream, ret);

		default:
			*ret = stream;
			return 0;
	}
}

static int read_binary_from_byte_(addr stream, byte *pos, size_t size, size_t *ret)
{
	int check;
	byte c;
	size_t x;

	for (x = 0; x < size; x++) {
		Return(read_unsigned8_stream_(stream, &c, &check));
		if (check)
			break;
		pos[x] = c;
	}

	return Result(ret, x);
}

int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	if (get_binary_stream(stream, &stream))
		return read_binary_file_(stream, pos, size, ret);
	else
		return read_binary_from_byte_(stream, (byte *)pos, size, ret);
}

static int write_binary_from_byte_(addr stream,
		const byte *pos, size_t size, size_t *ret)
{
	size_t x;

	for (x = 0; x < size; x++) {
		Return(write_unsigned8_stream_(stream, pos[x]));
	}

	return Result(ret, x);
}

int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	if (get_binary_stream(stream, &stream))
		return write_binary_file_(stream, pos, size, ret);
	else
		return write_binary_from_byte_(stream, (const byte *)pos, size, ret);
}


/*
 *  terpri
 */
int terpri_stream_(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return write_char_stream_(stream, '\n');
}


/*
 *  fresh-line
 */
int fresh_line_stream_(addr stream, int *ret)
{
	size_t size;

	CheckType(stream, LISPTYPE_STREAM);
	Return(getleft_stream_(stream, &size));
	if (size == 0) {
		if (ret)
			*ret = 0;
		return 0;
	}
	Return(terpri_stream_(stream));
	if (ret)
		*ret = 1;
	return 0;
}


/*
 *  peek-char
 */
static int end_of_file_recursive_(Execute ptr, addr pos, int recp)
{
	return call_end_of_file_(ptr, pos);
}

static int peek_char_nil_(Execute ptr, addr *ret,
		addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c;

	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		if (errorp) {
			*ret = Nil;
			return end_of_file_recursive_(ptr, stream, recp);
		}
		return Result(ret, value);
	}

	Return(unread_char_stream_(stream, c));
	character_heap(ret, c);
	return 0;
}

static int peek_char_t_(Execute ptr, addr *ret,
		addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c;

	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			if (errorp) {
				*ret = Nil;
				return end_of_file_recursive_(ptr, stream, recp);
			}
			*ret = value;
			break;
		}
		if (! isSpaceUnicode(c)) {
			Return(unread_char_stream_(stream, c));
			character_heap(ret, c);
			break;
		}
	}

	return 0;
}

static int peek_char_character_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c, v;

	GetCharacter(type, &v);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			if (errorp) {
				*ret = Nil;
				return end_of_file_recursive_(ptr, stream, recp);
			}
			*ret = value;
			break;
		}
		if (v == c) {
			Return(unread_char_stream_(stream, c));
			Return(peek_char_nil_(ptr, ret, stream, errorp, value, recp));
			break;
		}
	}

	return 0;
}

int peek_char_stream_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	Return(input_stream_designer_(ptr, stream, &stream));
	if (type == Nil)
		return peek_char_nil_(ptr, ret, stream, errorp, value, recp);
	else if (type == T)
		return peek_char_t_(ptr, ret, stream, errorp, value, recp);
	else
		return peek_char_character_(ptr, ret, type, stream, errorp, value, recp);
}


/*
 *  read-line
 */
enum EndOfLine_Mode {
	EndOfLine_Auto,
	EndOfLine_CR,
	EndOfLine_LF,
	EndOfLine_CRLF
};

static int get_end_of_line_mode_(Execute ptr, enum EndOfLine_Mode *ret)
{
	addr pos, check;

	GetConst(SYSTEM_END_OF_LINE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* Auto */
	GetConst(SYSTEM_AUTO, &check);
	if (check == pos)
		return Result(ret, EndOfLine_Auto);
	/* CR */
	GetConst(SYSTEM_CR, &check);
	if (check == pos)
		return Result(ret, EndOfLine_CR);
	/* LF */
	GetConst(SYSTEM_LF, &check);
	if (check == pos)
		return Result(ret, EndOfLine_LF);
	/* CRLF */
	GetConst(SYSTEM_CRLF, &check);
	if (check == pos)
		return Result(ret, EndOfLine_CRLF);
	/* error */
	*ret = EndOfLine_Auto;
	return fmte_("Invalid *end-of-line* value ~S.", pos, NULL);
}

static int read_line_stream_auto_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	int loop, check, type;
	unicode c;

	type = 0;
	for (loop = 0; ; loop = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check) {
			type = 0;
			break;
		}
		if (c == 0x0A) {
			type = 1;
			break;
		}
		if (c == 0x0D) {
			Return(read_char_stream_(pos, &c, &check));
			if (check == 0 && c != 0x0A) {
				Return(unread_char_stream_(pos, c));
			}
			type = 1;
			break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

	*rloop = loop;
	return Result(ret, type);
}

static int read_line_stream_value_(LocalRoot local,
		addr pos, addr queue, unicode eol, int *rloop, int *ret)
{
	int loop, check, type;
	unicode c;

	type = 0;
	for (loop = 0; ; loop = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check) {
			type = 0;
			break;
		}
		if (c == eol) {
			type = 1;
			break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

	*rloop = loop;
	return Result(ret, type);
}

static int read_line_stream_cr_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	return read_line_stream_value_(local, pos, queue, 0x0D, rloop, ret);
}

static int read_line_stream_lf_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	return read_line_stream_value_(local, pos, queue, 0x0A, rloop, ret);
}

static int read_line_stream_crlf_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	int loop, check, type;
	unicode c;

	type = 0;
	for (loop = 0; ; loop = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check) {
			type = 0;
			break;
		}
		if (c == 0x0A)
			goto error;
		if (c == 0x0D) {
			Return(read_char_stream_(pos, &c, &check));
			if (check || c != 0x0A)
				goto error;
			type = 1;
			break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

	*rloop = loop;
	return Result(ret, type);

error:
	*rloop = 0;
	*ret = 0;
	return fmte_("Invalid CR-LF code.", NULL);
}

static int read_line_stream_mode_(Execute ptr,
		addr pos, addr queue, int *loop, int *ret)
{
	enum EndOfLine_Mode mode;
	LocalRoot local;

	local = ptr->local;
	Return(get_end_of_line_mode_(ptr, &mode));
	switch (mode) {
		case EndOfLine_Auto:
			return read_line_stream_auto_(local, pos, queue, loop, ret);

		case EndOfLine_CR:
			return read_line_stream_cr_(local, pos, queue, loop, ret);

		case EndOfLine_LF:
			return read_line_stream_lf_(local, pos, queue, loop, ret);

		case EndOfLine_CRLF:
			return read_line_stream_crlf_(local, pos, queue, loop, ret);

		default:
			return read_line_stream_auto_(local, pos, queue, loop, ret);
	}
}

static int read_line_stream_loop_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	int loop, type;
	addr queue;

	charqueue_local(ptr->local, &queue, 0);
	Return(read_line_stream_mode_(ptr, pos, queue, &loop, &type));
	if (type) {
		make_charqueue_heap(queue, ret);
		return Result(miss, 0);
	}

	/* result */
	if (loop != 0) {
		make_charqueue_heap(queue, ret);
		return Result(miss, 1);
	}

	/* error */
	if (errorp) {
		*ret = Nil;
		*miss = 0;
		return end_of_file_recursive_(ptr, pos, recp);
	}
	*ret = value;
	return Result(miss, 1);
}

int read_line_stream_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	LocalRoot local;
	LocalStack stack;

	Return(input_stream_designer_(ptr, pos, &pos));
	local = ptr->local;
	push_local(local, &stack);
	Return(read_line_stream_loop_(ptr, ret, miss, pos, errorp, value, recp));
	rollback_local(local, stack);

	return 0;
}


/*
 *  write-string
 */
int write_string_stream(Execute ptr, addr string, addr rest, addr *ret)
{
	unicode c;
	addr stream;
	size_t size, start, end, i;

	/* argument */
	string_length(string, &size);
	if (rest == Nil) {
		Return(output_stream_designer_(ptr, Unbound, &stream));
		start = 0;
		end = size;
	}
	else {
		Return_getcons(rest, &stream, &rest);
		Return(output_stream_designer_(ptr, stream, &stream));
		Return(keyword_start_end_(size, rest, &start, &end));
	}

	for (i = start; i < end; i++) {
		Return(string_getc_(string, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return Result(ret, stream);
}


/*
 *  read-sequence
 */
static int read_sequence_character_(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;
	unicode c;
	addr value;

	for (; start < end; start++) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		character_heap(&value, c);
		Return(setelt_sequence_(seq, start, value));
	}
	make_index_integer_heap(&value, start);

	return Result(ret, value);
}

static int read_sequence_binary_(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;
	addr value;

	for (; start < end; start++) {
		Return(read_byte_stream_(stream, &value, &check));
		if (check)
			break;
		Return(setelt_sequence_(seq, start, value));
	}
	make_index_integer_heap(&value, start);

	return Result(ret, value);
}

int read_sequence_stream(addr *ret, addr seq, addr stream, size_t start, size_t end)
{
	int check;

	/* character stream */
	Return(characterp_stream_(stream, &check));
	if (check)
		return read_sequence_character_(ret, seq, stream, start, end);

	/* binary stream */
	Return(binaryp_stream_(stream, &check));
	if (check)
		return read_sequence_binary_(ret, seq, stream, start, end);

	/* error */
	return fmte_("Invalid stream ~S.", stream, NULL);
}


/*
 *  write-sequence
 */
static int write_sequence_character_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	unicode c;
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		Return(getelt_sequence_(local, seq, start, &value));
		if (! characterp(value))
			return TypeError_(value, CHARACTER);
		GetCharacter(value, &c);
		rollback_local(local, stack);
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int write_sequence_binary_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		Return(getelt_sequence_(local, seq, start, &value));
		if (! fixnump(value))
			return TypeError_(value, INTEGER);
		Return(write_byte_stream_(stream, value));
		rollback_local(local, stack);
	}

	return 0;
}

int write_sequence_stream(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;

	/* character stream */
	Return(characterp_stream_(stream, &check));
	if (check)
		return write_sequence_character_(local, seq, stream, start, end);

	/* binary stream */
	Return(binaryp_stream_(stream, &check));
	if (check)
		return write_sequence_binary_(local, seq, stream, start, end);

	/* error */
	return fmte_("Invalid stream ~S.", stream, NULL);
}


/************************************************************
 *  stream_concat.c
 ************************************************************/

#define CheckConcatenatedStream(stream) { \
	Check(! concatenated_stream_p(stream), "type error"); \
}

int open_concatenated_stream_(addr *stream, addr list)
{
	addr pos;

	if (! listp(list))
		return TypeError_(list, LIST);
	stream_heap(&pos, StreamType_Concatenated, 0);
	SetInfoStream(pos, list);
	force_open_stream(pos);

	return Result(stream, pos);
}

void push_concatenated_stream(addr stream, addr input)
{
	addr list;

	CheckConcatenatedStream(stream);
	GetInfoStream(stream, &list);
	cons_heap(&list, input, list);
	SetInfoStream(stream, list);
}

void get_concatenated_stream(addr stream, addr *ret)
{
	CheckConcatenatedStream(stream);
	GetInfoStream(stream, ret);
}

static void current_concatenated(addr stream, addr *ret)
{
	CheckConcatenatedStream(stream);
	GetInfoStream(stream, &stream);
	if (stream == Nil) {
		*ret = Nil;
	}
	else {
		GetCar(stream, ret);
	}
}

static int read_byte_Concatenated(addr stream, addr *value, int *ret)
{
	int check;
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return Result(ret, 1);
		Return_getcons(list, &pos, &list);
		Return(read_byte_stream_(pos, value, &check));
		if (! check)
			break;
		SetInfoStream(stream, list);
	}

	return Result(ret, 0);
}

static int unread_byte_Concatenated(addr stream, byte c)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return 0;
	return unread_byte_stream_(stream, c);
}

static int read_char_Concatenated(addr stream, unicode *u, int *ret)
{
	int check;
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return Result(ret, 1);
		Return_getcons(list, &pos, &list);
		Return(read_char_stream_(pos, u, &check));
		if (! check)
			break;
		SetInfoStream(stream, list);
	}

	return Result(ret, 0);
}

static int read_hang_Concatenated(addr stream, unicode *u, int *hang, int *ret)
{
	int check;
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return Result(ret, 1);
		Return_getcons(list, &pos, &list);
		Return(read_hang_stream_(pos, u, hang, &check));
		if (! check)
			break;
		SetInfoStream(stream, list);
	}

	return Result(ret, 0);
}

static int unread_char_Concatenated(addr stream, unicode c)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		return unread_char_stream_(stream, c);

	return 0;
}

static int interactivep_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 0);
	else
		return interactivep_stream_(stream, ret);
}

static int characterp_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 1);
	else
		return characterp_stream_(stream, ret);
}

static int binaryp_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 1);
	else
		return binaryp_stream_(stream, ret);
}

static int element_type_Concatenated(addr stream, addr *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, Nil);
	else
		return element_type_stream_(stream, ret);
}

static int external_format_Concatenated(addr stream, addr *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil) {
		GetConst(KEYWORD_DEFAULT, ret);
		return 0;
	}

	return external_format_stream_(stream, ret);
}

static int listen_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 0);
	else
		return listen_stream_(stream, ret);
}

static int clear_input_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		return clear_input_stream_(stream);
	return 0;
}

static int exitpoint_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		return exitpoint_stream_(stream);
	return 0;
}

void init_stream_concatenated(void)
{
	DefineStreamDef(Concatenated, close);
	DefineStreamSet(Concatenated, read_byte);
	DefineStreamSet(Concatenated, unread_byte);
	DefineStream___(Concatenated, write_byte);
	DefineStreamSet(Concatenated, read_char);
	DefineStreamSet(Concatenated, read_hang);
	DefineStreamSet(Concatenated, unread_char);
	DefineStream___(Concatenated, write_char);
	DefineStream___(Concatenated, getleft);
	DefineStream___(Concatenated, setleft);
	DefineStreamChk(Concatenated, inputp, true);
	DefineStreamChk(Concatenated, outputp, false);
	DefineStreamSet(Concatenated, interactivep);
	DefineStreamSet(Concatenated, characterp);
	DefineStreamSet(Concatenated, binaryp);
	DefineStreamSet(Concatenated, element_type);
	DefineStreamSet(Concatenated, external_format);
	DefineStream___(Concatenated, file_length);
	DefineStreamDef(Concatenated, file_position);
	DefineStreamDef(Concatenated, file_position_start);
	DefineStreamDef(Concatenated, file_position_end);
	DefineStreamDef(Concatenated, file_position_set);
	DefineStream___(Concatenated, file_charlen);
	DefineStream___(Concatenated, file_strlen);
	DefineStreamSet(Concatenated, listen);
	DefineStreamSet(Concatenated, clear_input);
	DefineStream___(Concatenated, finish_output);
	DefineStream___(Concatenated, force_output);
	DefineStream___(Concatenated, clear_output);
	DefineStreamSet(Concatenated, exitpoint);
	DefineStream___(Concatenated, termsize);
}


/************************************************************
 *  stream_default.c
 ************************************************************/

/*
 *  default call
 */
int close_default_stream(addr stream, addr *ret)
{
	return Result(ret, T);
}

int read_char_default_stream(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_char_file_(stream, c, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	return Result(ret, 0);
}

int read_hang_default_stream(addr stream, unicode *c, int *hang, int *ret)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_hang_file_(stream, c, hang, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	*hang = 0;
	return Result(ret, 0);
}

int unread_char_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return fmte_("unread already exists.", NULL);
	ptr->unread = c;
	ptr->unread_check = 1;

	return 0;
}

int write_char_default_stream(addr stream, unicode c)
{
	Return(write_char_file_(stream, c));
	charleft_default_stream(stream, c);
	return 0;
}

int getleft_default_stream(addr stream, size_t *ret)
{
	*ret = PtrStructStream(stream)->terpri;
	return 0;
}

int setleft_default_stream(addr stream, size_t value)
{
	PtrStructStream(stream)->terpri = value;
	return 0;
}

void charleft_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (c == '\n' || c == '\f')
		ptr->terpri = 0;
	else
		ptr->terpri += eastasian_width(c);
}

int file_length_default_stream(addr stream, addr *ret)
{
	int check;
	addr pos;
	size_t size;

	Return(file_length_file_(stream, &size, &check));
	if (check) {
		return Result(ret, Nil);
	}
	else {
		make_index_integer_heap(&pos, size);
		return Result(ret, pos);
	}
}

int file_position_default_stream(addr stream, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

int file_position_start_default_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

int file_position_end_default_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

int file_position_set_default_stream(addr stream, size_t value, int *ret)
{
	return Result(ret, 1);
}

int finish_output_default_stream(addr stream)
{
	return exitpoint_stream_(stream);
}

int force_output_default_stream(addr stream)
{
	return exitpoint_stream_(stream);
}

int clear_output_default_stream(addr stream)
{
	return 0;
}

int exitpoint_default_stream(addr stream)
{
	return 0;
}

int termsize_default_stream(addr stream, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

int checkp_true_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}


/*
 *  default value
 */
int checkp_false_stream(addr stream, int *ret)
{
	return Result(ret, 0);
}

int element_type_character_stream(addr stream, addr *ret)
{
	GetConst(COMMON_CHARACTER, ret);
	return 0;
}

int element_type_io_stream(addr stream, addr *ret)
{
	int check;
	addr input, output;

	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(element_type_stream_(input, &input));
	Return(element_type_stream_(output, &output));
	Return(equal_function_(input, output, &check));
	if (check) {
		return Result(ret, input);
	}
	else {
		GetConst(COMMON_OR, &stream);
		list_heap(ret, stream, input, output, NULL);
		return 0;
	}
}

int external_format_default_stream(addr stream, addr *ret)
{
	GetConst(KEYWORD_DEFAULT, ret);
	return 0;
}


/************************************************************
 *  stream_echo.c
 ************************************************************/

#define CheckEchoStream(stream) { \
	Check(! echo_stream_p(stream), "type error"); \
}

void open_echo_stream(addr *stream, addr input, addr output)
{
	addr pos;

	CheckType(input, LISPTYPE_STREAM);
	CheckType(output, LISPTYPE_STREAM);
	stream_heap(&pos, StreamType_Echo, 0);
	SetInputStream(pos, input);
	SetOutputStream(pos, output);
	force_open_stream(pos);
	*stream = pos;
}

void get_echo_input_stream(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetInputStream(stream, ret);
}

void set_echo_input_stream(addr stream, addr input)
{
	CheckEchoStream(stream);
	SetInputStream(stream, input);
}

void get_echo_output_stream(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetOutputStream(stream, ret);
}

void set_echo_output_stream(addr stream, addr output)
{
	CheckEchoStream(stream);
	SetOutputStream(stream, output);
}

static void input_Echo(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetInputStream(stream, ret);
}

static void output_Echo(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetOutputStream(stream, ret);
}

static void io_Echo(addr stream, addr *input, addr *output)
{
	CheckEchoStream(stream);
	GetInputStream(stream, input);
	GetOutputStream(stream, output);
}

static int read_byte_Echo(addr stream, addr *value, int *ret)
{
	int check;
	addr input, output;

	io_Echo(stream, &input, &output);
	Return(read_byte_stream_(input, value, &check));
	if (check == 0) {
		Return(write_byte_stream_(output, *value));
	}

	return Result(ret, check);
}

static int unread_byte_Echo(addr stream, byte c)
{
	input_Echo(stream, &stream);
	return unread_byte_stream_(stream, c);
}

static int write_byte_Echo(addr stream, addr pos)
{
	output_Echo(stream, &stream);
	return write_byte_stream_(stream, pos);
}

static int read_char_Echo(addr stream, unicode *u, int *ret)
{
	int check;
	addr input, output;
	struct StructStream *ptr;

	io_Echo(stream, &input, &output);
	ptr = PtrStructStream(stream);
	Return(read_char_stream_(input, u, &check));
	if (check == 0 && ptr->unread_check == 0) {
		Return(write_char_stream_(output, *u));
	}
	ptr->unread_check = 0;

	return Result(ret, check);
}

static int read_hang_Echo(addr stream, unicode *u, int *hang, int *ret)
{
	int check, value;
	addr input, output;
	struct StructStream *ptr;

	io_Echo(stream, &input, &output);
	ptr = PtrStructStream(stream);
	Return(read_hang_stream_(input, u, &value, &check));
	if (check == 0) {
		if (value == 0 && ptr->unread_check == 0) {
			Return(write_char_stream_(output, *u));
		}
		*hang = value;
	}
	ptr->unread_check = 0;

	return Result(ret, check);
}

static int unread_char_Echo(addr stream, unicode c)
{
	addr input;

	input_Echo(stream, &input);
	Return(unread_char_stream_(input, c));
	PtrStructStream(stream)->unread_check = 1;

	return 0;
}

static int write_char_Echo(addr stream, unicode u)
{
	output_Echo(stream, &stream);
	return write_char_stream_(stream, u);
}

static int getleft_Echo(addr stream, size_t *ret)
{
	output_Echo(stream, &stream);
	return getleft_stream_(stream, ret);
}

static int setleft_Echo(addr stream, size_t value)
{
	output_Echo(stream, &stream);
	return setleft_stream_(stream, value);
}

static int characterp_Echo(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	io_Echo(stream, &input, &output);
	Return(characterp_stream_(input, &check1));
	Return(characterp_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int binaryp_Echo(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	io_Echo(stream, &input, &output);
	Return(binaryp_stream_(input, &check1));
	Return(binaryp_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int external_format_Echo(addr stream, addr *ret)
{
	addr input;
	input_Echo(stream, &input);
	return external_format_stream_(input, ret);
}

static int file_charlen_Echo(addr stream, unicode u, size_t *value, int *ret)
{
	output_Echo(stream, &stream);
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Echo(addr stream, addr pos, size_t *value, int *ret)
{
	output_Echo(stream, &stream);
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Echo(addr stream, int *ret)
{
	input_Echo(stream, &stream);
	return listen_stream_(stream, ret);
}

static int clear_input_Echo(addr stream)
{
	input_Echo(stream, &stream);
	return clear_input_stream_(stream);
}

static int finish_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return finish_output_stream_(stream);
}

static int force_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return force_output_stream_(stream);
}

static int clear_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return clear_output_stream_(stream);
}

static int exitpoint_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return exitpoint_stream_(stream);
}

static int termsize_Echo(addr stream, size_t *value, int *ret)
{
	output_Echo(stream, &stream);
	return termsize_stream_(stream, value, ret);
}

void init_stream_echo(void)
{
	DefineStreamDef(Echo, close);
	DefineStreamSet(Echo, read_byte);
	DefineStreamSet(Echo, unread_byte);
	DefineStreamSet(Echo, write_byte);
	DefineStreamSet(Echo, read_char);
	DefineStreamSet(Echo, read_hang);
	DefineStreamSet(Echo, unread_char);
	DefineStreamSet(Echo, write_char);
	DefineStreamSet(Echo, getleft);
	DefineStreamSet(Echo, setleft);
	DefineStreamChk(Echo, inputp, true);
	DefineStreamChk(Echo, outputp, true);
	DefineStreamChk(Echo, interactivep, false);
	DefineStreamSet(Echo, characterp);
	DefineStreamSet(Echo, binaryp);
	DefineStreamLet(Echo, element_type, io_stream);
	DefineStreamSet(Echo, external_format);
	DefineStream___(Echo, file_length);
	DefineStreamDef(Echo, file_position);
	DefineStreamDef(Echo, file_position_start);
	DefineStreamDef(Echo, file_position_end);
	DefineStreamDef(Echo, file_position_set);
	DefineStreamSet(Echo, file_charlen);
	DefineStreamSet(Echo, file_strlen);
	DefineStreamSet(Echo, listen);
	DefineStreamSet(Echo, clear_input);
	DefineStreamSet(Echo, finish_output);
	DefineStreamSet(Echo, force_output);
	DefineStreamSet(Echo, clear_output);
	DefineStreamSet(Echo, exitpoint);
	DefineStreamSet(Echo, termsize);
}


/************************************************************
 *  stream_error.c
 ************************************************************/

static int stream_type_error(const char *str, addr stream)
{
	addr type;
	GetTypeTable(&type, Stream);
	return call_type_error_va_(NULL, stream, type, str, stream, NULL);
}

int close_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run close function.", stream);
}

int read_byte_stream_error(addr stream, addr *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run read-byte function.", stream);
}

int unread_byte_stream_error(addr stream, byte c)
{
	return stream_type_error(
			"The stream ~S don't run unread-byte function.", stream);
}

int write_byte_stream_error(addr stream, addr pos)
{
	return stream_type_error(
			"The stream ~S don't run write-byte function.", stream);
}

int read_char_stream_error(addr stream, unicode *c, int *ret)
{
	return stream_type_error(
			"The stream ~S don't run read-char function.", stream);
}

int read_hang_stream_error(addr stream, unicode *c, int *hang, int *ret)
{
	*c = 0;
	*hang = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run read-char-no-hang function.", stream);
}

int unread_char_stream_error(addr stream, unicode c)
{
	return stream_type_error(
			"The stream ~S don't run unread-char function.", stream);
}

int write_char_stream_error(addr stream, unicode c)
{
	return stream_type_error(
			"The stream ~S don't run write-char function.", stream);
}

int getleft_stream_error(addr stream, size_t *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run getleft function.", stream);
}

int setleft_stream_error(addr stream, size_t value)
{
	return stream_type_error(
			"The stream ~S don't run setleft function.", stream);
}

int inputp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run inputp function.", stream);
}

int outputp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run outputp function.", stream);
}

int interactivep_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run interactivep function.", stream);
}

int characterp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run characterp function.", stream);
}

int binaryp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run binaryp function.", stream);
}

int element_type_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run element-type function.", stream);
}

int external_format_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run external_format function.", stream);
}

int file_length_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run file-length function.", stream);
}

int file_position_stream_error(addr stream, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position function.", stream);
}

int file_position_start_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position-start function.", stream);
}

int file_position_end_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position-end function.", stream);
}

int file_position_set_stream_error(addr stream, size_t value, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position-set function.", stream);
}

int file_charlen_stream_error(addr stream, unicode u, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-charlen function.", stream);
}

int file_strlen_stream_error(addr stream, addr pos, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-strlen function.", stream);
}

int listen_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run listen function.", stream);
}

int clear_input_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run clear-input function.", stream);
}

int finish_output_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run finish-output function.", stream);
}

int force_output_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run force-output function.", stream);
}

int clear_output_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run clear-output function.", stream);
}

int exitpoint_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run exitpoint function.", stream);
}

int termsize_stream_error(addr stream, size_t *value, int *ret)
{
	/* return stream_type_error(
	 *    "The stream ~S don't run termsize function.", stream);
	 */
	*value = 0;
	return Result(ret, 1);
}


/************************************************************
 *  stream_file.c
 ************************************************************/

void init_stream_binary_input(void)
{
	DefineStreamLet(BinaryInput, close, stream_file_);
	DefineStreamLet(BinaryInput, read_byte, file_);
	DefineStreamLet(BinaryInput, unread_byte, file_);
	DefineStream___(BinaryInput, write_byte);
	DefineStream___(BinaryInput, read_char);
	DefineStream___(BinaryInput, read_hang);
	DefineStream___(BinaryInput, unread_char);
	DefineStream___(BinaryInput, write_char);
	DefineStream___(BinaryInput, getleft);
	DefineStream___(BinaryInput, setleft);
	DefineStreamChk(BinaryInput, inputp, true);
	DefineStreamChk(BinaryInput, outputp, false);
	DefineStreamChk(BinaryInput, interactivep, false);
	DefineStreamChk(BinaryInput, characterp, false);
	DefineStreamChk(BinaryInput, binaryp, true);
	DefineStreamLet(BinaryInput, element_type, file_);
	DefineStreamLet(BinaryInput, external_format, file_);
	DefineStreamDef(BinaryInput, file_length);
	DefineStreamLet(BinaryInput, file_position, file_);
	DefineStreamLet(BinaryInput, file_position_start, file_);
	DefineStreamLet(BinaryInput, file_position_end, file_);
	DefineStreamLet(BinaryInput, file_position_set, file_);
	DefineStream___(BinaryInput, file_charlen);
	DefineStream___(BinaryInput, file_strlen);
	DefineStreamLet(BinaryInput, listen, file_);
	DefineStreamLet(BinaryInput, clear_input, file_);
	DefineStream___(BinaryInput, finish_output);
	DefineStream___(BinaryInput, force_output);
	DefineStream___(BinaryInput, clear_output);
	DefineStreamDef(BinaryInput, exitpoint);
	DefineStream___(BinaryInput, termsize);
}

void init_stream_binary_output(void)
{
	DefineStreamLet(BinaryOutput, close, stream_file_);
	DefineStream___(BinaryOutput, read_byte);
	DefineStream___(BinaryOutput, unread_byte);
	DefineStreamLet(BinaryOutput, write_byte, file_);
	DefineStream___(BinaryOutput, read_char);
	DefineStream___(BinaryOutput, read_hang);
	DefineStream___(BinaryOutput, unread_char);
	DefineStream___(BinaryOutput, write_char);
	DefineStream___(BinaryOutput, getleft);
	DefineStream___(BinaryOutput, setleft);
	DefineStreamChk(BinaryOutput, inputp, false);
	DefineStreamChk(BinaryOutput, outputp, true);
	DefineStreamChk(BinaryOutput, interactivep, false);
	DefineStreamChk(BinaryOutput, characterp, false);
	DefineStreamChk(BinaryOutput, binaryp, true);
	DefineStreamLet(BinaryOutput, element_type, file_);
	DefineStreamLet(BinaryOutput, external_format, file_);
	DefineStreamDef(BinaryOutput, file_length);
	DefineStreamLet(BinaryOutput, file_position, file_);
	DefineStreamLet(BinaryOutput, file_position_start, file_);
	DefineStreamLet(BinaryOutput, file_position_end, file_);
	DefineStreamLet(BinaryOutput, file_position_set, file_);
	DefineStreamLet(BinaryOutput, file_charlen, file_);
	DefineStreamLet(BinaryOutput, file_strlen, file_);
	DefineStream___(BinaryOutput, listen);
	DefineStream___(BinaryOutput, clear_input);
	DefineStreamLet(BinaryOutput, finish_output, file_);
	DefineStreamLet(BinaryOutput, force_output, file_);
	DefineStreamLet(BinaryOutput, clear_output, file_);
	DefineStreamDef(BinaryOutput, exitpoint);
	DefineStreamDef(BinaryOutput, termsize);
}

void init_stream_binary_io(void)
{
	DefineStreamLet(BinaryIO, close, stream_file_);
	DefineStreamLet(BinaryIO, read_byte, file_);
	DefineStreamLet(BinaryIO, unread_byte, file_);
	DefineStreamLet(BinaryIO, write_byte, file_);
	DefineStream___(BinaryIO, read_char);
	DefineStream___(BinaryIO, read_hang);
	DefineStream___(BinaryIO, unread_char);
	DefineStream___(BinaryIO, write_char);
	DefineStream___(BinaryIO, getleft);
	DefineStream___(BinaryIO, setleft);
	DefineStreamChk(BinaryIO, inputp, true);
	DefineStreamChk(BinaryIO, outputp, true);
	DefineStreamChk(BinaryIO, interactivep, false);
	DefineStreamChk(BinaryIO, characterp, false);
	DefineStreamChk(BinaryIO, binaryp, true);
	DefineStreamLet(BinaryIO, element_type, file_);
	DefineStreamLet(BinaryIO, external_format, file_);
	DefineStreamDef(BinaryIO, file_length);
	DefineStreamLet(BinaryIO, file_position, file_);
	DefineStreamLet(BinaryIO, file_position_start, file_);
	DefineStreamLet(BinaryIO, file_position_end, file_);
	DefineStreamLet(BinaryIO, file_position_set, file_);
	DefineStreamLet(BinaryIO, file_charlen, file_);
	DefineStreamLet(BinaryIO, file_strlen, file_);
	DefineStreamLet(BinaryIO, listen, file_);
	DefineStreamLet(BinaryIO, clear_input, file_);
	DefineStreamLet(BinaryIO, finish_output, file_);
	DefineStreamLet(BinaryIO, force_output, file_);
	DefineStreamLet(BinaryIO, clear_output, file_);
	DefineStreamDef(BinaryIO, exitpoint);
	DefineStreamDef(BinaryIO, termsize);
}

void init_stream_character_input(void)
{
	DefineStreamLet(CharacterInput, close, stream_file_);
	DefineStream___(CharacterInput, read_byte);
	DefineStream___(CharacterInput, unread_byte);
	DefineStream___(CharacterInput, write_byte);
	DefineStreamDef(CharacterInput, read_char);
	DefineStreamDef(CharacterInput, read_hang);
	DefineStreamDef(CharacterInput, unread_char);
	DefineStream___(CharacterInput, write_char);
	DefineStream___(CharacterInput, getleft);
	DefineStream___(CharacterInput, setleft);
	DefineStreamChk(CharacterInput, inputp, true);
	DefineStreamChk(CharacterInput, outputp, false);
	DefineStreamChk(CharacterInput, interactivep, false);
	DefineStreamChk(CharacterInput, characterp, true);
	DefineStreamChk(CharacterInput, binaryp, false);
	DefineStreamLet(CharacterInput, element_type, character_stream);
	DefineStreamLet(CharacterInput, external_format, file_);
	DefineStreamDef(CharacterInput, file_length);
	DefineStreamLet(CharacterInput, file_position, file_);
	DefineStreamLet(CharacterInput, file_position_start, file_);
	DefineStreamLet(CharacterInput, file_position_end, file_);
	DefineStreamLet(CharacterInput, file_position_set, file_);
	DefineStream___(CharacterInput, file_charlen);
	DefineStream___(CharacterInput, file_strlen);
	DefineStreamLet(CharacterInput, listen, file_);
	DefineStreamLet(CharacterInput, clear_input, file_);
	DefineStream___(CharacterInput, finish_output);
	DefineStream___(CharacterInput, force_output);
	DefineStream___(CharacterInput, clear_output);
	DefineStreamDef(CharacterInput, exitpoint);
	DefineStream___(CharacterInput, termsize);
}

void init_stream_character_output(void)
{
	DefineStreamLet(CharacterOutput, close, stream_file_);
	DefineStream___(CharacterOutput, read_byte);
	DefineStream___(CharacterOutput, unread_byte);
	DefineStream___(CharacterOutput, write_byte);
	DefineStream___(CharacterOutput, read_char);
	DefineStream___(CharacterOutput, read_hang);
	DefineStream___(CharacterOutput, unread_char);
	DefineStreamDef(CharacterOutput, write_char);
	DefineStreamDef(CharacterOutput, getleft);
	DefineStreamDef(CharacterOutput, setleft);
	DefineStreamChk(CharacterOutput, inputp, false);
	DefineStreamChk(CharacterOutput, outputp, true);
	DefineStreamChk(CharacterOutput, interactivep, false);
	DefineStreamChk(CharacterOutput, characterp, true);
	DefineStreamChk(CharacterOutput, binaryp, false);
	DefineStreamLet(CharacterOutput, element_type, character_stream);
	DefineStreamLet(CharacterOutput, external_format, file_);
	DefineStreamDef(CharacterOutput, file_length);
	DefineStreamLet(CharacterOutput, file_position, file_);
	DefineStreamLet(CharacterOutput, file_position_start, file_);
	DefineStreamLet(CharacterOutput, file_position_end, file_);
	DefineStreamLet(CharacterOutput, file_position_set, file_);
	DefineStreamLet(CharacterOutput, file_charlen, file_);
	DefineStreamLet(CharacterOutput, file_strlen, file_);
	DefineStream___(CharacterOutput, listen);
	DefineStream___(CharacterOutput, clear_input);
	DefineStreamLet(CharacterOutput, finish_output, file_);
	DefineStreamLet(CharacterOutput, force_output, file_);
	DefineStreamLet(CharacterOutput, clear_output, file_);
	DefineStreamDef(CharacterOutput, exitpoint);
	DefineStreamDef(CharacterOutput, termsize);
}

void init_stream_character_io(void)
{
	DefineStreamLet(CharacterIO, close, stream_file_);
	DefineStream___(CharacterIO, read_byte);
	DefineStream___(CharacterIO, unread_byte);
	DefineStream___(CharacterIO, write_byte);
	DefineStreamDef(CharacterIO, read_char);
	DefineStreamDef(CharacterIO, read_hang);
	DefineStreamDef(CharacterIO, unread_char);
	DefineStreamDef(CharacterIO, write_char);
	DefineStreamDef(CharacterIO, getleft);
	DefineStreamDef(CharacterIO, setleft);
	DefineStreamChk(CharacterIO, inputp, true);
	DefineStreamChk(CharacterIO, outputp, true);
	DefineStreamChk(CharacterIO, interactivep, false);
	DefineStreamChk(CharacterIO, characterp, true);
	DefineStreamChk(CharacterIO, binaryp, false);
	DefineStreamLet(CharacterIO, element_type, character_stream);
	DefineStreamLet(CharacterIO, external_format, file_);
	DefineStreamDef(CharacterIO, file_length);
	DefineStreamLet(CharacterIO, file_position, file_);
	DefineStreamLet(CharacterIO, file_position_start, file_);
	DefineStreamLet(CharacterIO, file_position_end, file_);
	DefineStreamLet(CharacterIO, file_position_set, file_);
	DefineStreamLet(CharacterIO, file_charlen, file_);
	DefineStreamLet(CharacterIO, file_strlen, file_);
	DefineStreamLet(CharacterIO, listen, file_);
	DefineStreamLet(CharacterIO, clear_input, file_);
	DefineStreamLet(CharacterIO, finish_output, file_);
	DefineStreamLet(CharacterIO, force_output, file_);
	DefineStreamLet(CharacterIO, clear_output, file_);
	DefineStreamDef(CharacterIO, exitpoint);
	DefineStreamDef(CharacterIO, termsize);
}

void init_stream_binchar_input(void)
{
	DefineStreamLet(BincharInput, close, stream_file_);
	DefineStreamLet(BincharInput, read_byte, file_);
	DefineStreamLet(BincharInput, unread_byte, file_);
	DefineStream___(BincharInput, write_byte);
	DefineStreamDef(BincharInput, read_char);
	DefineStreamDef(BincharInput, read_hang);
	DefineStreamDef(BincharInput, unread_char);
	DefineStream___(BincharInput, write_char);
	DefineStream___(BincharInput, getleft);
	DefineStream___(BincharInput, setleft);
	DefineStreamChk(BincharInput, inputp, true);
	DefineStreamChk(BincharInput, outputp, false);
	DefineStreamChk(BincharInput, interactivep, true);
	DefineStreamChk(BincharInput, characterp, true);
	DefineStreamChk(BincharInput, binaryp, true);
	DefineStreamLet(BincharInput, element_type, character_stream);
	DefineStreamLet(BincharInput, external_format, file_);
	DefineStream___(BincharInput, file_length);
	DefineStreamLet(BincharInput, file_position, file_);
	DefineStreamLet(BincharInput, file_position_start, file_);
	DefineStreamLet(BincharInput, file_position_end, file_);
	DefineStreamLet(BincharInput, file_position_set, file_);
	DefineStream___(BincharInput, file_charlen);
	DefineStream___(BincharInput, file_strlen);
	DefineStreamLet(BincharInput, listen, file_);
	DefineStreamLet(BincharInput, clear_input, file_);
	DefineStream___(BincharInput, finish_output);
	DefineStream___(BincharInput, force_output);
	DefineStream___(BincharInput, clear_output);
	DefineStreamLet(BincharInput, exitpoint, file_);
	DefineStream___(BincharInput, termsize);
}

void init_stream_binchar_output(void)
{
	DefineStreamLet(BincharOutput, close, stream_file_);
	DefineStream___(BincharOutput, read_byte);
	DefineStream___(BincharOutput, unread_byte);
	DefineStreamLet(BincharOutput, write_byte, file_);
	DefineStream___(BincharOutput, read_char);
	DefineStream___(BincharOutput, read_hang);
	DefineStream___(BincharOutput, unread_char);
	DefineStreamDef(BincharOutput, write_char);
	DefineStreamDef(BincharOutput, getleft);
	DefineStreamDef(BincharOutput, setleft);
	DefineStreamChk(BincharOutput, inputp, false);
	DefineStreamChk(BincharOutput, outputp, true);
	DefineStreamChk(BincharOutput, interactivep, true);
	DefineStreamChk(BincharOutput, characterp, true);
	DefineStreamChk(BincharOutput, binaryp, true);
	DefineStreamLet(BincharOutput, element_type, character_stream);
	DefineStreamLet(BincharOutput, external_format, file_);
	DefineStream___(BincharOutput, file_length);
	DefineStreamLet(BincharOutput, file_position, file_);
	DefineStreamLet(BincharOutput, file_position_start, file_);
	DefineStreamLet(BincharOutput, file_position_end, file_);
	DefineStreamLet(BincharOutput, file_position_set, file_);
	DefineStreamLet(BincharOutput, file_charlen, file_);
	DefineStreamLet(BincharOutput, file_strlen, file_);
	DefineStream___(BincharOutput, listen);
	DefineStream___(BincharOutput, clear_input);
	DefineStreamLet(BincharOutput, finish_output, file_);
	DefineStreamLet(BincharOutput, force_output, file_);
	DefineStreamLet(BincharOutput, clear_output, file_);
	DefineStreamLet(BincharOutput, exitpoint, file_);
	DefineStreamLet(BincharOutput, termsize, file_);
}

void init_stream_binchar_io(void)
{
	DefineStreamLet(BincharIO, close, stream_file_);
	DefineStreamLet(BincharIO, read_byte, file_);
	DefineStreamLet(BincharIO, unread_byte, file_);
	DefineStreamLet(BincharIO, write_byte, file_);
	DefineStreamDef(BincharIO, read_char);
	DefineStreamDef(BincharIO, read_hang);
	DefineStreamDef(BincharIO, unread_char);
	DefineStreamDef(BincharIO, write_char);
	DefineStreamDef(BincharIO, getleft);
	DefineStreamDef(BincharIO, setleft);
	DefineStreamChk(BincharIO, inputp, true);
	DefineStreamChk(BincharIO, outputp, true);
	DefineStreamChk(BincharIO, interactivep, true);
	DefineStreamChk(BincharIO, characterp, true);
	DefineStreamChk(BincharIO, binaryp, true);
	DefineStreamLet(BincharIO, element_type, character_stream);
	DefineStreamLet(BincharIO, external_format, file_);
	DefineStream___(BincharIO, file_length);
	DefineStreamLet(BincharIO, file_position, file_);
	DefineStreamLet(BincharIO, file_position_start, file_);
	DefineStreamLet(BincharIO, file_position_end, file_);
	DefineStreamLet(BincharIO, file_position_set, file_);
	DefineStreamLet(BincharIO, file_charlen, file_);
	DefineStreamLet(BincharIO, file_strlen, file_);
	DefineStreamLet(BincharIO, listen, file_);
	DefineStreamLet(BincharIO, clear_input, file_);
	DefineStreamLet(BincharIO, finish_output, file_);
	DefineStreamLet(BincharIO, force_output, file_);
	DefineStreamLet(BincharIO, clear_output, file_);
	DefineStreamLet(BincharIO, exitpoint, file_);
	DefineStreamLet(BincharIO, termsize, file_);
}

void init_stream_probe(void)
{
	DefineStreamLet(Probe, close, stream_file_);
	DefineStream___(Probe, read_byte);
	DefineStream___(Probe, unread_byte);
	DefineStream___(Probe, write_byte);
	DefineStream___(Probe, read_char);
	DefineStream___(Probe, read_hang);
	DefineStream___(Probe, unread_char);
	DefineStream___(Probe, write_char);
	DefineStream___(Probe, getleft);
	DefineStream___(Probe, setleft);
	DefineStreamChk(Probe, inputp, false);
	DefineStreamChk(Probe, outputp, false);
	DefineStreamChk(Probe, interactivep, false);
	DefineStreamChk(Probe, characterp, true);
	DefineStreamChk(Probe, binaryp, false);
	DefineStreamLet(Probe, element_type, character_stream);
	DefineStreamLet(Probe, external_format, file_);
	DefineStream___(Probe, file_length);
	DefineStream___(Probe, file_position);
	DefineStream___(Probe, file_position_start);
	DefineStream___(Probe, file_position_end);
	DefineStream___(Probe, file_position_set);
	DefineStream___(Probe, file_charlen);
	DefineStream___(Probe, file_strlen);
	DefineStream___(Probe, listen);
	DefineStream___(Probe, clear_input);
	DefineStream___(Probe, finish_output);
	DefineStream___(Probe, force_output);
	DefineStream___(Probe, clear_output);
	DefineStream___(Probe, exitpoint);
	DefineStream___(Probe, termsize);
}


/************************************************************
 *  stream_function.c
 ************************************************************/

#define CheckStream(stream, ptr) { \
	CheckType(stream, LISPTYPE_STREAM); \
	ptr = PtrStructStream(stream); \
	if (ptr->closed) { \
		return fmte_("The stream ~S is already closed.", stream, NULL); \
	} \
}

int close_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	addr pos;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	Return((Stream_close[ptr->type])(stream, &pos));
	force_close_stream(stream);
	if (ret)
		*ret = pos;

	return 0;
}

int read_byte_stream_(addr stream, addr *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_byte[(int)ptr->type])(stream, value, ret);
}

int unread_byte_stream_(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_byte[(int)ptr->type])(stream, c);
}

int write_byte_stream_(addr stream, addr pos)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_byte[(int)ptr->type])(stream, pos);
}

int read_char_stream_(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_char[(int)ptr->type])(stream, c, ret);
}

int read_hang_stream_(addr stream, unicode *c, int *hang, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_hang[(int)ptr->type])(stream, c, hang, ret);
}

int unread_char_stream_(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_char[(int)ptr->type])(stream, c);
}

int write_char_stream_(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_char[(int)ptr->type])(stream, c);
}

int getleft_stream_(addr stream, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_getleft[(int)ptr->type])(stream, ret);
}

int setleft_stream_(addr stream, size_t value)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_setleft[(int)ptr->type])(stream, value);
}

int clear_input_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_clear_input[(int)ptr->type])(stream);
}

int inputp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_inputp[GetIndexStream(stream)])(stream, ret);
}

int outputp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_outputp[GetIndexStream(stream)])(stream, ret);
}

int interactivep_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_interactivep[GetIndexStream(stream)])(stream, ret);
}

int characterp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_characterp[GetIndexStream(stream)])(stream, ret);
}

int binaryp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_binaryp[GetIndexStream(stream)])(stream, ret);
}

int element_type_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	return (Stream_element_type[(int)ptr->type])(stream, ret);
}

int external_format_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	return (Stream_external_format[(int)ptr->type])(stream, ret);
}

int file_length_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_length[(int)ptr->type])(stream, ret);
}

int file_position_stream_(addr stream, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position[(int)ptr->type])(stream, value, ret);
}

int file_position_start_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_start[(int)ptr->type])(stream, ret);
}

int file_position_end_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_end[(int)ptr->type])(stream, ret);
}

int file_position_set_stream_(addr stream, size_t value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_set[(int)ptr->type])(stream, value, ret);
}

int file_charlen_stream_(addr stream, unicode u, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_charlen[(int)ptr->type])(stream, u, value, ret);
}

int file_strlen_stream_(addr stream, addr pos, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_strlen[(int)ptr->type])(stream, pos, value, ret);
}

int listen_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_listen[(int)ptr->type])(stream, ret);
}

int finish_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_finish_output[(int)ptr->type])(stream);
}

int force_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_force_output[(int)ptr->type])(stream);
}

int clear_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_clear_output[(int)ptr->type])(stream);
}

int exitpoint_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_exitpoint[(int)ptr->type])(stream);
}

int termsize_stream_(addr stream, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_termsize[(int)ptr->type])(stream, value, ret);
}


/************************************************************
 *  stream_init.c
 ************************************************************/

/*
 *  initialize
 */
#define LispStreamTypeError(x, type) Stream_##x[type] = x##_stream_error

static void init_stream_extend_type(int type)
{
	LispStreamTypeError(close, type);
	LispStreamTypeError(read_byte, type);
	LispStreamTypeError(unread_byte, type);
	LispStreamTypeError(write_byte, type);
	LispStreamTypeError(read_char, type);
	LispStreamTypeError(read_hang, type);
	LispStreamTypeError(unread_char, type);
	LispStreamTypeError(write_char, type);
	LispStreamTypeError(getleft, type);
	LispStreamTypeError(setleft, type);
	LispStreamTypeError(inputp, type);
	LispStreamTypeError(outputp, type);
	LispStreamTypeError(interactivep, type);
	LispStreamTypeError(characterp, type);
	LispStreamTypeError(binaryp, type);
	LispStreamTypeError(element_type, type);
	LispStreamTypeError(external_format, type);
	LispStreamTypeError(file_length, type);
	LispStreamTypeError(file_position, type);
	LispStreamTypeError(file_position_start, type);
	LispStreamTypeError(file_position_end, type);
	LispStreamTypeError(file_position_set, type);
	LispStreamTypeError(file_charlen, type);
	LispStreamTypeError(file_strlen, type);
	LispStreamTypeError(listen, type);
	LispStreamTypeError(clear_input, type);
	LispStreamTypeError(finish_output, type);
	LispStreamTypeError(force_output, type);
	LispStreamTypeError(clear_output, type);
	LispStreamTypeError(exitpoint, type);
	LispStreamTypeError(termsize, type);
}

static void init_stream_extend(void)
{
	int i;

	for (i = 0; i < LISP_STREAM_EXTEND; i++)
		init_stream_extend_type(((int)StreamType_Size) + i);
}

void init_stream(void)
{
	init_stream_binary_input();
	init_stream_binary_output();
	init_stream_binary_io();
	init_stream_character_input();
	init_stream_character_output();
	init_stream_character_io();
	init_stream_binchar_input();
	init_stream_binchar_output();
	init_stream_binchar_io();
	init_stream_probe();
	init_stream_string_input();
	init_stream_string_output();
	init_stream_synonym();
	init_stream_broadcast();
	init_stream_concatenated();
	init_stream_twoway();
	init_stream_echo();
	init_stream_prompt();
	init_stream_pretty();
	init_stream_extend();
	init_stream_memory_input();
	init_stream_memory_output();
	init_stream_memory_io();
}


/*
 *  build
 */
static void defvar_external_format(void)
{
	addr symbol, value;

	GetConst(SYSTEM_EXTERNAL_FORMAT, &symbol);
	GetConst(SYSTEM_UTF_8, &value);
	SetValueSymbol(symbol, value);
}

static void defvar_system_standard_input(void)
{
	addr stream, symbol;

	make_standard_input(&stream);
	GetConst(SYSTEM_STANDARD_INPUT, &symbol);
	SetValueSymbol(symbol, stream);
	SetConst(STREAM_STDIN, stream);
}

static void defvar_system_standard_output(void)
{
	addr stream, symbol;

	make_standard_output(&stream);
	GetConst(SYSTEM_STANDARD_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
	SetConst(STREAM_STDOUT, stream);
}

static void defvar_system_standard_error(void)
{
	addr stream, symbol;

	make_standard_error(&stream);
	GetConst(SYSTEM_STANDARD_ERROR, &symbol);
	SetValueSymbol(symbol, stream);
	SetConst(STREAM_STDERR, stream);
}

static void defvar_system_prompt(void)
{
	addr stream, symbol;

	open_prompt_stream(&stream);
	GetConst(SYSTEM_PROMPT_VALUE, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_standard_input(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SYSTEM_STANDARD_INPUT, &stream);
	Error(open_synonym_stream_(&stream, stream));
	/* defvar */
	GetConst(SPECIAL_STANDARD_INPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_standard_output(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SYSTEM_STANDARD_OUTPUT, &stream);
	Error(open_synonym_stream_(&stream, stream));
	/* defvar */
	GetConst(SPECIAL_STANDARD_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_error_output(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SYSTEM_STANDARD_ERROR, &stream);
	Error(open_synonym_stream_(&stream, stream));
	/* defvar */
	GetConst(SPECIAL_ERROR_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_terminal_io(void)
{
	addr symbol, input, output, stream;

	/* twoway */
	GetConst(SYSTEM_PROMPT_VALUE, &input);
	Error(open_synonym_stream_(&input, input));
	GetConst(SPECIAL_STANDARD_OUTPUT, &output);
	Error(open_synonym_stream_(&output, output));
	open_twoway_stream(&stream, input, output);
	/* defvar */
	GetConst(SPECIAL_TERMINAL_IO, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_trace_output(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SPECIAL_TERMINAL_IO, &stream);
	Error(open_synonym_stream_(&stream, stream));
	/* defvar */
	GetConst(SPECIAL_TRACE_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_debug_io(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SPECIAL_TERMINAL_IO, &stream);
	Error(open_synonym_stream_(&stream, stream));
	/* defvar */
	GetConst(SPECIAL_DEBUG_IO, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_query_io(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SPECIAL_TERMINAL_IO, &stream);
	Error(open_synonym_stream_(&stream, stream));
	/* defvar */
	GetConst(SPECIAL_QUERY_IO, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_end_of_line(void)
{
	addr symbol, value;

	GetConst(SYSTEM_END_OF_LINE, &symbol);
	GetConst(SYSTEM_AUTO, &value);
	SetValueSymbol(symbol, value);
}

void build_stream(void)
{
	defvar_external_format();
	defvar_system_standard_input();
	defvar_system_standard_output();
	defvar_system_standard_error();
	defvar_standard_input();
	defvar_standard_output();
	defvar_system_prompt();
	defvar_error_output();
	defvar_terminal_io();
	defvar_trace_output();
	defvar_debug_io();
	defvar_query_io();
	defvar_end_of_line();
}


/************************************************************
 *  stream_memory.c
 ************************************************************/

#define CheckMemoryStream(stream) { \
	Check(! memory_stream_p(stream), "type error"); \
}

static int close_memory_stream_p(addr stream, addr *ret)
{
	addr pos;

	CheckMemoryStream(stream);
	if (open_stream_p(stream)) {
		GetInfoStream(stream, &pos);
		if (ret)
			*ret = pos;
		return 0;
	}

	/* close */
	if (ret)
		*ret = Nil;
	return 0;
}

static int close_memory_stream_error_(addr stream, addr *ret)
{
	CheckMemoryStream(stream);
	if (close_memory_stream_p(stream, ret))
		return fmte_("The stream is already closed.", stream, NULL);

	return 0;
}


/*****************************************************************************
 *  MemoryStream
 *****************************************************************************/
struct stream_MemoryStream {
	unsigned cache : 1;
	unsigned unread_index;
	byte unread[INPUT_MEMORY_UNREAD_SIZE];
};
#define PtrMemoryStream(pos) ((struct stream_MemoryStream *)PtrDataStream(pos))

static int open_memory_stream_(addr *ret,
		enum StreamType type, addr input, size_t cell, size_t array, int cache)
{
	struct stream_MemoryStream *str;
	addr pos, file;

	if (! sequencep(input)) {
		*ret = Nil;
		return TypeError_(input, SEQUENCE);
	}

	/* object */
	stream_heap(&pos, type, sizeoft(struct stream_MemoryStream));
	str = PtrMemoryStream(pos);
	str->unread_index = 0;
	str->cache = (cache != 0);

	/* buffering */
	buffering_heap(&file, cell, array);
	SetInfoStream(pos, file);
	Return(read_buffering_(file, input));
	force_open_stream(pos);

	return Result(ret, pos);
}

static void clear_unread_io_memory_stream(addr stream)
{
	struct stream_MemoryStream *str;

	CheckMemoryStream(stream);
	str = PtrMemoryStream(stream);
	str->unread_index = 0;
}

int memory_stream_heap_(addr stream, addr *ret)
{
	addr page;

	CheckMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &page));
	return make_vector_buffering_heap_(page, ret);
}

int clear_memory_stream_(addr stream)
{
	addr page;

	CheckMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &page));
	clear_unread_io_memory_stream(stream);
	clear_buffering(page);

	return 0;
}

void getsize_memory_stream(addr stream, size_t *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	getcell_buffering(page, ret);
}

void getarray_memory_stream(addr stream, size_t *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	getwidth_buffering(page, ret);
}

int getcache_memory_stream(addr stream)
{
	struct stream_MemoryStream *str;

	CheckMemoryStream(stream);
	str = PtrMemoryStream(stream);
	return str->cache;
}

void gettype_memory_stream(addr stream, addr *ret)
{
	if (! streamp(stream)) {
		*ret = Nil;
		return;
	}

	switch (getstreamtype(stream)) {
		case StreamType_MemoryInput:
			GetConst(KEYWORD_INPUT, ret);
			break;

		case StreamType_MemoryOutput:
			GetConst(KEYWORD_OUTPUT, ret);
			break;

		case StreamType_MemoryIO:
			GetConst(KEYWORD_IO, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}

int settype_memory_stream_(addr stream, addr value)
{
	addr check;
	struct StructStream *str;

	CheckType(stream, LISPTYPE_STREAM);
	str = PtrStructStream(stream);

	/* input */
	GetConst(KEYWORD_INPUT, &check);
	if (value == check) {
		str->type = StreamType_MemoryInput;
		return 0;
	}

	/* output */
	GetConst(KEYWORD_OUTPUT, &check);
	if (value == check) {
		str->type = StreamType_MemoryOutput;
		return 0;
	}

	/* io */
	GetConst(KEYWORD_IO, &check);
	if (value == check) {
		str->type = StreamType_MemoryIO;
		return 0;
	}

	/* error */
	return fmte_("Invalid memory-stream type ~S.", value, NULL);
}


/*
 *  stream function
 */
static int close_MemoryStream(addr stream, addr *ret)
{
	CheckMemoryStream(stream);
	force_close_stream(stream);
	return Result(ret, T);
}

static int read_byte_MemoryStream(addr stream, addr *value, int *ret)
{
	byte c;
	addr page;
	struct stream_MemoryStream *str;
	size_t index;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);

	/* unread */
	str = PtrMemoryStream(stream);
	position_get_buffering(page, &index);
	if (str->unread_index) {
		str->unread_index--;
		fixnum_heap(value, (fixnum)str->unread[str->unread_index]);
		position_set_buffering(page, index + 1UL);
		return Result(ret, 0);
	}

	/* read */
	if (getc_buffering(page, &c)) {
		*value = Nil;
		return Result(ret, 1);  /* EOF */
	}

	fixnum_heap(value, (fixnum)c);
	return Result(ret, 0);
}

static int unread_byte_MemoryStream(addr stream, byte c)
{
	struct stream_MemoryStream *str;
	addr page;
	size_t index;

	CheckMemoryStream(stream);
	str = PtrMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_get_buffering(page, &index);

	/* unread check */
	if (INPUT_MEMORY_UNREAD_SIZE <= str->unread_index)
		return fmte_("The unread buffer is overflow.", NULL);

	/* index check */
	if (index == 0)
		return fmte_("The memory-stream index is underflow.", NULL);

	str->unread[str->unread_index] = c;
	str->unread_index++;
	position_set_buffering(page, index - 1UL);

	return 0;
}

static int write_byte_MemoryStream(addr stream, addr pos)
{
	addr page;
	fixnum v;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	Return(getfixnum_signed_(pos, &v));
	if (! IsByteSign(v))
		return fmte_("The argument ~S must be a (unsigned-byte 8) type.", pos, NULL);
	if (putc_buffering(page, (byte)v))
		return fmte_("Too large file size.", NULL);
	clear_unread_io_memory_stream(stream);

	return 0;
}

static int element_type_MemoryStream(addr stream, addr *ret)
{
	addr x, y;

	CheckMemoryStream(stream);
	/* (unsigned-byte 8) */
	GetConst(COMMON_UNSIGNED_BYTE, &x);
	fixnum_heap(&y, 8);
	list_heap(ret, x, y, NULL);

	return 0;
}

static int file_length_MemoryStream(addr stream, addr *ret)
{
	addr page;
	size_t size;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	length_buffering(page, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

static int file_position_MemoryStream(addr stream, size_t *value, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_get_buffering(page, value);

	return Result(ret, 0);
}

static int file_position_start_MemoryStream(addr stream, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_start_buffering(page);
	clear_unread_io_memory_stream(stream);

	return Result(ret, 0);
}

static int file_position_end_MemoryStream(addr stream, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_end_buffering(page);
	clear_unread_io_memory_stream(stream);

	return Result(ret, 0);
}

static int file_position_set_MemoryStream(addr stream, size_t value, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_set_buffering(page, value);
	clear_unread_io_memory_stream(stream);

	return Result(ret, 0);
}

static int listen_MemoryStream(addr stream, int *ret)
{
	int check;
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	check = end_buffering(page);

	return Result(ret, ! check);
}

static int clear_input_MemoryStream(addr stream)
{
	CheckMemoryStream(stream);
	return 0;
}


/*****************************************************************************
 *  MemoryInput
 *****************************************************************************/
int open_input_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache)
{
	int ignore;
	addr pos;

	pos = Nil;
	Return(open_memory_stream_(&pos, StreamType_MemoryInput, input,
				cell, array, cache));
	Return(file_position_start_MemoryStream(pos, &ignore));

	return Result(ret, pos);
}

void init_stream_memory_input(void)
{
	DefineStreamLet(MemoryInput, close, MemoryStream);
	DefineStreamLet(MemoryInput, read_byte, MemoryStream);
	DefineStreamLet(MemoryInput, unread_byte, MemoryStream);
	DefineStream___(MemoryInput, write_byte);
	DefineStream___(MemoryInput, read_char);
	DefineStream___(MemoryInput, read_hang);
	DefineStream___(MemoryInput, unread_char);
	DefineStream___(MemoryInput, write_char);
	DefineStream___(MemoryInput, getleft);
	DefineStream___(MemoryInput, setleft);
	DefineStreamChk(MemoryInput, inputp, true);
	DefineStreamChk(MemoryInput, outputp, false);
	DefineStreamChk(MemoryInput, interactivep, false);
	DefineStreamChk(MemoryInput, characterp, false);
	DefineStreamChk(MemoryInput, binaryp, true);
	DefineStreamLet(MemoryInput, element_type, MemoryStream);
	DefineStreamDef(MemoryInput, external_format);
	DefineStreamLet(MemoryInput, file_length, MemoryStream);
	DefineStreamLet(MemoryInput, file_position, MemoryStream);
	DefineStreamLet(MemoryInput, file_position_start, MemoryStream);
	DefineStreamLet(MemoryInput, file_position_end, MemoryStream);
	DefineStreamLet(MemoryInput, file_position_set, MemoryStream);
	DefineStream___(MemoryInput, file_charlen);
	DefineStream___(MemoryInput, file_strlen);
	DefineStreamLet(MemoryInput, listen, MemoryStream);
	DefineStreamLet(MemoryInput, clear_input, MemoryStream);
	DefineStream___(MemoryInput, finish_output);
	DefineStream___(MemoryInput, force_output);
	DefineStream___(MemoryInput, clear_output);
	DefineStreamDef(MemoryInput, exitpoint);
	DefineStreamDef(MemoryInput, termsize);
}


/*****************************************************************************
 *  MemoryOutput
 *****************************************************************************/
int open_output_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache)
{
	return open_memory_stream_(ret, StreamType_MemoryOutput,
			input, cell, array, cache);
}

void init_stream_memory_output(void)
{
	DefineStreamLet(MemoryOutput, close, MemoryStream);
	DefineStream___(MemoryOutput, read_byte);
	DefineStream___(MemoryOutput, unread_byte);
	DefineStreamLet(MemoryOutput, write_byte, MemoryStream);
	DefineStream___(MemoryOutput, read_char);
	DefineStream___(MemoryOutput, read_hang);
	DefineStream___(MemoryOutput, unread_char);
	DefineStream___(MemoryOutput, write_char);
	DefineStream___(MemoryOutput, getleft);
	DefineStream___(MemoryOutput, setleft);
	DefineStreamChk(MemoryOutput, inputp, false);
	DefineStreamChk(MemoryOutput, outputp, true);
	DefineStreamChk(MemoryOutput, interactivep, false);
	DefineStreamChk(MemoryOutput, characterp, false);
	DefineStreamChk(MemoryOutput, binaryp, true);
	DefineStreamLet(MemoryOutput, element_type, MemoryStream);
	DefineStreamDef(MemoryOutput, external_format);
	DefineStreamLet(MemoryOutput, file_length, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position_start, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position_end, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position_set, MemoryStream);
	DefineStream___(MemoryOutput, file_charlen);
	DefineStream___(MemoryOutput, file_strlen);
	DefineStream___(MemoryOutput, listen);
	DefineStream___(MemoryOutput, clear_input);
	DefineStreamDef(MemoryOutput, finish_output);
	DefineStreamDef(MemoryOutput, force_output);
	DefineStreamDef(MemoryOutput, clear_output);
	DefineStreamDef(MemoryOutput, exitpoint);
	DefineStreamDef(MemoryOutput, termsize);
}


/*****************************************************************************
 *  MemoryIO
 *****************************************************************************/
int open_io_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache)
{
	return open_memory_stream_(ret, StreamType_MemoryIO,
			input, cell, array, cache);
}

void init_stream_memory_io(void)
{
	DefineStreamLet(MemoryIO, close, MemoryStream);
	DefineStreamLet(MemoryIO, read_byte, MemoryStream);
	DefineStreamLet(MemoryIO, unread_byte, MemoryStream);
	DefineStreamLet(MemoryIO, write_byte, MemoryStream);
	DefineStream___(MemoryIO, read_char);
	DefineStream___(MemoryIO, read_hang);
	DefineStream___(MemoryIO, unread_char);
	DefineStream___(MemoryIO, write_char);
	DefineStream___(MemoryIO, getleft);
	DefineStream___(MemoryIO, setleft);
	DefineStreamChk(MemoryIO, inputp, true);
	DefineStreamChk(MemoryIO, outputp, true);
	DefineStreamChk(MemoryIO, interactivep, false);
	DefineStreamChk(MemoryIO, characterp, false);
	DefineStreamChk(MemoryIO, binaryp, true);
	DefineStreamLet(MemoryIO, element_type, MemoryStream);
	DefineStreamDef(MemoryIO, external_format);
	DefineStreamLet(MemoryIO, file_length, MemoryStream);
	DefineStreamLet(MemoryIO, file_position, MemoryStream);
	DefineStreamLet(MemoryIO, file_position_start, MemoryStream);
	DefineStreamLet(MemoryIO, file_position_end, MemoryStream);
	DefineStreamLet(MemoryIO, file_position_set, MemoryStream);
	DefineStream___(MemoryIO, file_charlen);
	DefineStream___(MemoryIO, file_strlen);
	DefineStreamLet(MemoryIO, listen, MemoryStream);
	DefineStreamLet(MemoryIO, clear_input, MemoryStream);
	DefineStreamDef(MemoryIO, finish_output);
	DefineStreamDef(MemoryIO, force_output);
	DefineStreamDef(MemoryIO, clear_output);
	DefineStreamDef(MemoryIO, exitpoint);
	DefineStreamDef(MemoryIO, termsize);
}


/*****************************************************************************
 *  file-buffering
 *****************************************************************************/
/* read-byte */
static int read_byte_read_memory(addr stream, byte *value, int *ret)
{
	struct stream_MemoryStream *str;
	addr page;
	size_t index;

	CheckMemoryStream(stream);
	if (close_memory_stream_p(stream, &page)) {
		*value = 0;
		*ret = 0;
		return 1;  /* already closed */
	}

	/* unread */
	str = PtrMemoryStream(stream);
	position_get_buffering(page, &index);
	if (str->unread_index) {
		str->unread_index--;
		*value = str->unread[str->unread_index];
		position_set_buffering(page, index + 1UL);
		*ret = 0;
		return 0;
	}

	/* read */
	if (getc_buffering(page, value)) {
		*value = 0;
		*ret = 1; /* EOF */
		return 0;
	}

	*ret = 0;
	return 0;
}

int read_byte_memory_stream(addr stream, byte *value, int *ret)
{
	if (input_memory_stream_p(stream))
		return read_byte_read_memory(stream, value, ret);
	if (io_memory_stream_p(stream))
		return read_byte_read_memory(stream, value, ret);

	return 1;
}


/* write-byte */
static int write_byte_write_memory(addr stream, byte value)
{
	addr page;

	CheckMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	clear_unread_io_memory_stream(stream);

	return putc_buffering(page, value);
}

int write_byte_memory_stream(addr stream, byte value)
{
	if (output_memory_stream_p(stream))
		return write_byte_write_memory(stream, value);
	if (io_memory_stream_p(stream))
		return write_byte_write_memory(stream, value);

	return 1;
}


/* file-length */
int file_length_memory_stream(addr stream, size_t *ret)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	length_buffering(page, ret);

	return 0;
}


/* file-position */
int file_position_memory_stream(addr stream, size_t *ret)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_get_buffering(page, ret);

	return 0;
}


/* file-position-start */
int file_position_start_memory_stream(addr stream)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_start_buffering(page);
	clear_unread_io_memory_stream(stream);

	return 0;
}


/* file-position-end */
int file_position_end_memory_stream(addr stream)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_end_buffering(page);
	clear_unread_io_memory_stream(stream);

	return 0;
}


/* file-position-set */
int file_position_set_memory_stream(addr stream, size_t value)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_set_buffering(page, value);
	clear_unread_io_memory_stream(stream);

	return 0;
}


/************************************************************
 *  stream_object.c
 ************************************************************/

void *ptrbody_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrBodyStream_Low(stream);
}

struct StructStream *ptrstruct_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream_Low(stream);
}

void *ptrdata_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrDataStream_Low(stream);
}

void gettype_stream(addr stream, enum StreamType *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetTypeStream_Low(stream, ret);
}

size_t getindex_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return GetIndexStream_Low(stream);
}

void getpathname_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetPathnameStream_Low(stream, ret);
}

void setpathname_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetPathnameStream_Low(stream, value);
}

void getinfo_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetInfoStream_Low(stream, ret);
}

void setinfo_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetInfoStream_Low(stream, value);
}

void getinput_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetInputStream_Low(stream, ret);
}

void setinput_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetInputStream_Low(stream, value);
}

void getoutput_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetOutputStream_Low(stream, ret);
}

void setoutput_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetOutputStream_Low(stream, value);
}

void stream_heap(addr *ret, enum StreamType type, size_t size)
{
	struct StructStream *ptr;
	size_t allsize;

	allsize = sizeoft(struct StructStream) + size;
	heap_arraybody(ret, LISPTYPE_STREAM,
			STREAM_INDEX_SIZE, (byte16)allsize);
	Check(0xFFFF <= allsize, "size error");
	ptr = PtrStructStream(*ret);
	memset(ptr, 0, allsize);
	ptr->type = type;
	ptr->terpri = 0;
	ptr->unread = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;
}

enum StreamType getstreamtype(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->type;
}

int streamp(addr stream)
{
	return GetType(stream) == LISPTYPE_STREAM;
}

int file_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_BinaryInput
		|| check == StreamType_BinaryOutput
		|| check == StreamType_BinaryIO
		|| check == StreamType_CharacterInput
		|| check == StreamType_CharacterOutput
		|| check == StreamType_CharacterIO
		|| check == StreamType_BincharInput
		|| check == StreamType_BincharOutput
		|| check == StreamType_BincharIO
		|| check == StreamType_Probe;
}

int broadcast_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_BroadCast;
}

int concatenated_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Concatenated;
}

int echo_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Echo;
}

int synonym_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Synonym;
}

int twoway_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_TwoWay;
}

int input_string_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_StringInput;
}

int output_string_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_StringOutput;
}

int string_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_StringInput
		|| check == StreamType_StringOutput;
}

int prompt_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Prompt;
}

int pretty_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Pretty;
}

int input_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryInput;
}

int output_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryOutput;
}

int io_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryIO;
}

int memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryInput
		|| check == StreamType_MemoryOutput
		|| check == StreamType_MemoryIO;
}

int read_memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryInput
		|| check == StreamType_MemoryIO;
}

int write_memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryOutput
		|| check == StreamType_MemoryIO;
}

int extend_stream_p(addr stream)
{
	return streamp(stream)
		&& ((int)StreamType_Size) <= ((int)getstreamtype(stream));
}

int extend_type_stream_p(addr stream, int type)
{
	return streamp(stream)
		&& ((int)getstreamtype(stream)) == type;
}


/*
 *  control
 */
void force_open_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	PtrStructStream(stream)->closed = 0;
}

void force_close_stream(addr stream)
{
	struct StructStream *ptr;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;
}


/*
 *  check prompt
 */
int use_prompt_stream(Execute ptr, addr stream)
{
	if (! streamp(stream))
		return 0;
	if (prompt_stream_p(stream))
		return 1;

	/* synonym */
	if (synonym_stream_p(stream)) {
		get_synonym_stream(stream, &stream);
		if (! symbolp(stream))
			return 0;
		getspecial_local(ptr, stream, &stream);
		if (stream == Unbound)
			return 0;
		return use_prompt_stream(ptr, stream);
	}

	/* two-way */
	if (twoway_stream_p(stream)) {
		get_twoway_input_stream(stream, &stream);
		return use_prompt_stream(ptr, stream);
	}

	return 0;
}


/************************************************************
 *  stream_open.c
 ************************************************************/

/*
 *  upgraded-open-element-type
 */
int upgrade_open_element_type_stream_(addr var, addr *ret)
{
	*ret = Nil;
	return 0;
}

int open_element_stream_(Execute ptr, addr value, enum Stream_Open_Element *ret)
{
	int result;
	addr check, type;

	/* default */
	if (value == Unbound)
		return Result(ret, Stream_Open_Element_Character);

	/* :default */
	GetConst(KEYWORD_DEFAULT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Character);

	/* unsigned-byte */
	GetConst(COMMON_UNSIGNED_BYTE, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Unsigned8);

	/* character */
	GetConst(COMMON_CHARACTER, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Character);

	/* type */
	if (value == Nil)
		goto error;
	if (parse_type(ptr, &check, value, Nil))
		goto error;

	/* Unicode */
	GetTypeTable(&type, Character);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Character);

	/* bit */
	GetTypeTable(&type, Bit);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned8);

	/* (unsigned-byte 8) */
	GetTypeTable(&type, Unsigned8);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned8);

	/* (unsigned-byte 16) */
	GetTypeTable(&type, Unsigned16);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned16);

	/* (unsigned-byte 32) */
	GetTypeTable(&type, Unsigned32);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned32);

#ifdef LISP_ARCH_64BIT
	/* (unsigned-byte 64) 64-bit only */
	GetTypeTable(&type, Unsigned64);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned64);
#endif

	/* (signed-byte 8) */
	GetTypeTable(&type, Signed8);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed8);

	/* (signed-byte 16) */
	GetTypeTable(&type, Signed16);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed16);

	/* (signed-byte 32) */
	GetTypeTable(&type, Signed32);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed32);

#ifdef LISP_ARCH_64BIT
	/* (signed-byte 64) 64-bit only */
	GetTypeTable(&type, Signed64);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed64);
#endif

	/* error */
error:
	*ret = Stream_Open_Element_Character;
	return fmte_("Invalid :element-type value ~S.", value, NULL);
}


/*
 *  open
 */
static int open_make_empty_stream_(Execute ptr, addr pos)
{
	int ignore;
	addr stream;

	/* memory-stream */
	if (streamp(pos))
		return file_position_start_stream_(pos, &ignore);

	/* pathname */
	Return(open_output_binary_stream_(ptr, &stream, pos, FileOutput_supersede));
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return close_stream_(stream, NULL);
}

static int open_probe_file_stream_(Execute ptr, addr pos, int *ret)
{
	/* memory-stream */
	if (streamp(pos))
		return Result(ret, 1); /* always exist */

	/* pathname */
	Return(probe_file_files_(ptr, &pos, pos));
	return Result(ret, pos != Nil);
}

static int open_if_does_not_exist_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfDoesNot value, int createp, int *existp)
{
	int check;

	Return(open_probe_file_stream_(ptr, pos, &check));
	if (check)
		return Result(existp, 0);

	switch (value) {
		case Stream_Open_IfDoesNot_Create:
			if (createp) {
				Return(open_make_empty_stream_(ptr, pos));
			}
			return Result(existp, 0);

		case Stream_Open_IfDoesNot_Error:
			return call_file_error_(ptr, pos);

		case Stream_Open_IfDoesNot_Nil:
			*ret = Nil;
			return Result(existp, 1);

		default:
			return fmte_("Invalid :if-does-not-exist value.", NULL);
	}
}

static int open_if_exists_pathname_stream_(LocalRoot local,
		addr pos, size_t i, addr *ret)
{
	addr queue, type;

	copy_pathname_alloc(local, &pos, pos);
	charqueue_local(local, &queue, 0);
	GetTypePathname(pos, &type);
	if (stringp(type)) {
		Return(pushstring_charqueue_local_(local, queue, type));
		Return(pushchar_charqueue_local_(local, queue, "."));
	}
	make_index_integer_local(local, &type, i);
	Return(decimal_charqueue_integer_local_(local, type, queue));
	make_charqueue_local(local, queue, &type);
	SetTypePathname(pos, type);

	return Result(ret, pos);
}

static int open_if_exists_rename_stream_(Execute ptr, addr pos)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, ret1, ret2, ret3;
	size_t i;

	/* memory-stream */
	if (streamp(pos))
		return 0;

	/* pathname */
	Return(open_probe_file_stream_(ptr, pos, &check));
	if (! check)
		return 0;

	/* make pathname */
	local = ptr->local;
	push_local(local, &stack);
	for (i = 0; ; i++) {
		Return(open_if_exists_pathname_stream_(local, pos, i, &path));
		Return(open_probe_file_stream_(ptr, path, &check));
		if (! check)
			break;
	}

	/* rename */
	Return(rename_file_files_(ptr, &ret1, &ret2, &ret3, pos, path));
	rollback_local(local, stack);
	return 0;
}

static int open_if_exists_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfExists value,
		enum FileOutput *mode,
		int *existp)
{
	int check;

	Return(open_probe_file_stream_(ptr, pos, &check));
	if (! check) {
		*mode = FileOutput_supersede;
		return Result(existp, 0);
	}
	switch (value) {
		case Stream_Open_IfExists_Error:
			*mode = FileOutput_supersede;
			return call_file_error_(ptr, pos);

		case Stream_Open_IfExists_RenameAndDelete:
		case Stream_Open_IfExists_NewVersion:
		case Stream_Open_IfExists_Supersede:
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Rename:
			Return(open_if_exists_rename_stream_(ptr, pos));
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Overwrite:
			*mode = FileOutput_overwrite;
			break;

		case Stream_Open_IfExists_Append:
			*mode = FileOutput_append;
			break;

		case Stream_Open_IfExists_Nil:
			*ret = Nil;
			return Result(existp, 1);

		default:
			*mode = FileOutput_supersede;
			*existp = 0;
			return fmte_("Invalid :if-exist value.", NULL);
	}

	return Result(existp, 0);
}

static int open_external_input_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_input_stream_(ptr, ret, pos);

		case Stream_Open_External_Ascii:
			return open_input_ascii_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf8:
			return open_input_utf8_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf8Bom:
			return open_input_utf8bom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16:
			return open_input_utf16_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16Le:
			return open_input_utf16le_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16Be:
			return open_input_utf16be_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16LeBom:
			return open_input_utf16lebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16BeBom:
			return open_input_utf16bebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32:
			return open_input_utf32_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32Le:
			return open_input_utf32le_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32Be:
			return open_input_utf32be_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32LeBom:
			return open_input_utf32lebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32BeBom:
			return open_input_utf32bebom_stream_(ptr, ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_input_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	addr stream;

	/* rewind */
	if (memory_stream_p(pos)) {
		Return(file_position_start_stream_(pos, &check));
		if (check)
			return call_file_error_(ptr, pos);
	}

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 1, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Character:
			Return(open_external_input_stream_(ptr, &stream, pos, ext));
			break;

		case Stream_Open_Element_Unsigned8:
			Return(open_input_binary_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Unsigned16:
			Return(open_input_unsigned16_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Unsigned32:
			Return(open_input_unsigned32_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed8:
			Return(open_input_signed8_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed16:
			Return(open_input_signed16_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed32:
			Return(open_input_signed32_stream_(ptr, &stream, pos));
			break;

#ifdef LISP_64BIT
		case Stream_Open_Element_Unsigned64:
			Return(open_input_unsigned64_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed64:
			Return(open_input_signed64_stream_(ptr, &stream, pos));
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_external_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_output_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Ascii:
			return open_output_ascii_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_output_utf8_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf8Bom:
			return open_output_utf8_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Le:
			return open_output_utf16le_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Be:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16LeBom:
			return open_output_utf16le_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16BeBom:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Le:
			return open_output_utf32le_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Be:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32LeBom:
			return open_output_utf32le_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32BeBom:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 1);

		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 0, &check));
	if (check)
		return 0;

	/* :if-exists */
	Return(open_if_exists_stream_(ptr, ret, pos, if1, &mode, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Character:
			Return(open_external_output_stream_(ptr, &stream, pos, ext, mode));
			break;

		case Stream_Open_Element_Unsigned8:
			Return(open_output_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned16:
			Return(open_output_unsigned16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned32:
			Return(open_output_unsigned32_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed8:
			Return(open_output_signed8_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed16:
			Return(open_output_signed16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed32:
			Return(open_output_signed32_stream_(ptr, &stream, pos, mode));
			break;

#ifdef LISP_64BIT
		case Stream_Open_Element_Unsigned64:
			Return(open_output_unsigned64_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed64:
			Return(open_output_signed64_stream_(ptr, &stream, pos, mode));
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_external_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_io_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Ascii:
			return open_io_ascii_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_io_utf8_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8Bom:
			return open_io_utf8bom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16:
			return open_io_utf16_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Le:
			return open_io_utf16le_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Be:
			return open_io_utf16be_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16LeBom:
			return open_io_utf16lebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16BeBom:
			return open_io_utf16bebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32:
			return open_io_utf32_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Le:
			return open_io_utf32le_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Be:
			return open_io_utf32be_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32LeBom:
			return open_io_utf32lebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32BeBom:
			return open_io_utf32bebom_stream_(ptr, ret, pos, mode);

		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 0, &check));
	if (check)
		return 0;

	/* :if-exists */
	Return(open_if_exists_stream_(ptr, ret, pos, if1, &mode, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Character:
			Return(open_external_io_stream_(ptr, &stream, pos, ext, mode));
			break;

		case Stream_Open_Element_Unsigned8:
			Return(open_io_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned16:
			Return(open_io_unsigned16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned32:
			Return(open_io_unsigned32_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed8:
			Return(open_io_signed8_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed16:
			Return(open_io_signed16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed32:
			Return(open_io_signed32_stream_(ptr, &stream, pos, mode));
			break;

#ifdef LISP_64BIT
		case Stream_Open_Element_Unsigned64:
			Return(open_io_unsigned64_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed64:
			Return(open_io_signed64_stream_(ptr, &stream, pos, mode));
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_direct_probe_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ignore)
{
	int check;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 1, &check));
	if (check)
		return 0;

	/* :element-type */
	Return(open_probe_stream_(ptr, &stream, pos));

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

int open_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	/* :direction */
	switch (direction) {
		case Stream_Open_Direction_Input:
			return open_direct_input_stream_(ptr, ret, pos, type, if2, ext);

		case Stream_Open_Direction_Output:
			return open_direct_output_stream_(ptr, ret, pos, type, if1, if2, ext);

		case Stream_Open_Direction_Io:
			return open_direct_io_stream_(ptr, ret, pos, type, if1, if2, ext);

		case Stream_Open_Direction_Probe:
			return open_direct_probe_stream_(ptr, ret, pos, type, if2, ext);

		default:
			*ret = Nil;
			return fmte_("Invalid direction.", NULL);
	}
}


/************************************************************
 *  stream_pretty.c
 ************************************************************/

/*
 *  stream-pretty object
 */
struct stream_Pretty {
	unsigned list : 1;
	unsigned discard : 1;
	unsigned alive : 1;
	size_t length, depth, terpri;
};

#define CheckPrettyStream(stream) { \
	Check(! pretty_stream_p(stream), "type error"); \
}
#define PtrPrettyStream(pos) ((struct stream_Pretty *)PtrDataStream(pos))

enum StreamPretty_Index {
	StreamPretty_Stream,
	StreamPretty_Root,
	StreamPretty_Object,
	StreamPretty_Prefix,
	StreamPretty_PerLine,
	StreamPretty_Suffix,
	StreamPretty_Gensym,
	StreamPretty_Stack,
	StreamPretty_Queue,
	StreamPretty_Sharp,
	StreamPretty_Size
};


/*
 *  access
 */
static void setalive_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->alive = value;
}

static void increment_pretty_stream(addr stream)
{
	CheckPrettyStream(stream);
	(PtrPrettyStream(stream)->length)++;
}

static int alive_pretty_stream_(addr stream)
{
	CheckPrettyStream(stream);
	if (! PtrPrettyStream(stream)->alive)
		return fmte_("The stream ~S is already closed.", stream, NULL);
	return 0;
}

void setlistp_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->list = value;
}

int listp_pretty_stream(addr stream)
{
	return pretty_stream_p(stream)
		&& PtrPrettyStream(stream)->list != 0;
}

void setdiscard_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->discard = value;
}

int discard_pretty_stream(addr stream)
{
	return pretty_stream_p(stream)
		&& PtrPrettyStream(stream)->discard != 0;
}

static int getinfo_pretty_stream_(addr stream, addr *ret)
{
	Return(alive_pretty_stream_(stream));
	GetInfoStream(stream, ret);
	return 0;
}

int length_pretty_stream_(addr stream, size_t *ret)
{
	Return(alive_pretty_stream_(stream));
	return Result(ret, PtrPrettyStream(stream)->length);
}

int first_pretty_stream_(addr stream, int *ret)
{
	size_t check;
	Return(length_pretty_stream_(stream, &check));
	return Result(ret, check == 0);
}

static int getstream_pretty_stream_(addr stream, addr *ret)
{
	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Stream, ret);
	return 0;
}

int gensym_pretty_stream_(addr stream, addr *ret)
{
	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Gensym, ret);
	return 0;
}

int root_pretty_stream_(addr stream, addr *ret)
{
	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Root, ret);
	return 0;
}

int setroot_pretty_stream_(addr stream, addr value)
{
	Return(getinfo_pretty_stream_(stream, &stream));
	SetArrayA2(stream, StreamPretty_Root, value);
	return 0;
}

int object_pretty_stream_(addr stream, addr *ret)
{
	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Object, ret);
	return 0;
}

static int queue_pretty_stream_(addr stream, addr *ret)
{
	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Queue, ret);
	return 0;
}


/*
 *  external function
 */
static void nocheck_info_pretty_stream(addr stream, addr *ret)
{
	CheckPrettyStream(stream);
	GetInfoStream(stream, ret);
}

void prefix_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Prefix, ret);
}

void perline_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_PerLine, ret);
}

void suffix_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Suffix, ret);
}

void stream_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stream, ret);
}

void result_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stack, ret);
}

void sharp_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Sharp, ret);
}

void setsharp_pretty_stream(addr stream, addr value)
{
	nocheck_info_pretty_stream(stream, &stream);
	SetArrayA2(stream, StreamPretty_Sharp, value);
}

static int push_unsafe_pretty_stream_(addr stream, addr pos)
{
	addr stack;

	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Stack, &stack);
	cons_heap(&stack, pos, stack);
	SetArrayA2(stream, StreamPretty_Stack, stack);

	return 0;
}

static int flush_pretty_stream_(addr stream)
{
	addr queue, value;
	size_t size;

	Return(queue_pretty_stream_(stream, &queue));
	getsize_charqueue(queue, &size);
	if (size != 0) {
		make_charqueue_heap(queue, &value);
		clear_charqueue(queue);
		Return(push_unsafe_pretty_stream_(stream, value));
	}

	return 0;
}


/*
 *  make
 */
static void make_info_pretty_vector(addr *ret, addr stream,
		addr root, addr prefix, addr perline, addr suffix, addr gensym, addr queue)
{
	addr pos;

	vector2_heap(&pos, StreamPretty_Size);
	SetArrayA2(pos, StreamPretty_Stream, stream);
	SetArrayA2(pos, StreamPretty_Root, root);
	SetArrayA2(pos, StreamPretty_Object, root);
	SetArrayA2(pos, StreamPretty_Prefix, prefix);
	SetArrayA2(pos, StreamPretty_PerLine, perline);
	SetArrayA2(pos, StreamPretty_Suffix, suffix);
	SetArrayA2(pos, StreamPretty_Gensym, gensym);
	SetArrayA2(pos, StreamPretty_Queue, queue);
	SetArrayA2(pos, StreamPretty_Sharp, Nil);
	*ret = pos;
}

static int make_info_pretty_stream_(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix)
{
	addr gensym, queue;

	/* gensym */
	Return(make_gensym_(ptr, &gensym));
	/* charqueue */
	if (pretty_stream_p(stream)) {
		Return(flush_pretty_stream_(stream));
		Return(queue_pretty_stream_(stream, &queue));
	}
	else {
		charqueue_heap(&queue, 0);
	}
	/* info */
	make_info_pretty_vector(ret, stream, root, prefix, perline, suffix, gensym, queue);
	return 0;
}

int open_pretty_stream_(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix)
{
	addr pos, info;
	struct stream_Pretty *str;

	/* type check */
	if (! streamp(stream))
		return TypeError_(stream, STREAM);
	if (prefix != Nil && (! stringp(prefix)))
		return TypeError_(prefix, STRING);
	if (perline != Nil && (! stringp(perline)))
		return TypeError_(perline, STRING);
	if (suffix != Nil && (! stringp(suffix)))
		return TypeError_(suffix, STRING);
	if (prefix != Nil && perline != Nil)
		return fmte_("Cannot supply both :PREFIX and :PER-LINE-PREFIX.", NULL);

	/* make */
	stream_heap(&pos, StreamType_Pretty, sizeoft(struct stream_Pretty));
	str = PtrPrettyStream(pos);
	str->list = listp(root);
	str->discard = discard_pretty_stream(stream);
	str->alive = 1;
	str->length = 0;
	Return(getleft_stream_(stream, &(str->terpri)));
	getdepth_print_write(ptr, &(str->depth));
	/* info */
	Return(make_info_pretty_stream_(ptr, &info, stream, root, prefix, perline, suffix));
	SetInfoStream(pos, info);
	/* result */
	force_open_stream(pos);
	return Result(ret, pos);
}


/*
 *  pretty-stream function
 */
static int nreverse_unsafe_pretty_stream_(addr stream)
{
	addr stack;

	Return(getinfo_pretty_stream_(stream, &stream));
	GetArrayA2(stream, StreamPretty_Stack, &stack);
	nreverse(&stack, stack);
	SetArrayA2(stream, StreamPretty_Stack, stack);

	return 0;
}

void setdepth_pretty_stream(Execute ptr, addr stream, size_t inc)
{
	size_t depth;

	depth = PtrPrettyStream(stream)->depth;
	setdepth_print_write(ptr, depth + inc);
}

int close_pretty_stream_(Execute ptr, addr stream)
{
	addr pos;

	/* depth */
	setdepth_pretty_stream(ptr, stream, 0);
	/* discard */
	if (discard_pretty_stream(stream)) {
		setalive_pretty_stream(stream, 0);
		return 0;
	}
	/* stack */
	Return(flush_pretty_stream_(stream));
	Return(nreverse_unsafe_pretty_stream_(stream));
	/* close */
	setalive_pretty_stream(stream, 0);
	force_close_stream(stream);
	/* output */
	stream_pretty_stream(stream, &pos);
	if (pretty_stream_p(pos)) {
		Return(push_pretty_stream_(pos, stream));
	}
	else {
		Return(pprint_output_(ptr, pos, stream));
	}

	return 0;
}

int close_pretty_stream_unwind_protect_(Execute ptr, addr stream)
{
	addr control, save;

	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (close_pretty_stream_(ptr, stream))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}

int push_pretty_stream_(addr stream, addr pos)
{
	Return(flush_pretty_stream_(stream));
	return push_unsafe_pretty_stream_(stream, pos);
}

int pop_pretty_stream_(addr stream, addr *value, int *ret)
{
	addr info, list;

	Return(getinfo_pretty_stream_(stream, &info));
	GetArrayA2(info, StreamPretty_Root, &list);
	if (list == Nil)
		return Result(ret, 1);
	if (consp(list)) {
		GetCons(list, value, &list);
	}
	else {
		*value = list;
		list = Nil;
	}
	increment_pretty_stream(stream);
	SetArrayA2(info, StreamPretty_Root, list);

	return Result(ret, 0);
}

static int character_pretty_stream_(addr stream, unicode u)
{
	if (u == 0x0A) {
		Return(pprint_newline_terpri_(stream));
	}
	else {
		Return(queue_pretty_stream_(stream, &stream));
		Return(push_charqueue_heap_(stream, u));
	}

	return 0;
}

int push_pretty_stream_p(addr stream)
{
	return output_string_stream_p(stream)?
		get_pretty_output_string_stream(stream):
		pretty_stream_p(stream);
}

static int Push_pretty_stream_p_(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return Result(ret, push_pretty_stream_p(stream));
}

static int rollback_pretty_stream_(addr stream)
{
	addr root, pos, info;
	struct stream_Pretty *str;

	/* object */
	GetInfoStream(stream, &info);
	GetArrayA2(info, StreamPretty_Object, &root);
	SetArrayA2(info, StreamPretty_Root, root);
	SetArrayA2(info, StreamPretty_Stack, Nil);
	GetArrayA2(info, StreamPretty_Queue, &pos);
	clear_charqueue(pos);

	/* struct */
	str = PtrPrettyStream(stream);
	str->list = listp(root);
	str->discard = 0;
	str->alive = 1;
	str->length = 0;
	return setleft_stream_(stream, str->terpri);
}

static int call_pretty_stream_call_(Execute ptr, addr stream, addr call)
{
	int circlep;
	addr pos;

	/* normal */
	Return(circle_print_(ptr, &circlep));
	if (! circlep)
		return callclang_funcall(ptr, &pos, call, NULL);
	/* circle check */
	setdiscard_pretty_stream(stream, 1);
	Return(root_pretty_stream_(stream, &pos));
	/* check */
	Return(write_check_call_(ptr, pos));
	/* call */
	Return(callclang_funcall(ptr, &pos, call, NULL));
	/* circle output */
	Return(rollback_pretty_stream_(stream));
	write_check_all_clear(ptr);
	return callclang_funcall(ptr, &pos, call, NULL);
}

int call_pretty_stream(Execute ptr, addr stream, addr call)
{
	int check;
	addr control;

	Check(! pretty_stream_p(stream), "type error");
	Check(! functionp(call), "type error");

	Return(Push_pretty_stream_p_(stream, &check));
	if (check)
		return callclang_funcall(ptr, &call, call, NULL);

	/* push */
	push_control(ptr, &control);
	push_write_object(ptr);
	(void)call_pretty_stream_call_(ptr, stream, call);
	return pop_control_(ptr, control);
}


/*
 *  stream function
 */
static int close_Pretty(addr stream, addr *ret)
{
	stream_pretty_stream(stream, &stream);
	return Result(ret, T);
}

static int read_char_Pretty(addr stream, unicode *u, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return read_char_stream_(stream, u, ret);
}

static int read_hang_Pretty(addr stream, unicode *u, int *hang, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_Pretty(addr stream, unicode c)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return unread_char_stream_(stream, c);
}

static int write_char_Pretty(addr stream, unicode u)
{
	return character_pretty_stream_(stream, u);
}

static int getleft_Pretty(addr stream, size_t *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return getleft_stream_(stream, ret);
}

static int setleft_Pretty(addr stream, size_t value)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return setleft_stream_(stream, value);
}

static int inputp_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return inputp_stream_(stream, ret);
}

static int outputp_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return outputp_stream_(stream, ret);
}

static int interactivep_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return interactivep_stream_(stream, ret);
}

static int characterp_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return characterp_stream_(stream, ret);
}

static int binaryp_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return binaryp_stream_(stream, ret);
}

static int element_type_Pretty(addr stream, addr *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return element_type_stream_(stream, ret);
}

static int external_format_Pretty(addr stream, addr *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return external_format_stream_(stream, ret);
}

static int file_length_Pretty(addr stream, addr *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_length_stream_(stream, ret);
}

static int file_position_Pretty(addr stream, size_t *value, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_position_stream_(stream, value, ret);
}

static int file_position_start_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_position_start_stream_(stream, ret);
}

static int file_position_end_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_position_end_stream_(stream, ret);
}

static int file_position_set_Pretty(addr stream, size_t value, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_position_set_stream_(stream, value, ret);
}

static int file_charlen_Pretty(addr stream, unicode u, size_t *value, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Pretty(addr stream, addr pos, size_t *value, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Pretty(addr stream, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return listen_stream_(stream, ret);
}

static int clear_input_Pretty(addr stream)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return clear_input_stream_(stream);
}

static int finish_output_Pretty(addr stream)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return finish_output_stream_(stream);
}

static int force_output_Pretty(addr stream)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return force_output_stream_(stream);
}

static int clear_output_Pretty(addr stream)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return clear_output_stream_(stream);
}

static int termsize_Pretty(addr stream, size_t *value, int *ret)
{
	Return(getstream_pretty_stream_(stream, &stream));
	return termsize_stream_(stream, value, ret);
}

void init_stream_pretty(void)
{
	DefineStreamSet(Pretty, close);
	DefineStream___(Pretty, read_byte);
	DefineStream___(Pretty, unread_byte);
	DefineStream___(Pretty, write_byte);
	DefineStreamSet(Pretty, read_char);
	DefineStreamSet(Pretty, read_hang);
	DefineStreamSet(Pretty, unread_char);
	DefineStreamSet(Pretty, write_char);
	DefineStreamSet(Pretty, getleft);
	DefineStreamSet(Pretty, setleft);
	DefineStreamSet(Pretty, inputp);
	DefineStreamSet(Pretty, outputp);
	DefineStreamSet(Pretty, interactivep);
	DefineStreamSet(Pretty, characterp);
	DefineStreamSet(Pretty, binaryp);
	DefineStreamSet(Pretty, element_type);
	DefineStreamSet(Pretty, external_format);
	DefineStreamSet(Pretty, file_length);
	DefineStreamSet(Pretty, file_position);
	DefineStreamSet(Pretty, file_position_start);
	DefineStreamSet(Pretty, file_position_end);
	DefineStreamSet(Pretty, file_position_set);
	DefineStreamSet(Pretty, file_charlen);
	DefineStreamSet(Pretty, file_strlen);
	DefineStreamSet(Pretty, listen);
	DefineStreamSet(Pretty, clear_input);
	DefineStreamSet(Pretty, finish_output);
	DefineStreamSet(Pretty, force_output);
	DefineStreamSet(Pretty, clear_output);
	DefineStreamDef(Pretty, exitpoint);
	DefineStreamSet(Pretty, termsize);
}


/************************************************************
 *  stream_prompt.c
 ************************************************************/

#define CheckPromptStream(stream) { \
	Check(! prompt_stream_p(stream), "type error"); \
}

#ifdef LISP_PROMPT_DISABLE
void open_prompt_stream(addr *stream)
{
	Error(standard_input_stream_(Execute_Thread, stream));
}
#else
void open_prompt_stream(addr *stream)
{
	addr pos, value;

	stream_heap(&pos, StreamType_Prompt, 0);
	null_input_string_stream(&value);
	SetInfoStream(pos, value);
	force_open_stream(pos);
	*stream = pos;
}
#endif

static int close_Prompt(addr stream, addr *ret)
{
	CheckPromptStream(stream);
	GetInfoStream(stream, &stream);
	clear_input_string_stream(stream);
	return Result(ret, T);
}

static int input_prompt_stream(addr stream, addr *ret)
{
	addr pos, prompt, dribble;
	Execute ptr;

	/* read */
	ptr = Execute_Thread;
	getvalue_prompt(ptr, &prompt);
	Return(input_prompt_(ptr, &pos));
	if (pos == Nil) /* eof */
		return Result(ret, Nil);
	/* dribble check */
	GetConst(SYSTEM_DRIBBLE_FILE, &dribble);
	GetValueSymbol(dribble, &dribble);
	if (dribble != Unbound) {
		if (prompt != Nil) {
			Return(print_string_stream_(dribble, prompt));
		}
		Return(print_string_stream_(dribble, pos));
	}
	/* result */
	return Result(ret, pos);
}

static int read_char_prompt_line_(addr stream, unicode *c, int *ret)
{
	int check;
	addr string, pos;

	GetInfoStream(stream, &string);
	if (! open_stream_p(string)) {
		Return(input_prompt_stream(stream, &pos));
		if (pos == Nil)
			return Result(ret, 1); /* eof */
		setvalue_input_string_stream(string, pos);
	}
	for (;;) {
		Return(read_char_stream_(string, c, &check));
		if (! check)
			break;
		Return(input_prompt_stream(stream, &pos));
		if (pos == Nil)
			return Result(ret, 1); /* eof */
		setvalue_input_string_stream(string, pos);
	}

	/* normal */
	return Result(ret, 0);
}

static int read_char_Prompt(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;

	CheckPromptStream(stream);
	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_char_prompt_line_(stream, c, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	return Result(ret, 0);
}

static int read_hang_Prompt(addr stream, unicode *c, int *hang, int *ret)
{
	int check;
	addr string;
	struct StructStream *ptr;

	CheckPromptStream(stream);
	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		ptr->unread_check = 0;
		*c = ptr->unread;
		*hang = 0;
		return Result(ret, 0);
	}

	/* read string-buffer */
	GetInfoStream(stream, &string);
	Return(read_char_stream_(stream, c, &check));
	*hang = (check != 0);

	return Result(ret, 0);
}

static int listen_Prompt(addr stream, int *ret)
{
	int check;
	unicode c;
	struct StructStream *ptr;

	CheckPromptStream(stream);
	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return Result(ret, 1);
	/* string */
	GetInfoStream(stream, &stream);
	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		/* input-prompt */
		return Result(ret, 0);
	}
	Return(unread_char_stream_(stream, c));

	return Result(ret, 1);
}

static int clear_input_Prompt(addr stream)
{
	CheckPromptStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	GetInfoStream(stream, &stream);
	clear_input_string_stream(stream);
	return 0;
}

void init_stream_prompt(void)
{
	DefineStreamSet(Prompt, close);
	DefineStream___(Prompt, read_byte);
	DefineStream___(Prompt, unread_byte);
	DefineStream___(Prompt, write_byte);
	DefineStreamSet(Prompt, read_char);
	DefineStreamSet(Prompt, read_hang);
	DefineStreamDef(Prompt, unread_char);
	DefineStream___(Prompt, write_char);
	DefineStream___(Prompt, getleft);
	DefineStream___(Prompt, setleft);
	DefineStreamChk(Prompt, inputp, true);
	DefineStreamChk(Prompt, outputp, false);
	DefineStreamChk(Prompt, interactivep, true);
	DefineStreamChk(Prompt, characterp, true);
	DefineStreamChk(Prompt, binaryp, false);
	DefineStreamLet(Prompt, element_type, character_stream);
	DefineStreamDef(Prompt, external_format);
	DefineStream___(Prompt, file_length);
	DefineStreamDef(Prompt, file_position);
	DefineStreamDef(Prompt, file_position_start);
	DefineStreamDef(Prompt, file_position_end);
	DefineStreamDef(Prompt, file_position_set);
	DefineStream___(Prompt, file_charlen);
	DefineStream___(Prompt, file_strlen);
	DefineStreamSet(Prompt, listen);
	DefineStreamSet(Prompt, clear_input);
	DefineStream___(Prompt, finish_output);
	DefineStream___(Prompt, force_output);
	DefineStream___(Prompt, clear_output);
	DefineStream___(Prompt, exitpoint);
	DefineStreamLet(Prompt, termsize, file_);
}


/************************************************************
 *  stream_string.c
 ************************************************************/

/*****************************************************************************
 *  StringInput
 *****************************************************************************/
struct stream_StringInput {
	size_t index, size;
};

#define CheckInputStringStream(stream) { \
	Check(! input_string_stream_p(stream), "type error"); \
}
#define PtrStringInputStream(pos) ((struct stream_StringInput *)PtrDataStream(pos))

static void make_input_string(addr *ret, addr string, size_t start, size_t end)
{
	addr pos;
	struct stream_StringInput *input;

	stream_heap(&pos, StreamType_StringInput, sizeoft(struct stream_StringInput));
	input = PtrStringInputStream(pos);
	input->index = start;
	input->size = end;
	SetInfoStream(pos, string);
	force_open_stream(pos);
	*ret = pos;
}

int open_input_string_stream_(addr *ret, addr string)
{
	size_t size;

	if (! stringp(string))
		return TypeError_(string, STRING);
	string_length(string, &size);
	make_input_string(ret, string, 0, size);

	return 0;
}

int open_input_string_stream1_(addr *ret, addr string, size_t start)
{
	addr pos1, pos2;
	size_t size;

	if (! stringp(string))
		return TypeError_(string, STRING);
	string_length(string, &size);
	if (size < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, size);
		return fmte_("The start index ~S "
				"must be less than equal to length of string ~S.", pos1, pos2, NULL);
	}
	make_input_string(ret, string, start, size);

	return 0;
}

int open_input_string_stream2_(addr *ret, addr string, size_t start, size_t end)
{
	addr pos1, pos2;
	size_t size;

	if (! stringp(string))
		return TypeError_(string, STRING);
	string_length(string, &size);
	if (size < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, size);
		return fmte_("The start index ~S "
				"must be less than equal to length of string ~S.", pos1, pos2, NULL);
	}
	if (size < end) {
		make_index_integer_alloc(NULL, &pos1, end);
		make_index_integer_alloc(NULL, &pos2, size);
		return fmte_("The end index ~S "
				"must be less than equal to length of string ~S.", pos1, pos2, NULL);
	}
	if (end < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, end);
		return fmte_("The start index ~S "
				"must be less than equal to the end index~S.", pos1, pos2, NULL);
	}
	make_input_string(ret, string, start, end);

	return 0;
}

void open_input_char_stream(addr *stream, const char *str)
{
	addr pos;
	size_t size;

	strvect_char_heap(&pos, str);
	string_length(pos, &size);
	make_input_string(stream, pos, 0, size);
}

void null_input_string_stream(addr *stream)
{
	addr pos;
	struct StructStream *str;

	make_input_string(&pos, Nil, 0, 0);
	str = PtrStructStream(pos);
	str->unread_check = 0;
	str->closed = 1;
	str->unread = 0;
	str->terpri = 0;
	*stream = pos;
}

void close_input_string_stream(addr stream)
{
	force_close_stream(stream);
}

void getindex_input_stream(addr stream, size_t *ret)
{
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	input = PtrStringInputStream(stream);
	*ret = input->index;
}

static int unread_char_StringInput(addr stream, unicode c)
{
	struct StructStream *ptr;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	/* unread check */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return fmte_("unread already exists.", NULL);
	/* index check */
	input = PtrStringInputStream(stream);
	if (input->index == 0)
		return fmte_("index underflow.", NULL);
	input->index--;
	ptr->unread = c;
	ptr->unread_check = 1;

	return 0;
}

static int read_char_StringInput(addr stream, unicode *c, int *ret)
{
	addr string;
	struct StructStream *ptr;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	ptr = PtrStructStream(stream);
	input = PtrStringInputStream(stream);
	if (ptr->unread_check) {
		*c = ptr->unread;
		ptr->unread_check = 0;
		input->index++;
	}
	else {
		if (input->size <= input->index)
			return Result(ret, 1); /* EOF */
		GetInfoStream(stream, &string);
		Return(string_getc_(string, input->index++, c));
	}

	return Result(ret, 0);
}

static int read_hang_StringInput(addr stream, unicode *c, int *hang, int *ret)
{
	int check;

	CheckInputStringStream(stream);
	Return(read_char_StringInput(stream, c, &check));
	*hang = (check != 0);

	return Result(ret, check);
}

void setvalue_input_string_stream(addr stream, addr value)
{
	struct StructStream *str;
	struct stream_StringInput *input;
	size_t size;

	CheckInputStringStream(stream);
	Check(! stringp(value), "type error string.");
	/* string-stream */
	string_length(value, &size);
	input = PtrStringInputStream(stream);
	input->index = 0;
	input->size = size;
	SetInfoStream(stream, value);
	/* stream */
	str = PtrStructStream(stream);
	str->unread_check = 0;
	str->closed = 0;
	str->unread = 0;
	str->terpri = 0;
}

void clear_input_string_stream(addr stream)
{
	struct StructStream *str;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	/* string-stream */
	input = PtrStringInputStream(stream);
	input->index = 0;
	input->size = 0;
	SetInfoStream(stream, Nil);
	/* stream */
	str = PtrStructStream(stream);
	str->unread_check = 0;
	str->closed = 0;
	str->unread = 0;
	str->terpri = 0;
}

static int file_position_StringInput(addr stream, size_t *value, int *ret)
{
	struct StructStream *str;
	struct stream_StringInput *input;
	size_t size;

	CheckInputStringStream(stream);
	input = PtrStringInputStream(stream);
	size = input->index;
	str = PtrStructStream(stream);
	if (str->unread_check) {
		if (size == 0) {
			*value = 0;
			*ret = 0;
			return fmte_("The stream ~S position is minus value.", stream, NULL);
		}
	}
	*value = size;
	return Result(ret, 0);
}

static int file_position_start_StringInput(addr stream, int *ret)
{
	struct StructStream *str;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	str = PtrStructStream(stream);
	input = PtrStringInputStream(stream);
	str->unread_check = 0;
	input->index = 0;
	return Result(ret, 0);
}

static int file_position_end_StringInput(addr stream, int *ret)
{
	struct stream_StringInput *str;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	str->index = str->size;
	PtrStructStream(stream)->unread_check = 0;

	return Result(ret, 0);
}

static int file_position_set_StringInput(addr stream, size_t value, int *ret)
{
	struct stream_StringInput *str;
	addr pos;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	if (str->size < value) {
		*ret = 0;
		make_index_integer_heap(&pos, value);
		return fmte_("The position ~A is too large.", pos, NULL);
	}

	str->index = value;
	PtrStructStream(stream)->unread_check = 0;
	return Result(ret, 0);
}

static int listen_StringInput(addr stream, int *ret)
{
	struct stream_StringInput *str;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	return Result(ret, str->index < str->size);
}

static int clear_input_StringInput(addr stream)
{
	CheckInputStringStream(stream);
	return 0;
}

void init_stream_string_input(void)
{
	DefineStreamDef(StringInput, close);
	DefineStream___(StringInput, read_byte);
	DefineStream___(StringInput, unread_byte);
	DefineStream___(StringInput, write_byte);
	DefineStreamSet(StringInput, read_char);
	DefineStreamSet(StringInput, read_hang);
	DefineStreamSet(StringInput, unread_char);
	DefineStream___(StringInput, write_char);
	DefineStream___(StringInput, getleft);
	DefineStream___(StringInput, setleft);
	DefineStreamChk(StringInput, inputp, true);
	DefineStreamChk(StringInput, outputp, false);
	DefineStreamChk(StringInput, interactivep, false);
	DefineStreamChk(StringInput, characterp, true);
	DefineStreamChk(StringInput, binaryp, false);
	DefineStreamLet(StringInput, element_type, character_stream);
	DefineStreamDef(StringInput, external_format);
	DefineStream___(StringInput, file_length);
	DefineStreamSet(StringInput, file_position);
	DefineStreamSet(StringInput, file_position_start);
	DefineStreamSet(StringInput, file_position_end);
	DefineStreamSet(StringInput, file_position_set);
	DefineStream___(StringInput, file_charlen);
	DefineStream___(StringInput, file_strlen);
	DefineStreamSet(StringInput, listen);
	DefineStreamSet(StringInput, clear_input);
	DefineStream___(StringInput, finish_output);
	DefineStream___(StringInput, force_output);
	DefineStream___(StringInput, clear_output);
	DefineStreamDef(StringInput, exitpoint);
	DefineStreamDef(StringInput, termsize);
}


/*****************************************************************************
 *  StringOutput
 *****************************************************************************/
struct stream_StringOutput {
	unsigned extend_p : 1;
	unsigned width_p : 1;
	unsigned pretty : 1;
	size_t width;
};

#define CheckOutputStringStream(stream) { \
	Check(! output_string_stream_p(stream), "type error"); \
}
#define PtrStringOutputStream(pos) ((struct stream_StringOutput *)PtrDataStream(pos))

static int extend_string_p(addr stream)
{
	struct stream_StringOutput *output = PtrStringOutputStream(stream);
	return output->extend_p;
}

void open_output_string_stream(addr *stream, size_t size)
{
	addr pos, queue;
	struct stream_StringOutput *str;

	stream_heap(&pos, StreamType_StringOutput, sizeoft(struct stream_StringOutput));
	str = PtrStringOutputStream(pos);
	str->extend_p = 0;
	str->width_p = 0;
	str->pretty = 0;
	str->width = 0;
	charqueue_heap(&queue, size);
	SetInfoStream(pos, queue);
	force_open_stream(pos);
	*stream = pos;
}

int copy_termsize_string_stream_(addr stream, addr src)
{
	int check;
	struct stream_StringOutput *str;
	size_t size;

	CheckOutputStringStream(stream);
	str = PtrStringOutputStream(stream);
	Return(termsize_stream_(src, &size, &check));
	if (check) {
		str->width_p = 0;
		str->width = 0;
	}
	else {
		str->width_p = 1;
		str->width = size;
	}

	return 0;
}

int string_stream_alloc_(LocalRoot local, addr stream, addr *string)
{
	addr queue;

	if (extend_string_p(stream)) {
		*string = Nil;
		return fmte_("The extended-string-stream ~S "
				"don't make a string.", stream, NULL);
	}
	GetInfoStream(stream, &queue);
	if (queue == Nil) {
		*string = Nil;
		return fmte_("stream is already closed.", NULL);
	}
	make_charqueue_alloc(local, queue, string);

	return 0;
}

int string_stream_local_(LocalRoot local, addr stream, addr *string)
{
	Check(local == NULL, "local error");
	return string_stream_alloc_(local, stream, string);
}

int string_stream_heap_(addr stream, addr *string)
{
	return string_stream_alloc_(NULL, stream, string);
}

void clear_output_string_stream(addr stream)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	Check(queue == Nil, "stream is already closed.");
	clear_charqueue(queue);
}

void set_pretty_output_string_stream(addr stream)
{
	CheckOutputStringStream(stream);
	PtrStringOutputStream(stream)->pretty = 1;
}

int get_pretty_output_string_stream(addr stream)
{
	CheckOutputStringStream(stream);
	return PtrStringOutputStream(stream)->pretty != 0;
}

void open_extend_output_stream(addr *stream, addr array)
{
	addr pos;
	struct stream_StringOutput *str;

	stream_heap(&pos, StreamType_StringOutput, sizeoft(struct stream_StringOutput));
	str = PtrStringOutputStream(pos);
	str->extend_p = 1;
	str->width_p = 0;
	str->pretty = 0;
	str->width = 0;
	SetInfoStream(pos, array);
	force_open_stream(pos);
	*stream = pos;
}

void close_output_string_stream(addr stream)
{
	CheckOutputStringStream(stream);
	SetInfoStream(stream, Nil);
	force_close_stream(stream);
}

static int close_StringOutput(addr stream, addr *ret)
{
	close_output_string_stream(stream);
	return Result(ret, T);
}

static int write_char_StringOutput_normal_(addr stream, unicode c)
{
	addr queue;

	/* stream */
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		return fmte_("stream is already closed.", NULL);
	if (GetStatusDynamic(stream)) {
		Return(push_charqueue_local_(Local_Thread, queue, c));
	}
	else {
		Return(push_charqueue_heap_(queue, c));
	}

	/* terpri */
	charleft_default_stream(stream, c);

	return 0;
}

static int write_char_StringOutput_extend_(addr stream, unicode c)
{
	addr queue, value;

	/* stream */
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		return fmte_("stream is already closed.", NULL);
	character_heap(&value, c);
	Return(vector_push_extend_common_(Execute_Thread, value, queue, Unbound, &value));

	/* terpri */
	charleft_default_stream(stream, c);

	return 0;
}

static int write_char_StringOutput(addr stream, unicode c)
{
	CheckOutputStringStream(stream);
	if (extend_string_p(stream))
		return write_char_StringOutput_extend_(stream, c);
	else
		return write_char_StringOutput_normal_(stream, c);
}

static int file_position_StringOutput(addr stream, size_t *value, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		/* fill-pointer */
		Return(array_get_vector_length_(queue, 1, value));
	}
	else {
		getsize_charqueue(queue, value);
	}

	return Result(ret, 0);
}

static int file_position_start_StringOutput(addr stream, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		*ret = array_fill_pointer_start(queue);
		return 0;
	}
	else {
		clear_charqueue(queue);
		return Result(ret, 0);
	}
}

static int file_position_end_StringOutput(addr stream, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		*ret = array_fill_pointer_end(queue);
		return 0;
	}
	else {
		return Result(ret, 0);
	}
}

static int file_position_set_StringOutput(addr stream, size_t value, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream))
		*ret = array_fill_pointer_set(queue, value);
	else
		*ret = position_charqueue(queue, value);

	return 0;
}

static int file_charlen_StringOutput(addr stream, unicode u, size_t *value, int *ret)
{
	CheckOutputStringStream(stream);
	*value = 1;
	return Result(ret, 0);
}

static int file_strlen_StringOutput(addr stream, addr pos, size_t *value, int *ret)
{
	CheckOutputStringStream(stream);
	string_length(pos, value);
	return Result(ret, 0);
}

static int termsize_StringOutput(addr stream, size_t *value, int *ret)
{
	struct stream_StringOutput *str;

	CheckOutputStringStream(stream);
	str = PtrStringOutputStream(stream);
	if (str->width_p)
		*value = str->width;

	return Result(ret, str->width_p);
}

void init_stream_string_output(void)
{
	DefineStreamSet(StringOutput, close);
	DefineStream___(StringOutput, read_byte);
	DefineStream___(StringOutput, unread_byte);
	DefineStream___(StringOutput, write_byte);
	DefineStream___(StringOutput, read_char);
	DefineStream___(StringOutput, read_hang);
	DefineStream___(StringOutput, unread_char);
	DefineStreamSet(StringOutput, write_char);
	DefineStreamDef(StringOutput, getleft);
	DefineStreamDef(StringOutput, setleft);
	DefineStreamChk(StringOutput, inputp, false);
	DefineStreamChk(StringOutput, outputp, true);
	DefineStreamChk(StringOutput, interactivep, false);
	DefineStreamChk(StringOutput, characterp, true);
	DefineStreamChk(StringOutput, binaryp, false);
	DefineStreamLet(StringOutput, element_type, character_stream);
	DefineStreamDef(StringOutput, external_format);
	DefineStream___(StringOutput, file_length);
	DefineStreamSet(StringOutput, file_position);
	DefineStreamSet(StringOutput, file_position_start);
	DefineStreamSet(StringOutput, file_position_end);
	DefineStreamSet(StringOutput, file_position_set);
	DefineStreamSet(StringOutput, file_charlen);
	DefineStreamSet(StringOutput, file_strlen);
	DefineStream___(StringOutput, listen);
	DefineStream___(StringOutput, clear_input);
	DefineStreamDef(StringOutput, finish_output);
	DefineStreamDef(StringOutput, force_output);
	DefineStreamDef(StringOutput, clear_output);
	DefineStreamDef(StringOutput, exitpoint);
	DefineStreamSet(StringOutput, termsize);
}


/************************************************************
 *  stream_synonym.c
 ************************************************************/

#define CheckSynonymStream(stream) { \
	Check(! synonym_stream_p(stream), "type error"); \
}

int open_synonym_stream_(addr *stream, addr symbol)
{
	addr pos;

	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);
	stream_heap(&pos, StreamType_Synonym, 0);
	SetInfoStream(pos, symbol);
	force_open_stream(pos);

	return Result(stream, pos);
}

void get_synonym_stream(addr stream, addr *ret)
{
	CheckSynonymStream(stream);
	GetInfoStream(stream, ret);
}

void set_synonym_stream(addr stream, addr symbol)
{
	CheckSynonymStream(stream);
	CheckType(symbol, LISPTYPE_SYMBOL);
	SetInfoStream(stream, symbol);
}

static int getstream_synonym_(addr stream, addr *ret)
{
	CheckSynonymStream(stream);
	GetInfoStream(stream, &stream);
	return getspecialcheck_local_(Execute_Thread, stream, ret);
}

static int read_byte_Synonym(addr stream, addr *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return read_byte_stream_(stream, value, ret);
}

static int unread_byte_Synonym(addr stream, byte c)
{
	Return(getstream_synonym_(stream, &stream));
	return unread_byte_stream_(stream, c);
}

static int write_byte_Synonym(addr stream, addr pos)
{
	Return(getstream_synonym_(stream, &stream));
	return write_byte_stream_(stream, pos);
}

static int read_char_Synonym(addr stream, unicode *u, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return read_char_stream_(stream, u, ret);
}

static int read_hang_Synonym(addr stream, unicode *u, int *hang, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_Synonym(addr stream, unicode c)
{
	Return(getstream_synonym_(stream, &stream));
	return unread_char_stream_(stream, c);
}

static int write_char_Synonym(addr stream, unicode u)
{
	Return(getstream_synonym_(stream, &stream));
	return write_char_stream_(stream, u);
}

static int getleft_Synonym(addr stream, size_t *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return getleft_stream_(stream, ret);
}

static int setleft_Synonym(addr stream, size_t value)
{
	Return(getstream_synonym_(stream, &stream));
	return setleft_stream_(stream, value);
}

static int inputp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return inputp_stream_(stream, ret);
}

static int outputp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return outputp_stream_(stream, ret);
}

static int interactivep_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return interactivep_stream_(stream, ret);
}

static int characterp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return characterp_stream_(stream, ret);
}

static int binaryp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return binaryp_stream_(stream, ret);
}

static int element_type_Synonym(addr stream, addr *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return element_type_stream_(stream, ret);
}

static int external_format_Synonym(addr stream, addr *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return external_format_stream_(stream, ret);
}

static int file_length_Synonym(addr stream, addr *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_length_stream_(stream, ret);
}

static int file_position_Synonym(addr stream, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_stream_(stream, value, ret);
}

static int file_position_start_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_start_stream_(stream, ret);
}

static int file_position_end_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_end_stream_(stream, ret);
}

static int file_position_set_Synonym(addr stream, size_t value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_set_stream_(stream, value, ret);
}

static int file_charlen_Synonym(addr stream, unicode u, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Synonym(addr stream, addr pos, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return listen_stream_(stream, ret);
}

static int clear_input_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return clear_input_stream_(stream);
}

static int finish_output_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return finish_output_stream_(stream);
}

static int force_output_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return force_output_stream_(stream);
}

static int clear_output_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return clear_output_stream_(stream);
}

static int exitpoint_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return exitpoint_stream_(stream);
}

static int termsize_Synonym(addr stream, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return termsize_stream_(stream, value, ret);
}

void init_stream_synonym(void)
{
	DefineStreamDef(Synonym, close);
	DefineStreamSet(Synonym, read_byte);
	DefineStreamSet(Synonym, unread_byte);
	DefineStreamSet(Synonym, write_byte);
	DefineStreamSet(Synonym, read_char);
	DefineStreamSet(Synonym, read_hang);
	DefineStreamSet(Synonym, unread_char);
	DefineStreamSet(Synonym, write_char);
	DefineStreamSet(Synonym, getleft);
	DefineStreamSet(Synonym, setleft);
	DefineStreamSet(Synonym, inputp);
	DefineStreamSet(Synonym, outputp);
	DefineStreamSet(Synonym, interactivep);
	DefineStreamSet(Synonym, characterp);
	DefineStreamSet(Synonym, binaryp);
	DefineStreamSet(Synonym, element_type);
	DefineStreamSet(Synonym, external_format);
	DefineStreamSet(Synonym, file_length);
	DefineStreamSet(Synonym, file_position);
	DefineStreamSet(Synonym, file_position_start);
	DefineStreamSet(Synonym, file_position_end);
	DefineStreamSet(Synonym, file_position_set);
	DefineStreamSet(Synonym, file_charlen);
	DefineStreamSet(Synonym, file_strlen);
	DefineStreamSet(Synonym, listen);
	DefineStreamSet(Synonym, clear_input);
	DefineStreamSet(Synonym, finish_output);
	DefineStreamSet(Synonym, force_output);
	DefineStreamSet(Synonym, clear_output);
	DefineStreamSet(Synonym, exitpoint);
	DefineStreamSet(Synonym, termsize);
}


/************************************************************
 *  stream_twoway.c
 ************************************************************/

#define CheckTwoWayStream(stream) { \
	Check(! twoway_stream_p(stream), "type error"); \
}

void open_twoway_stream(addr *stream, addr input, addr output)
{
	addr pos;

	CheckType(input, LISPTYPE_STREAM);
	CheckType(output, LISPTYPE_STREAM);
	stream_heap(&pos, StreamType_TwoWay, 0);
	SetInputStream(pos, input);
	SetOutputStream(pos, output);
	force_open_stream(pos);
	*stream = pos;
}

void get_twoway_input_stream(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetInputStream(stream, ret);
}

void set_twoway_input_stream(addr stream, addr input)
{
	CheckTwoWayStream(stream);
	SetInputStream(stream, input);
}

void get_twoway_output_stream(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetOutputStream(stream, ret);
}

void set_twoway_output_stream(addr stream, addr output)
{
	CheckTwoWayStream(stream);
	SetOutputStream(stream, output);
}

static void input_twoway(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetInputStream(stream, ret);
}

static void output_twoway(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetOutputStream(stream, ret);
}

static int read_byte_TwoWay(addr stream, addr *value, int *ret)
{
	input_twoway(stream, &stream);
	return read_byte_stream_(stream, value, ret);
}

static int unread_byte_TwoWay(addr stream, byte c)
{
	input_twoway(stream, &stream);
	return unread_byte_stream_(stream, c);
}

static int write_byte_TwoWay(addr stream, addr pos)
{
	output_twoway(stream, &stream);
	return write_byte_stream_(stream, pos);
}

static int read_char_TwoWay(addr stream, unicode *u, int *ret)
{
	input_twoway(stream, &stream);
	return read_char_stream_(stream, u, ret);
}

static int read_hang_TwoWay(addr stream, unicode *u, int *hang, int *ret)
{
	input_twoway(stream, &stream);
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_TwoWay(addr stream, unicode c)
{
	input_twoway(stream, &stream);
	return unread_char_stream_(stream, c);
}

static int write_char_TwoWay(addr stream, unicode u)
{
	output_twoway(stream, &stream);
	return write_char_stream_(stream, u);
}

static int getleft_TwoWay(addr stream, size_t *ret)
{
	output_twoway(stream, &stream);
	return getleft_stream_(stream, ret);
}

static int setleft_TwoWay(addr stream, size_t value)
{
	output_twoway(stream, &stream);
	return setleft_stream_(stream, value);
}

static int interactivep_TwoWay(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(interactivep_stream_(input, &check1));
	Return(interactivep_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int characterp_TwoWay(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(characterp_stream_(input, &check1));
	Return(characterp_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int binaryp_TwoWay(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(binaryp_stream_(input, &check1));
	Return(binaryp_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int external_format_TwoWay(addr stream, addr *ret)
{
	addr input;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	return external_format_stream_(input, ret);
}

static int file_charlen_TwoWay(addr stream, unicode u, size_t *value, int *ret)
{
	output_twoway(stream, &stream);
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_TwoWay(addr stream, addr pos, size_t *value, int *ret)
{
	output_twoway(stream, &stream);
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_TwoWay(addr stream, int *ret)
{
	input_twoway(stream, &stream);
	return listen_stream_(stream, ret);
}

static int clear_input_TwoWay(addr stream)
{
	input_twoway(stream, &stream);
	return clear_input_stream_(stream);
}

static int finish_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return finish_output_stream_(stream);
}

static int force_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return force_output_stream_(stream);
}

static int clear_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return clear_output_stream_(stream);
}

static int exitpoint_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return exitpoint_stream_(stream);
}

static int termsize_TwoWay(addr stream, size_t *value, int *ret)
{
	output_twoway(stream, &stream);
	return termsize_stream_(stream, value, ret);
}

void init_stream_twoway(void)
{
	DefineStreamDef(TwoWay, close);
	DefineStreamSet(TwoWay, read_byte);
	DefineStreamSet(TwoWay, unread_byte);
	DefineStreamSet(TwoWay, write_byte);
	DefineStreamSet(TwoWay, read_char);
	DefineStreamSet(TwoWay, read_hang);
	DefineStreamSet(TwoWay, unread_char);
	DefineStreamSet(TwoWay, write_char);
	DefineStreamSet(TwoWay, getleft);
	DefineStreamSet(TwoWay, setleft);
	DefineStreamChk(TwoWay, inputp, true);
	DefineStreamChk(TwoWay, outputp, true);
	DefineStreamSet(TwoWay, interactivep);
	DefineStreamSet(TwoWay, characterp);
	DefineStreamSet(TwoWay, binaryp);
	DefineStreamLet(TwoWay, element_type, io_stream);
	DefineStreamSet(TwoWay, external_format);
	DefineStream___(TwoWay, file_length);
	DefineStreamDef(TwoWay, file_position);
	DefineStreamDef(TwoWay, file_position_start);
	DefineStreamDef(TwoWay, file_position_end);
	DefineStreamDef(TwoWay, file_position_set);
	DefineStreamSet(TwoWay, file_charlen);
	DefineStreamSet(TwoWay, file_strlen);
	DefineStreamSet(TwoWay, listen);
	DefineStreamSet(TwoWay, clear_input);
	DefineStreamSet(TwoWay, finish_output);
	DefineStreamSet(TwoWay, force_output);
	DefineStreamSet(TwoWay, clear_output);
	DefineStreamSet(TwoWay, exitpoint);
	DefineStreamSet(TwoWay, termsize);
}


/************************************************************
 *  stream_variable.c
 ************************************************************/

lisp_streamtype_close Stream_close[StreamType_Array];
lisp_streamtype_read_byte Stream_read_byte[StreamType_Array];
lisp_streamtype_unread_byte Stream_unread_byte[StreamType_Array];
lisp_streamtype_write_byte Stream_write_byte[StreamType_Array];
lisp_streamtype_read_char Stream_read_char[StreamType_Array];
lisp_streamtype_read_hang Stream_read_hang[StreamType_Array];
lisp_streamtype_unread_char Stream_unread_char[StreamType_Array];
lisp_streamtype_write_char Stream_write_char[StreamType_Array];
lisp_streamtype_getleft Stream_getleft[StreamType_Array];
lisp_streamtype_setleft Stream_setleft[StreamType_Array];
lisp_streamtype_inputp Stream_inputp[StreamType_Array];
lisp_streamtype_outputp Stream_outputp[StreamType_Array];
lisp_streamtype_interactivep Stream_interactivep[StreamType_Array];
lisp_streamtype_characterp Stream_characterp[StreamType_Array];
lisp_streamtype_binaryp Stream_binaryp[StreamType_Array];
lisp_streamtype_element_type Stream_element_type[StreamType_Array];
lisp_streamtype_external_format Stream_external_format[StreamType_Array];
lisp_streamtype_file_length Stream_file_length[StreamType_Array];
lisp_streamtype_file_position Stream_file_position[StreamType_Array];
lisp_streamtype_file_position_start Stream_file_position_start[StreamType_Array];
lisp_streamtype_file_position_end Stream_file_position_end[StreamType_Array];
lisp_streamtype_file_position_set Stream_file_position_set[StreamType_Array];
lisp_streamtype_file_charlen Stream_file_charlen[StreamType_Array];
lisp_streamtype_file_strlen Stream_file_strlen[StreamType_Array];
lisp_streamtype_listen Stream_listen[StreamType_Array];
lisp_streamtype_clear_input Stream_clear_input[StreamType_Array];
lisp_streamtype_finish_output Stream_finish_output[StreamType_Array];
lisp_streamtype_force_output Stream_force_output[StreamType_Array];
lisp_streamtype_clear_output Stream_clear_output[StreamType_Array];
lisp_streamtype_exitpoint Stream_exitpoint[StreamType_Array];
lisp_streamtype_termsize Stream_termsize[StreamType_Array];


/************************************************************
 *  strtype.c
 ************************************************************/

#define strvect_string_p(x) (GetType(x) == LISPTYPE_STRING)

/*
 *  string check
 */
int array_stringp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->dimension == 1 && str->type == ARRAY_TYPE_CHARACTER;
}

int strarrayp(addr pos)
{
	return arrayp(pos) && array_stringp(pos);
}

int stringp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING || strarrayp(pos);
}

int string_designer_p(addr pos)
{
	return stringp(pos) || symbolp(pos) || characterp(pos);
}

int string_base_p_(addr pos, int *ret)
{
	if (strvectp(pos))
		return strvect_base_p_(pos, ret);

	if (strarrayp(pos))
		return strarray_base_p_(pos, ret);

	return Result(ret, 0);
}

int string_simple_p(addr pos)
{
	if (strvectp(pos))
		return strvect_simple_p(pos);

	if (strarrayp(pos))
		return strarray_simple_p(pos);

	return 0;
}

int string_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	if (strvectp(pos))
		return strvect_character_type_(pos, ret);

	if (strarrayp(pos))
		return strarray_character_type_(pos, ret);

	*ret = CHARACTER_TYPE_INVALID;
	return fmte_("Invalid string object ~S.", pos, NULL);
}

int strarray_base_p_(addr pos, int *ret)
{
	enum CHARACTER_TYPE type;

	if (! strarrayp(pos))
		return Result(ret, 0);

	type = CHARACTER_TYPE_INVALID;
	Return(strarray_character_type_(pos, &type));
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return Result(ret, 1);

		default:
			return Result(ret, 0);
	}
}

int strarray_simple_p(addr pos)
{
	return strarrayp(pos) && array_simple_p(pos);
}

int strarray_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	enum CHARACTER_TYPE type;
	unicode c;
	size_t i, size;

	strarray_length(pos, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(pos, i, &c));
		type = unicode_character_type(type, c);
		if (type == CHARACTER_TYPE_INVALID)
			return fmte_("Invalid character code.", NULL);
	}

	return Result(ret, type);
}


/*
 *  strarray
 */
int strarray_alloc_(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Return(array_alloc_(local, &pos, 1, len));
	Return(array_character_alloc_(local, pos));
	return Result(ret, pos);
}

int strarray_local_(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Check(local == NULL, "local error");
	Return(array_local_(local, &pos, 1, len));
	Return(array_character_alloc_(local, pos));
	return Result(ret, pos);
}

int strarray_heap_(addr *ret, size_t len)
{
	addr pos;

	Return(array_heap_(&pos, 1, len));
	Return(array_character_alloc_(NULL, pos));
	return Result(ret, pos);
}

int strarray_char_alloc_(LocalRoot local, addr *ret, const char *arg)
{
	addr pos;
	size_t size, i;

	size = strlen(arg);
	Return(strarray_alloc_(local, &pos, size));
	for (i = 0; i < size; i++) {
		Return(strarray_setc_(pos, i, (unicode)arg[i]));
	}
	return Result(ret, pos);
}
int strarray_char_local_(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	return strarray_char_alloc_(local, ret, arg);
}
int strarray_char_heap_(addr *ret, const char *arg)
{
	return strarray_char_alloc_(NULL, ret, arg);
}

int strarray_size1_alloc_(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	addr pos;
	size_t i;

	Return(strarray_alloc_(local, &pos, size));
	for (i = 0; i < size; i++) {
		Return(strarray_setc_(pos, i, (unicode)arg[i]));
	}
	return Result(ret, pos);
}
int strarray_size1_local_(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_size1_alloc_(local, ret, arg, size);
}
int strarray_size1_heap_(addr *ret, const char *arg, size_t size)
{
	return strarray_size1_alloc_(NULL, ret, arg, size);
}

int strarray_sizeu_alloc_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	addr pos;
	size_t i;

	Return(strarray_alloc_(local, &pos, size));
	for (i = 0; i < size; i++) {
		Return(strarray_setc_(pos, i, arg[i]));
	}
	return Result(ret, pos);
}
int strarray_sizeu_local_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_sizeu_alloc_(local, ret, arg, size);
}
int strarray_sizeu_heap_(addr *ret, const unicode *arg, size_t size)
{
	return strarray_sizeu_alloc_(NULL, ret, arg, size);
}

void strarray_length(addr pos, size_t *ret)
{
	Check(! array_stringp(pos), "string type error");
	*ret = ArrayInfoStruct(pos)->front;
}

void strarray_length_buffer(addr pos, size_t *ret)
{
	Check(! array_stringp(pos), "string type error");
	*ret = ArrayInfoStruct(pos)->size;
}

int strarray_getc_(addr pos, size_t index, unicode *u)
{
	Check(! array_stringp(pos), "string type error");
	return array_get_unicode_(pos, index, u);
}

int strarray_setc_(addr pos, size_t index, unicode u)
{
	Check(! array_stringp(pos), "string type error");
	if (character_type(u) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	return array_set_character_(pos, index, u);
}

int strarray_equal_binary_(addr left, const unicode *right, size_t size, int *ret)
{
	unicode a, b;
	size_t check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equalp_binary_(addr left, const unicode *right, size_t size, int *ret)
{
	unicode a, b;
	size_t check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equal_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	if (size != strlen(right))
		return Result(ret, 0);
	body = (const byte *)right;
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equalp_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	if (size != strlen(right))
		return Result(ret, 0);
	body = (const byte *)right;
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equal_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size, check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	strarray_length(right, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equalp_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size, check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	strarray_length(right, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_character_equal_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_length(left, &size);
	if (size != 1)
		return Result(ret, 0);
	Return(strarray_getc_(left, 0, &a));
	GetCharacter(right, &b);
	return Result(ret, a == b);
}

int strarray_character_equalp_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_length(left, &size);
	if (size != 1)
		return Result(ret, 0);
	Return(strarray_getc_(left, 0, &a));
	GetCharacter(right, &b);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	return Result(ret, a == b);
}

int strarray_compare_binary_(addr left,
		const unicode *right, size_t size2, int *ret)
{
	unicode a, b;
	size_t size1, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_comparep_binary_(addr left,
		const unicode *right, size_t size2, int *ret)
{
	unicode a, b;
	size_t size1, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_compare_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	size2 = strlen(right);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	body = (const byte *)right;
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_comparep_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	size2 = strlen(right);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	body = (const byte *)right;
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_compare_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	strarray_length(right, &size2);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_comparep_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	strarray_length(right, &size2);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}


/*
 *  string
 */
static int copy_strvect_strarray_(addr vector, addr array, size_t size)
{
	unicode c;
	size_t i;

	Check(! strvectp(vector), "type error");
	Check(! strarrayp(array), "type error");
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(array, i, &c));
		Return(strvect_setc_(vector, i, c));
	}

	return 0;
}

int string_alloc_(LocalRoot local, addr *ret, addr pos)
{
	const unicode *body;
	addr vector;
	size_t size;

	if (strarrayp(pos)) {
		strarray_length(pos, &size);
		strvect_alloc(local, &vector, size);
		Return(copy_strvect_strarray_(vector, pos, size));
		return Result(ret, vector);
	}
	if (strvectp(pos)) {
		strvect_posbodylen(pos, &body, &size);
		return strvect_sizeu_alloc_(local, ret, body, size);
	}

	/* error */
	*ret = 0;
	return fmte_("type error.", NULL);
}
int string_local_(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	return string_alloc_(local, ret, pos);
}

int string_heap_(addr *ret, addr pos)
{
	return string_alloc_(NULL, ret, pos);
}

int strvect_value_heap_(addr *ret, addr pos)
{
	addr dst;
	unicode c;
	size_t size, i;

	if (GetType(pos) == LISPTYPE_STRING)
		return Result(ret, pos);
	string_length(pos, &size);
	strvect_heap(&dst, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(dst, i, c));
	}

	return Result(ret, dst);
}

void string_length(addr pos, size_t *ret)
{
	if (strvect_string_p(pos)) {
		strvect_length(pos, ret);
		return;
	}
	if (strarrayp(pos)) {
		strarray_length(pos, ret);
		return;
	}
	*ret = 0;
	Abort("type error.");
}

int string_getc_(addr pos, size_t index, unicode *u)
{
	if (strvect_string_p(pos)) {
		strvect_getc(pos, index, u);
		return 0;
	}
	if (strarrayp(pos))
		return strarray_getc_(pos, index, u);
	*u = 0;
	return fmte_("Argument ~S must be a string type.", pos, NULL);
}

int string_setc_(addr pos, size_t index, unicode u)
{
	if (strvect_string_p(pos))
		return strvect_setc_(pos, index, u);
	if (strarrayp(pos))
		return strarray_setc_(pos, index, u);

	return fmte_("Argument ~S must be a string type.", pos, NULL);
}

int string_equal_binary_(addr left, const unicode *right, size_t len, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equal_binary(left, right, len));
	if (strarrayp(left))
		return strarray_equal_binary_(left, right, len, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_equalp_binary_(addr left, const unicode *right, size_t len, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equalp_binary(left, right, len));
	if (strarrayp(left))
		return strarray_equalp_binary_(left, right, len, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_equal_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equal_char(left, right));
	if (strarrayp(left))
		return strarray_equal_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_equalp_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equalp_char(left, right));
	if (strarrayp(left))
		return strarray_equalp_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int stringp_equal_char_(addr left, const char *right, int *ret)
{
	if (! stringp(left))
		return Result(ret, 0);
	else
		return string_equal_char_(left, right, ret);
}

int stringp_equalp_char_(addr left, const char *right, int *ret)
{
	if (! stringp(left))
		return Result(ret, 0);
	else
		return string_equalp_char_(left, right, ret);
}

int string_equalp_char_va_(addr pos, int *ret, ...)
{
	int check, value;
	va_list args;
	const char *str;

	va_start(args, ret);
	value = 0;
	for (;;) {
		str = va_arg(args, const char *);
		if (str == NULL)
			break;
		Return(string_equalp_char_(pos, str, &check));
		if (check) {
			value = 1;
			break;
		}
	}
	va_end(args);

	return Result(ret, value);
}

static int strarray_strvect_equal_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type right error");
	Check(! strvect_string_p(right), "type left error");
	strvect_posbodylen(right, &body, &size);

	return strarray_equal_binary_(left, body, size, ret);
}

int string_equal_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_equal(left, right));
		if (strarrayp(right))
			return strarray_strvect_equal_(right, left, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_equal_(left, right, ret);
		if (strarrayp(right))
			return strarray_equal_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

static int strarray_strvect_equalp_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type right error");
	Check(! strvect_string_p(right), "type left error");
	strvect_posbodylen(right, &body, &size);

	return strarray_equalp_binary_(left, body, size, ret);
}

int string_equalp_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_equalp(left, right));
		if (strarrayp(right))
			return strarray_strvect_equalp_(right, left, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_equalp_(left, right, ret);
		if (strarrayp(right))
			return strarray_equalp_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_character_equal_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_character_equal(left, right));
	if (strarrayp(left))
		return strarray_character_equal_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_character_equalp_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_character_equalp(left, right));
	if (strarrayp(left))
		return strarray_character_equalp_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_compare_binary_(addr left, const unicode *right, size_t size2, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_compare_binary(left, right, size2));
	if (strarrayp(left))
		return strarray_compare_binary_(left, right, size2, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_comparep_binary_(addr left, const unicode *right, size_t size2, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_comparep_binary(left, right, size2));
	if (strarrayp(left))
		return strarray_comparep_binary_(left, right, size2, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_compare_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_compare_char(left, right));
	if (strarrayp(left))
		return strarray_compare_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_comparep_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_comparep_char(left, right));
	if (strarrayp(left))
		return strarray_comparep_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

static int strarray_strvect_compare_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_compare_binary_(left, body, size, ret);
}

static int strvect_strarray_compare_(addr left, addr right, int *ret)
{
	int check;
	Return(strarray_strvect_compare_(right, left, &check));
	return Result(ret, -check);
}

static int strarray_strvect_comparep_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_comparep_binary_(left, body, size, ret);
}

static int strvect_strarray_comparep_(addr left, addr right, int *ret)
{
	int check;
	Return(strarray_strvect_comparep_(right, left, &check));
	return Result(ret, -check);
}

int string_compare_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_compare(left, right));
		if (strarrayp(right))
			return strvect_strarray_compare_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_compare_(left, right, ret);
		if (strarrayp(right))
			return strarray_compare_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_comparep_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_comparep(left, right));
		if (strarrayp(right))
			return strvect_strarray_comparep_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_comparep_(left, right, ret);
		if (strarrayp(right))
			return strarray_comparep_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_designer_equal_(addr left, addr right, int *ret)
{
	int check1, check2, check3, check4;

	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (symbolp(right))
		GetNameSymbol(right, &right);
	check1 = stringp(left);
	check2 = stringp(right);
	check3 = characterp(left);
	check4 = characterp(right);
	if (check1 && check2)
		return string_equal_(left, right, ret);
	if (check3 && check4)
		return Result(ret, character_equal(left, right));
	if (check1 && check4)
		return string_character_equal_(left, right, ret);
	if (check2 && check3)
		return string_character_equal_(right, left, ret);

	return Result(ret, 0);
}

int string_designer_equal_char_(addr left, const char *right, int *ret)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return Result(ret, character_equal_char(left, right));
	if (stringp(left))
		return string_equal_char_(left, right, ret);

	return Result(ret, 0);
}

int string_designer_equalp_(addr left, addr right, int *ret)
{
	int check1, check2, check3, check4;

	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (symbolp(right))
		GetNameSymbol(right, &right);
	check1 = stringp(left);
	check2 = stringp(right);
	check3 = characterp(left);
	check4 = characterp(right);
	if (check1 && check2)
		return string_equalp_(left, right, ret);
	if (check3 && check4)
		return Result(ret, character_equalp(left, right));
	if (check1 && check4)
		return string_character_equalp_(left, right, ret);
	if (check2 && check3)
		return string_character_equalp_(right, left, ret);

	return Result(ret, 0);
}

int string_designer_equalp_char_(addr left, const char *right, int *ret)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return Result(ret, character_equalp_char(left, right));
	if (stringp(left))
		return string_equalp_char_(left, right, ret);

	return Result(ret, 0);
}

int string_designer_alloc_(LocalRoot local, addr *value, addr pos, int *ret)
{
	addr type;

	if (stringp(pos)) {
		*value = pos;
		if (ret)
			*ret = 1;
		return 0;
	}
	if (symbolp(pos)) {
		GetNameSymbol(pos, value);
		if (ret)
			*ret = 1;
		return 0;
	}
	if (characterp(pos)) {
		Return(strvect_character_alloc_(local, value, pos));
		if (ret)
			*ret = 1;
		return 0;
	}

	if (ret) {
		*ret = 0;
		return 0;
	}

	/* error */
	GetTypeTable(&type, StringDesigner);
	return call_type_error_va_(NULL, pos, type,
			"The object ~S is not string-designer.", pos, NULL);
}

int string_designer_local_(LocalRoot local, addr *value, addr pos, int *ret)
{
	Check(local == NULL, "local error");
	return string_designer_alloc_(local, value, pos, ret);
}

int string_designer_heap_(addr *value, addr pos, int *ret)
{
	return string_designer_alloc_(NULL, value, pos, ret);
}

int string_designer_string(addr *value, addr pos)
{
	if (stringp(pos)) {
		*value = pos;
		return 1;
	}
	if (symbolp(pos)) {
		GetNameSymbol(pos, value);
		return 1;
	}

	return 0;
}


/*
 *  concatenate
 */
int string_concat_heap_(addr *ret, addr a, addr b)
{
	unicode u;
	addr c;
	size_t x, y, i;

	Check(! stringp(a), "type error");
	Check(! stringp(b), "type error");
	string_length(a, &x);
	string_length(b, &y);
	strvect_heap(&c, x + y);
	for (i = 0; i < x; i++) {
		Return(string_getc_(a, i, &u));
		Return(string_setc_(c, i, u));
	}
	for (i = 0; i < y; i++) {
		Return(string_getc_(b, i, &u));
		Return(string_setc_(c, i+x, u));
	}

	return Result(ret, c);
}

int string_concat_hyphen_heap_(addr *ret, addr a, addr b)
{
	unicode u;
	addr c;
	size_t x, y, i;

	Check(! stringp(a), "type error");
	Check(! stringp(b), "type error");
	string_length(a, &x);
	string_length(b, &y);
	strvect_heap(&c, x + y + 1UL);
	for (i = 0; i < x; i++) {
		Return(string_getc_(a, i, &u));
		Return(string_setc_(c, i, u));
	}
	Return(string_setc_(c, i, (unicode)'-'));
	for (i = 0; i < y; i++) {
		Return(string_getc_(b, i, &u));
		Return(string_setc_(c, i+x+1UL, u));
	}

	return Result(ret, c);
}

int string_concat_char1_heap_(addr *ret, const char *str, addr b)
{
	const byte *a;
	addr c;
	unicode u;
	size_t x, y, i;

	Check(! stringp(b), "type error");
	x = strlen(str);
	a = (const byte *)str;
	string_length(b, &y);
	strvect_heap(&c, x + y);
	for (i = 0; i < x; i++)
		Return(string_setc_(c, i, (unicode)a[i]));
	for (i = 0; i < y; i++) {
		Return(string_getc_(b, i, &u));
		Return(string_setc_(c, i+x, u));
	}

	return Result(ret, c);
}

int string_concat_char2_heap_(addr *ret, addr a, const char *str)
{
	const byte *b;
	addr c;
	unicode u;
	size_t x, y, i;

	Check(! stringp(a), "type error");
	string_length(a, &x);
	y = strlen(str);
	b = (const byte *)str;
	strvect_heap(&c, x + y);
	for (i = 0; i < x; i++) {
		Return(string_getc_(a, i, &u));
		Return(string_setc_(c, i, u));
	}
	for (i = 0; i < y; i++) {
		Return(string_setc_(c, i+x, (unicode)b[i]));
	}

	return Result(ret, c);
}


/*
 *  case
 */
int string_upper_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	Check(! stringp(pos), "type error");
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (isLowerCase(c))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int string_lower_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	Check(! stringp(pos), "type error");
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (isUpperCase(c))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int string_upper_alloc_(LocalRoot local, addr pos, addr *ret)
{
	unicode c;
	addr dst;
	size_t size, i;

	string_length(pos, &size);
	strvect_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(dst, i, toUpperUnicode(c)));
	}

	return Result(ret, dst);
}

int string_upper_local_(LocalRoot local, addr pos, addr *ret)
{
	CheckLocal(local);
	return string_upper_alloc_(local, pos, ret);
}

int string_upper_heap_(addr pos, addr *ret)
{
	return string_upper_alloc_(NULL, pos, ret);
}

int string_lower_alloc_(LocalRoot local, addr pos, addr *ret)
{
	unicode c;
	addr dst;
	size_t size, i;

	string_length(pos, &size);
	strvect_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(dst, i, toLowerUnicode(c)));
	}

	return Result(ret, dst);
}

int string_lower_local_(LocalRoot local, addr pos, addr *ret)
{
	CheckLocal(local);
	return string_lower_alloc_(local, pos, ret);
}

int string_lower_heap_(addr pos, addr *ret)
{
	return string_lower_alloc_(NULL, pos, ret);
}


/*
 *  debug
 */
int string_equal_char_debug(addr left, const char *right)
{
	int check;
	check = 0;
	Error(string_equal_char_(left, right, &check));
	return check;
}

int string_equalp_char_debug(addr left, const char *right)
{
	int check;
	check = 0;
	Error(string_equalp_char_(left, right, &check));
	return check;
}

int string_equal_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(string_equal_(left, right, &check));
	return check;
}


/************************************************************
 *  structure.c
 ************************************************************/

/*
 *  control
 */
int structure_class_p_(addr pos, int *ret)
{
	addr check;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	Return(clos_class_of_(pos, &pos));
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	return clos_subclass_p_(pos, check, ret);
}

int structure_class_p_debug(addr pos)
{
	int check;
	check = 0;
	Error(structure_class_p_(pos, &check));
	return check;
}

int structure_instance_p_(addr pos, int *ret)
{
	addr right;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	Return(clos_class_of_(pos, &pos));
	GetConst(CLOS_STRUCTURE_OBJECT, &right);
	return clos_subclass_p_(pos, right, ret);
}

int structure_instance_p_debug(addr pos)
{
	int check;
	check = 0;
	Error(structure_instance_p_(pos, &check));
	return check;
}

static int equalcall_structure_(addr a, addr b, int *ret,
		int (*call)(addr, addr, int *))
{
	int check;
	addr c, d;
	size_t x, y;

	/* class-of */
	if (! closp(a))
		return Result(ret, 0);
	if (! closp(b))
		return Result(ret, 0);
	Return(clos_class_of_(a, &c));
	Return(clos_class_of_(b, &d));
	if (c != d)
		return Result(ret, 0);
	GetConst(CLOS_STRUCTURE_OBJECT, &d);
	Return(clos_subclass_p_(c, d, &check));
	if (! check)
		return Result(ret, 0);

	/* slots */
	GetValueClos(a, &a);
	GetValueClos(b, &b);
	LenClosValue(a, &x);
	LenClosValue(b, &y);
	if (x != y)
		return Result(ret, 0);
	for (x = 0; x < y; x++) {
		GetClosValue(a, x, &c);
		GetClosValue(b, x, &d);
		Return((*call)(c, d, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int equalp_structure_(addr a, addr b, int *ret)
{
	return equalcall_structure_(a, b, ret, equalp_function_);
}

int equalrt_structure_(addr a, addr b, int *ret)
{
	return equalcall_structure_(a, b, ret, equalrt_function_);
}

int typep_structure_(addr value, addr instance, int *ret)
{
	int check;

	Check(! structure_class_p_debug(instance), "type error");
	Return(structure_instance_p_(value, &check));
	if (! check)
		return Result(ret, 0);
	else
		return clos_subtype_p_(value, instance, ret);
}


/*
 *  structure-constructor
 */
static int make_structure_common_clos_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, value;
	LocalHold hold;

	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	Return(stdget_structure_slots_(instance, &value));
	SetSlotStructureType(pos, value);
	SetErrorpStructureType(pos, errorp);

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure_clos_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

static int make_structure_common_vector_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, slots, value, type, name, named;
	size_t size;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	Return(stdget_structure_slots_(instance, &slots));
	Return(stdget_structure_value_(instance, &value));
	Return(stdget_structure_vector_(instance, &type));
	Return(stdget_structure_name_(instance, &name));
	Return(stdget_structure_named_(instance, &named));
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	SetVectorStructureType(pos, type);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != Nil);
	str->size = size;
	Return(getindex_integer_(value, &size));
	str->size_value = size;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure_vector_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

static int make_structure_common_list_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, slots, value, name, named;
	size_t size;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	Return(stdget_structure_slots_(instance, &slots));
	Return(stdget_structure_value_(instance, &value));
	Return(stdget_structure_name_(instance, &name));
	Return(stdget_structure_named_(instance, &named));
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != Nil);
	str->size = size;
	Return(getindex_integer_(value, &size));
	str->size_value = size;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure_list_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

static int make_structure_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr type, check;

	/* clos */
	Return(stdget_structure_type_(instance, &type));
	GetConst(COMMON_CLASS, &check);
	if (type == check)
		return make_structure_common_clos_(ptr, ret, instance, rest, errorp, initp);

	/* vector */
	GetConst(COMMON_VECTOR, &check);
	if (type == check)
		return make_structure_common_vector_(ptr, ret, instance, rest, errorp, initp);

	/* list */
	GetConst(COMMON_LIST, &check);
	if (type == check)
		return make_structure_common_list_(ptr, ret, instance, rest, errorp, initp);

	/* error */
	return fmte_("Invalid type value ~S.", type, NULL);
}

int structure_constructor_common(Execute ptr, addr symbol, addr rest, addr *ret)
{
	int check;
	addr instance;

	if (! symbolp(symbol))
		return fmte_("The first argument ~S must be a symbol type.", symbol, NULL);
	Return(clos_find_class_(symbol, &instance));
	Return(structure_class_p_(instance, &check));
	if (! check)
		return fmte_("The class ~S don't be a structure-class.", symbol, NULL);

	return make_structure_common_(ptr, ret, instance, rest, 0, 1);
}

int allocate_instance_structure_(Execute ptr, addr clos, addr *ret)
{
	return make_structure_common_(ptr, ret, clos, Nil, 1, 0);
}

int make_instance_structure(Execute ptr, addr rest, addr *ret)
{
	addr instance;
	Return_getcons(rest, &instance, &rest);
	return make_structure_common_(ptr, ret, instance, rest, 1, 1);
}


/*
 *  copy-structure
 */
void copy_structure_common(addr inst, addr *ret)
{
	addr class_of, slots, clos, src, dst, pos;
	size_t size, i;

	Check(! structure_instance_p_debug(inst), "type error");
	/* source */
	GetClassOfClos(inst, &class_of);
	GetSlotClos(inst, &slots);
	GetValueClos(inst, &src);
	/* destination */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, class_of);
	GetValueClos(clos, &dst);
	/* value */
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetClosValue(src, i, &pos);
		SetClosValue(dst, i, pos);
	}
	/* result */
	*ret = clos;
}


/*
 *  initialize
 */
void init_structure(void)
{
	init_structure_define();
}


/************************************************************
 *  structure_define.c
 ************************************************************/

static int ensure_structure_constructor_(addr args, addr *result, int *ret)
{
	addr key, value, keyword, root;

	GetConst(KEYWORD_CONSTRUCTOR, &keyword);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &key, &args);
		Return_getcons(args, &value, &args);
		if (key != keyword)
			continue;
		cons_heap(&root, value, root);
	}
	nreverse(result, root);

	return Result(ret, root != Nil);
}

static int ensure_structure_struct_(struct defstruct *str,
		Execute ptr, addr name, addr slots, addr args)
{
	int check;
	addr pos, value;

	defstruct_clean(str);
	str->ptr = ptr;
	str->slots = slots;
	str->name = name;
	/* :documentation */
	if (GetKeyArgs(args, KEYWORD_DOCUMENTATION, &pos)) pos = Nil;
	str->doc = pos;
	/* :conc-name */
	if (! GetKeyArgs(args, KEYWORD_CONC_NAME, &pos)) {
		str->conc_name_p = 1;
		str->conc_name = pos;
	}
	/* :type */
	if (! GetKeyArgs(args, KEYWORD_TYPE, &pos)) {
		GetConst(COMMON_LIST, &value);
		if (pos == value) {
			str->type_list_p = 1;
		}
		else {
			str->type_vector_p = 1;
			str->type_vector = pos;
		}
		str->type_p = 1;
	}
	/* :initial-offset */
	if (! GetKeyArgs(args, KEYWORD_INITIAL_OFFSET, &pos)) {
		str->initial_offset_p = 1;
		str->initial_offset = pos;
		Return(getindex_integer_(pos, &(str->offset)));
	}
	/* :named */
	if (! GetKeyArgs(args, KEYWORD_NAMED, &pos)) {
		str->named_p = (pos != Nil);
		str->offset++;
	}
	/* :copier */
	if (! GetKeyArgs(args, KEYWORD_COPIER, &pos)) {
		str->copier_p = 1;
		str->copier = pos;
	}
	/* :predicate */
	if (! GetKeyArgs(args, KEYWORD_PREDICATE, &pos)) {
		str->predicate_p = 1;
		str->predicate = pos;
	}
	/* :include */
	if (! GetKeyArgs(args, KEYWORD_INCLUDE, &pos)) {
		if (! consp(pos))
			return fmte_("Invalid :include format ~S.", pos, NULL);
		GetCons(pos, &pos, &value);
		str->include_p = 1;
		str->iname = pos;
		str->iargs = value;
	}
	/* :print-object */
	if (! GetKeyArgs(args, KEYWORD_PRINT_OBJECT, &pos)) {
		str->print_object_p = 1;
		str->print_object = pos;
	}
	/* :print-function */
	if (! GetKeyArgs(args, KEYWORD_PRINT_FUNCTION, &pos)) {
		str->print_function_p = 1;
		str->print_function = pos;
	}
	/* :constructor */
	Return(ensure_structure_constructor_(args, &pos, &check));
	if (check) {
		str->constructor_p = 1;
		str->constructor = pos;
	}

	return 0;
}


/*
 *  check-instance
 */
static int structure_check_name_(struct defstruct *ptr)
{
	addr pos;

	clos_find_class_nil(ptr->name, &pos);
	if (pos != Nil) {
		Return(fmtw_("The structure ~S already exists.", ptr->name, NULL));
	}

	return 0;
}

static int structure_slots_heap_(addr list, addr *ret)
{
	addr pos, name, init, type, readonly, root;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(list_bind_(pos, &name, &init, &type, &readonly, NULL));
		slot_heap(&pos);
		SetNameSlot(pos, name);
		SetTypeSlot(pos, type);
		SetFunctionSlot(pos, init);
		SetReadOnlySlot(pos, readonly);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int structure_check_slots_(addr list)
{
	int check;
	addr pos, a, b, tail;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &a);
		Check(! symbolp(a), "type error");
		GetNameSymbol(a, &a);
		for (tail = list; tail != Nil; ) {
			Return_getcons(tail, &pos, &tail);
			GetNameSlot(pos, &b);
			Check(! symbolp(b), "type error");
			GetNameSymbol(b, &b);
			Return(string_equal_(a, b, &check));
			if (check) {
				return fmte_("The slot name ~S "
						"is duplicated in the defstruct.", a, NULL);
			}
		}
	}

	return 0;
}

static int structure_check_predicate_(struct defstruct *str)
{
	if (str->type_p && (! str->named_p)) {
		/* no-predicate */
		if (! str->predicate_p) {
			str->predicate_p = 1;
			str->predicate = Nil;
			return 0;
		}
		if (str->predicate == Nil) {
			return 0;
		}
		return fmte_("DEFSTRUCT ~S is defined :PREDICATE, "
				"but the structure is not named.", str->name, NULL);
	}
	if (str->predicate_p && str->predicate == T) {
		str->predicate_p = 0;
		return 0;
	}

	return 0;
}

static int structure_include_(struct defstruct *str)
{
	int check;
	addr instance, x, y;

	if (! str->include_p)
		return 0;
	/* instance check */
	clos_find_class_nil(str->iname, &instance);
	if (instance == Nil)
		return fmte_(":INCLUDE ~S structure don't exist.", str->iname, NULL);
	Return(structure_class_p_(instance, &check));
	if (! check)
		return fmte_(":INCLUDE ~S must be structure type.", instance, NULL);

	/* class check */
	Return(stdget_structure_type_(instance, &x));
	GetConst(COMMON_CLASS, &y);
	if (x == y) {
		if (str->type_list_p || str->type_vector_p) {
			return fmte_(":TYPE option is CLASS, "
					"but :INCLUDE type is not CLASS.", NULL);
		}
	}

	/* list check */
	GetConst(COMMON_LIST, &y);
	if (x == y) {
		if (! str->type_list_p)
			return fmte_(":TYPE option is LIST, but :INCLUDE type is not LIST.", NULL);
	}

	/* vector check */
	GetConst(COMMON_VECTOR, &y);
	if (x == y) {
		if (! str->type_vector_p) {
			return fmte_(":TYPE option is VECTOR, "
					"but :INCLUDE type is not VECTOR.", NULL);
		}
		x = str->type_vector;
		Return(stdget_structure_vector_(instance, &y));
		Return(subtypep_check_(str->ptr, x, y, Nil, &check, NULL));
		if (! check) {
			return fmte_(":TYPE ~A is not in the include ~A type.", x, y, NULL);
		}
	}

	/* instance */
	str->iname = instance;
	return 0;
}

static int structure_find_slots_(addr instance, addr name, addr *ret)
{
	int check;
	addr slots, pos, value;
	size_t size, i;

	Check(! structure_class_p_debug(instance), "type error");
	Check(! symbolp(name), "type error");

	/* find */
	GetNameSymbol(name, &name);
	Return(stdget_structure_slots_(instance, &slots));
	Check(! slot_vector_p(slots), "type error");
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return Result(ret, Unbound);
}

static int structure_include_slots_(struct defstruct *str)
{
	addr name, list, pos, instance;

	if (! str->include_p)
		return 0;
	instance = str->iname;
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		Return(structure_find_slots_(instance, name, &pos));
		if (pos != Unbound) {
			return fmte_("The slot ~S "
					"already exist in :INCLUDE structure.", name, NULL);
		}
	}

	return 0;
}

static int structure_include_arguments_(struct defstruct *str)
{
	int result;
	addr name, list, instance, a, b, x, y, gensym;

	if (! str->include_p)
		return 0;
	instance = str->iname;
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	for (list = str->iargs; list != Nil; ) {
		GetCons(list, &a, &list);
		GetNameSlot(a, &name);
		Return(structure_find_slots_(instance, name, &b));
		if (b == Unbound) {
			return fmte_("The :include argument ~S don't exist "
					"in :INCLUDE structure.", name, NULL);
		}
		/* form */
		GetFunctionSlot(a, &x);
		if (x == gensym) {
			GetFunctionSlot(b, &y);
			SetFunctionSlot(a, y);
		}
		/* type */
		GetTypeSlot(a, &x);
		GetTypeSlot(b, &y);
		if (x == gensym) {
			SetTypeSlot(a, y);
		}
		else {
			Return(subtypep_check_(str->ptr, x, y, Nil, &result, NULL));
			if (! result) {
				return fmte_("The slot ~S type ~A is not "
						"in the include ~A type.", name, x, y, NULL);
			}
		}
		/* readonly */
		GetReadOnlySlot(a, &x);
		GetReadOnlySlot(b, &y);
		if (x == gensym) {
			SetReadOnlySlot(a, y);
		}
		else if (x == Nil && y == T) {
			return fmte_("The slot ~S is readonly "
					"but include slot is not readonly.", name, NULL);
		}
	}

	return 0;
}

static int structure_print_check_(struct defstruct *str)
{
	if (str->print_function_p && str->print_object_p) {
		return fmte_("The defstruct option must be have "
				"either :PRINT-OBJECT or :PRINT-FUNCTION, "
				"but there are both options", NULL);
	}

	return 0;
}

static void structure_slots_value(struct defstruct *str)
{
	addr list, pos, check, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		/* init */
		GetFunctionSlot(pos, &check);
		if (check == g) {
			SetFunctionSlot(pos, Nil);
		}
		/* type */
		GetTypeSlot(pos, &check);
		if (check == g) {
			GetTypeTable(&check, T);
			SetTypeSlot(pos, check);
		}
		/* readonly */
		GetReadOnlySlot(pos, &check);
		if (check == g) {
			SetReadOnlySlot(pos, Nil);
		}
	}
}


/*
 *  make-instance
 */
static int structure_instance_include_(struct defstruct *str, addr instance)
{
	addr list, pos;

	/* include */
	if (str->include_p) {
		Return(stdset_structure_include_(instance, str->iname));
	}

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		Return(stdget_structure_include_(pos, &pos));
	}
	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	cons_heap(&list, pos, list);
	GetConst(CLOS_T, &pos);
	cons_heap(&list, pos, list);
	nreverse(&list, list);
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

static int structure_instance_(struct defstruct *str)
{
	addr clos, instance, pos;

	/* structure */
	GetConst(CLOS_STRUCTURE_CLASS, &clos);
	Return(clos_instance_heap_(clos, &instance));
	SetClassOfClos(instance, clos);
	/* name */
	Return(stdset_structure_name_(instance, str->name));
	/* documentation */
	if (str->doc != Nil) {
		Return(stdset_structure_documentation_(instance, str->doc));
	}
	/* include, precedence-list */
	Return(structure_instance_include_(str, instance));
	/* type */
	if (str->type_list_p) {
		GetConst(COMMON_LIST, &pos);
		Return(stdset_structure_type_(instance, pos));
	}
	else if (str->type_vector_p) {
		GetConst(COMMON_VECTOR, &pos);
		Return(stdset_structure_type_(instance, pos));
		Return(stdset_structure_vector_(instance, str->type_vector));
	}
	else {
		GetConst(COMMON_CLASS, &pos);
		Return(stdset_structure_type_(instance, pos));
	}
	/* named */
	Return(stdset_structure_named_(instance, str->named_p? T: Nil));
	/* result */
	str->instance = instance;

	return 0;
}


/*
 *  slots-make
 */
static int structure_pushnew_local_(LocalRoot local,
		addr *push, addr value, addr list, int *ret)
{
	int check;
	addr root, x;

	Check(! stringp(value), "type error");
	for (root = list; list != Nil; ) {
		GetCons(list, &x, &list);
		Return(string_equal_(x, value, &check));
		if (check)
			return Result(ret, 0);
	}
	cons_local(local, push, value, root);

	return Result(ret, 1);
}

static int structure_find_slotslist_(addr name, addr list, addr *ret)
{
	int check;
	addr pos, value;

	Check(! stringp(name), "type error");
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return 0;
}

static int structure_slots_make_(struct defstruct *str)
{
	int check;
	addr root, slots, list, pos, name, args, instance;
	LocalRoot local;
	LocalStack stack;
	size_t size, value, i, count;

	local = str->ptr->local;
	push_local(local, &stack);
	root = slots = Nil;
	size = value = 0;
	/* include */
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_slots_(pos, &list));
		Return(stdget_structure_value_(pos, &pos));
		if (pos != Nil) {
			Return(getindex_integer_(pos, &value));
		}
		args = str->iargs;
		LenSlotVector(list, &count);
		for (i = 0; i < count; i++) {
			GetSlotVector(list, i, &pos);
			slot_copy_heap(&pos, pos);
			GetNameSlot(pos, &name);
			GetNameSymbol(name, &name);
			cons_local(local, &root, name, root);
			Return(structure_find_slotslist_(name, args, &pos));
			Check(! slotp(pos), "type error");
			cons_local(local, &slots, pos, slots);
			/* location */
			SetLocationSlot(pos, size++);
		}
	}
	/* slots */
	if (str->named_p)
		str->named_index = value;
	value += str->offset;
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_pushnew_local_(local, &root, name, root, &check));
		if (check) {
			cons_local(local, &slots, pos, slots);
			SetLocationSlot(pos, size++);
			SetAccessSlot(pos, value++);
		}
	}
	/* array */
	instance = str->instance;
	slot_vector_heap(&list, size);
	while (slots != Nil) {
		GetCons(slots, &pos, &slots);
		GetLocationSlot(pos, &i);
		SetClassSlot(pos, instance);
		SetSlotVector(list, i, pos);
	}
	/* result */
	str->size = size;
	str->size_value = value;
	str->slots = list;
	Return(stdset_structure_slots_(instance, list));
	Return(stdset_structure_value_(instance, intsizeh(value)));
	Return(stdset_structure_named_index_(instance,
				str->named_p? intsizeh(str->named_index): Nil));
	rollback_local(local, stack);

	return 0;
}


/*
 *  accessor
 */
static int structure_slot_callname_(struct defstruct *str, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	GetNameSlot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (str->conc_name == Unbound) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_hyphen_heap_(ret, name, pos));
	}
	else if (str->conc_name == Nil) {
		*ret = pos;
	}
	else {
		Check(! stringp(str->conc_name), "type error");
		Return(string_concat_heap_(ret, str->conc_name, pos));
	}

	return 0;
}

/* list */
static int function_structure_reader_list(Execute ptr, addr var)
{
	int check;
	addr type;
	size_t index;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(type, &type);
	GetAccessSlot(type, &index);
	getnth_unsafe(var, index, &var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_reader_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_reader_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader_list);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_reader_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_structure_writer_list(Execute ptr, addr value, addr var)
{
	int check;
	addr type;
	size_t index;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(type, &type);
	GetAccessSlot(type, &index);
	setnth_unsafe(var, index, value);
	setresult_control(ptr, value);

	return 0;
}

static void structure_type_slot_writer_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_writer_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer_list);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_writer_list(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static int structure_slot_readonly_p(addr slot)
{
	GetReadOnlySlot(slot, &slot);
	return slot != Nil;
}

static int structure_slots_call_list_(struct defstruct *str)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		structure_type(str, pos, &type);
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_list(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_list(type, symbol);
	}

	return 0;
}

/* vector */
static int function_structure_reader_vector(Execute ptr, addr var)
{
	int check;
	addr type, slot, pos;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(type, &slot);
	GetVectorStructureType(type, &pos);
	Return(structure_getarray_(ptr, var, slot, pos, &var));
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_reader_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_reader_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader_vector);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_reader_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_structure_writer_vector(Execute ptr, addr value, addr var)
{
	int check;
	addr type, slot;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(type, &slot);
	GetVectorStructureType(type, &type);
	Return(structure_setarray_(ptr, var, slot, type, value));
	setresult_control(ptr, value);

	return 0;
}

static void structure_type_slot_writer_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_writer_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer_vector);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_writer_vector(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static int structure_slots_call_vector_(struct defstruct *str)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		structure_type(str, pos, &type);
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_vector(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_vector(type, symbol);
	}

	return 0;
}

/* clos */
static int function_structure_reader_clos(Execute ptr, addr var)
{
	int check;
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The reader don't read ~S structure.", pos, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	GetClosValue(var, index, &var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_reader_clos(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_reader_clos(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	structure_type_slot_reader_clos(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_structure_writer_clos(Execute ptr, addr value, addr var)
{
	int check;
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The reader don't read ~S structure.", pos, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	SetClosValue(var, index, value);
	setresult_control(ptr, value);

	return 0;
}

static void structure_type_slot_writer_clos(addr *ret, addr instance)
{
	addr args, values;

	GetTypeTable(&args, T);
	type_clos_heap(instance, &values);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_writer_clos(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer_clos);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	structure_type_slot_writer_clos(&type, instance);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static int structure_slots_call_clos_(struct defstruct *str)
{
	addr instance, package, slots, pos, symbol;
	size_t size, i;

	instance = str->instance;
	Check(! structure_class_p_debug(instance), "type error");
	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_clos(instance, pos, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_clos(instance, pos, symbol);
	}

	return 0;
}

/* call */
static int structure_slots_call_(struct defstruct *str)
{
	if (str->type_list_p)
		return structure_slots_call_list_(str);
	else if (str->type_vector_p)
		return structure_slots_call_vector_(str);
	else
		return structure_slots_call_clos_(str);
}


/*
 *  constructor
 */
static int structure_constructor_find_slots_(addr key, addr slots, int *ret)
{
	int check;
	addr value;
	size_t size, i;

	Check(! symbolp(key), "type error");
	Check(! slot_vector_p(slots), "type error");
	GetNameSymbol(key, &key);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(key, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int structure_constructor_dynamic_p_(
		int errorp, addr key, addr slots, int *ret)
{
	int check;

	if (! errorp)
		return Result(ret, 0);
	Return(structure_constructor_find_slots_(key, slots, &check));
	return Result(ret, ! check);
}

static int structure_constructor_dynamic_(addr instance,
		addr slots, addr list, int errorp)
{
	int check;
	addr key;

	while (list != Nil) {
		if (! consp(list))
			return fmte_("Invalid keyword-argumets ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			return fmte_("There is no value in the key ~S arguemnts.", key, NULL);
		if (! symbolp(key))
			return fmte_("The key ~S must be a symbol type.", key, NULL);
		Return(structure_constructor_dynamic_p_(errorp, key, slots, &check));
		if (check) {
			return fmte_("There is no slot ~S "
					"in the structure ~S.", key, instance, NULL);
		}
		GetCdr(list, &list);
	}

	return 0;
}

static int function_structure_constructor_find_(
		addr key, addr list, addr *value, int *ret)
{
	int check;
	addr left, right, g;

	Check(! slotp(key), "type error");
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	GetNameSlot(key, &key);
	GetNameSymbol(key, &key);
	while (list != Nil) {
		GetCons(list, &left, &list);
		GetCons(list, &right, &list);
		Check(! symbolp(left), "type error");
		if (right == g)
			continue;
		GetNameSymbol(left, &left);
		Return(string_equal_(key, left, &check));
		if (check) {
			*value = right;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

/* list */
static int structure_constructor_instance_list_(Execute ptr,
		addr list, addr slots, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i, index;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetAccessSlot(slot, &index);
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (! check) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
			}
		}
		setnth_unsafe(list, index, pos);
	}

	return 0;
}

static void make_structure_nil(addr *ret, size_t size)
{
	addr list;
	size_t i;

	list = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&list, Nil, list);
	*ret = list;
}

int make_structure_list_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, list, name;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	/* make */
	Return(structure_constructor_dynamic_(instance, slots, args, str->errorp));
	make_structure_nil(&list, str->size_value);

	if (initp) {
		hold = LocalHold_local_push(ptr, list);
		Return(structure_constructor_instance_list_(ptr, list, slots, args));
		localhold_end(hold);
	}

	if (str->named) {
		GetNameStructureType(pos, &name);
		Return(setnth_(list, str->named_index, name));
	}

	return Result(ret, list);
}

static int function_structure_constructor_list(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure_list_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void structure_type_constructor_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void structure_constructor_default_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_list);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int structure_constructor_instance_vector_(Execute ptr,
		addr vector, addr slots, addr value, addr args)
{
	int update, check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		update = 0;
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (check) {
			update = 1;
		}
		else {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
				update = 1;
			}
		}
		if (update) {
			Return(structure_setarray_(ptr, vector, slot, value, pos));
		}
	}

	return 0;
}

int make_structure_vector_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, vector, type, name;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetVectorStructureType(pos, &type);
	/* make */
	Return(structure_constructor_dynamic_(instance, slots, args, str->errorp));
	vector_heap(&vector, str->size_value);

	if (initp) {
		hold = LocalHold_local_push(ptr, vector);
		Return(structure_constructor_instance_vector_(ptr, vector, slots, type, args));
		localhold_end(hold);
	}

	if (str->named) {
		GetNameStructureType(pos, &name);
		Return(setelt_sequence_(vector, str->named_index, name));
	}

	return Result(ret, vector);
}

static int function_structure_constructor_vector(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure_vector_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void structure_type_constructor_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void structure_constructor_default_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_vector);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* clos */
static int structure_constructor_instance_clos_(
		Execute ptr, addr clos, addr args, int initp)
{
	int check;
	addr slots, slot, value, pos;
	size_t size, i, location;

	GetSlotClos(clos, &slots);
	GetValueClos(clos, &value);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetLocationSlot(slot, &location);
		if (! initp) {
			SetClosValue(value, location, Nil);
			continue;
		}
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (! check) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
			}
		}
		SetClosValue(value, location, pos);
	}

	return 0;
}

int make_structure_clos_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	int errorp;
	addr instance, slots, clos;
	LocalHold hold;

	/* variables */
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetErrorpStructureType(pos, &errorp);
	Return(structure_constructor_dynamic_(instance, slots, args, errorp));
	/* make */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, instance);

	hold = LocalHold_local_push(ptr, clos);
	Return(structure_constructor_instance_clos_(ptr, clos, args, initp));
	localhold_end(hold);

	return Result(ret, clos);
}

static int function_structure_constructor_clos(Execute ptr, addr args)
{
	addr pos;

	/* closure */
	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure_clos_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void structure_type_constructor_clos(addr *ret, addr data)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetInstanceStructureType(data, &data);
	type_clos_heap(data, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void structure_constructor_default_clos(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_clos(&type, data);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* default */
static int structure_constructor_default_(struct defstruct *str, addr symbol)
{
	addr pos;

	Return(parse_callname_error_(&symbol, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_list_p)
		structure_constructor_default_list(pos, symbol);
	else if (str->type_vector_p)
		structure_constructor_default_vector(pos, symbol);
	else
		structure_constructor_default_clos(pos, symbol);

	return 0;
}

/* constructor */
static int structure_constructor_make_(struct defstruct *str)
{
	addr name;

	/* name */
	Return(stdget_structure_name_(str->instance, &name));
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor_default_(str, name);
}

static int structure_constructor_lambda_(addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

static int structure_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor_lambda_(pos));
		}
		else {
			return fmte_("Invalid constructor parameter ~S.", pos, NULL);
		}
	}

	return 0;
}


/*
 *  copier
 */
/* list */
static int function_structure_copier_list(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* copy */
	copy_list_heap_unsafe(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_copier_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void structure_copier_list(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier_list);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	structure_type_slot_copier_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* vector */
static int function_structure_copier_vector(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* copy */
	copy_vector_heap(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_copier_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void structure_copier_vector(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier_vector);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	structure_type_slot_copier_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* clos */
static int function_structure_copier_clos(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void structure_type_slot_copier_clos(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &values);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void structure_copier_clos(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier_clos);
	SetFunctionSymbol(symbol, pos);
	/* type */
	structure_type_slot_copier_clos(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* copier */
static int structure_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char1_heap_(&name, "COPY-", name));
	}
	else if (str->copier == Nil) {
		return Result(ret, Unbound);
	}
	else {
		Check(! stringp(str->copier), "type error");
		name = str->copier;
	}
	return intern_default_package_(str->ptr, name, ret, NULL);
}

static int structure_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_copier_callname_(str, &symbol));
	if (symbol == Unbound)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		structure_copier_list(str, symbol);
	else if (str->type_vector_p)
		structure_copier_vector(str, symbol);
	else
		structure_copier_clos(str->instance, symbol);

	return 0;
}


/*
 *  predicate
 */
/* list */
static int function_structure_predicate_list(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_list_p(type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void structure_predicate_list(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_list);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* vector */
static int function_structure_predicate_vector(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_vector_p(ptr, type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void structure_predicate_vector(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_vector);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* clos */
static int function_structure_predicate_clos(Execute ptr, addr var)
{
	int check;
	addr instance;

	getdata_control(ptr, &instance);
	Return(typep_structure_(var, instance, &check));
	setbool_control(ptr, check);

	return 0;
}

static void structure_predicate_clos(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, instance);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* predicate */
static int structure_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char2_heap_(&name, name, "-P"));
	}
	else if (str->predicate == Nil) {
		return Result(ret, Unbound);
	}
	else {
		Check(! stringp(str->predicate), "type error");
		name = str->predicate;
	}
	return intern_default_package_(str->ptr, name, ret, NULL);
}

static int structure_predicate_(struct defstruct *str)
{
	addr symbol;

	Return(structure_predicate_callname_(str, &symbol));
	if (symbol == Unbound)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		structure_predicate_list(str, symbol);
	else if (str->type_vector_p)
		structure_predicate_vector(str, symbol);
	else
		structure_predicate_clos(str->instance, symbol);

	return 0;
}


/*
 *  printer
 */
static int structure_print_default_p(struct defstruct *str)
{
	addr g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (str->print_object_p && str->print_object == g)
		return 1;
	if (str->print_function_p && str->print_function == g)
		return 1;

	return 0;
}

static int method_defstruct_default(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	Return(print_structure(ptr, stream, var));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_default_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_default);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_add_method_(struct defstruct *str, addr name, addr method)
{
	addr generic;
	Execute ptr;

	ptr = str->ptr;
	Return(getglobalcheck_callname_(name, &generic));
	Check(! clos_generic_p_debug(generic), "type error");
	return method_add_method_(ptr, generic, method);
}

static int structure_print_default_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_default_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}

static int method_defstruct_object(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	getdata_control(ptr, &call);
	Return(callclang_apply(ptr, &call, call, Nil));
	Return(callclang_funcall(ptr, &call, call, var, stream, NULL));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_object_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_object);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_object);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_object_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_object_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}

static int method_defstruct_function(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call, pos;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	GetConst(SPECIAL_PRINT_LEVEL, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	getdata_control(ptr, &call);
	Return(callclang_apply(ptr, &call, call, Nil));
	Return(callclang_funcall(ptr, &call, call, var, stream, pos, NULL));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_function_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_function);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_function);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_function_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_function_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}

static int structure_print_(struct defstruct *str)
{
	if (str->type_p && str->print_object_p)
		return fmte_("Can't make print-object on :TYPE structure.", NULL);
	if (str->type_p && str->print_function_p)
		return fmte_("Can't make print-function on :TYPE structure.", NULL);
	if (structure_print_default_p(str))
		return structure_print_default_(str);
	else if (str->print_object_p)
		return structure_print_object_(str);
	else if (str->print_function_p)
		return structure_print_function_(str);

	return 0;
}


/*
 *  ensure-structure
 */
int ensure_structure_common_(Execute ptr, addr name, addr slots, addr args)
{
	struct defstruct str;
	LocalHold hold;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	Return(ensure_structure_struct_(&str, ptr, name, slots, args));

	hold = LocalHold_local(ptr);
	localhold_destruct(hold, &str);

	/* check */
	Return(structure_check_name_(&str));
	Return(structure_slots_heap_(str.slots, &(str.slots)));
	Return(structure_slots_heap_(str.iargs, &(str.iargs)));
	Return(structure_check_slots_(str.slots));
	Return(structure_check_slots_(str.iargs));
	Return(structure_check_predicate_(&str));
	Return(structure_include_(&str));
	Return(structure_include_slots_(&str));
	Return(structure_include_arguments_(&str));
	Return(structure_print_check_(&str));
	structure_slots_value(&str);
	/* make instance */
	Return(structure_instance_(&str));
	Check(! structure_class_p_debug(str.instance), "type error");
	clos_define_class(str.name, str.instance);
	/* settings */
	Return(structure_slots_make_(&str));
	Return(structure_slots_call_(&str));
	Return(structure_copier_(&str));
	Return(structure_predicate_(&str));
	Return(structure_constructor_(&str));
	Return(structure_print_(&str));
	/* rollback */
	localhold_end(hold);

	return 0;
}


/*
 *  initialize
 */
void init_structure_define(void)
{
	SetPointerCall(defun, var1, structure_reader_list);
	SetPointerCall(defun, var1, structure_reader_vector);
	SetPointerCall(defun, var1, structure_reader_clos);
	SetPointerCall(defun, var2, structure_writer_list);
	SetPointerCall(defun, var2, structure_writer_vector);
	SetPointerCall(defun, var2, structure_writer_clos);
	SetPointerCall(defun, dynamic, structure_constructor_list);
	SetPointerCall(defun, dynamic, structure_constructor_vector);
	SetPointerCall(defun, dynamic, structure_constructor_clos);
	SetPointerCall(defun, var1, structure_copier_list);
	SetPointerCall(defun, var1, structure_copier_vector);
	SetPointerCall(defun, var1, structure_copier_clos);
	SetPointerCall(defun, var1, structure_predicate_list);
	SetPointerCall(defun, var1, structure_predicate_vector);
	SetPointerCall(defun, var1, structure_predicate_clos);
	SetPointerType(var4, method_defstruct_default);
	SetPointerType(var4, method_defstruct_object);
	SetPointerType(var4, method_defstruct_function);
}


/************************************************************
 *  structure_object.c
 ************************************************************/

/*
 *  access
 */
static int stdget_structure_constant_(addr pos, addr *ret,
		enum Clos_structure_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_structure_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_structure_constant_(addr pos, addr value,
		enum Clos_structure_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_structure_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdGetStructure_(p,r,a,b) \
	stdget_structure_constant_((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)
#define StdSetStructure_(p,r,a,b) \
	stdset_structure_constant_((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)

int stdget_structure_name_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, name, NAME);
}
int stdset_structure_name_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, name, NAME);
}

int stdget_structure_slots_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, slots, SLOTS);
}
int stdset_structure_slots_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, slots, SLOTS);
}

int stdget_structure_documentation_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, documentation, DOCUMENTATION);
}
int stdset_structure_documentation_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, documentation, DOCUMENTATION);
}

int stdget_structure_include_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, include, INCLUDE);
}
int stdset_structure_include_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, include, INCLUDE);
}

int stdget_structure_precedence_list_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}
int stdset_structure_precedence_list_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdget_structure_type_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, type, TYPE);
}
int stdset_structure_type_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, type, TYPE);
}

int stdget_structure_vector_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, vector, VECTOR);
}
int stdset_structure_vector_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, vector, VECTOR);
}

int stdget_structure_named_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, named, NAMED);
}
int stdset_structure_named_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, named, NAMED);
}

int stdget_structure_named_index_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, named_index, NAMED_INDEX);
}
int stdset_structure_named_index_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, named_index, NAMED_INDEX);
}

int stdget_structure_value_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, value, VALUE);
}
int stdset_structure_value_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, value, VALUE);
}


/*
 *  structure
 */
void localhold_destruct(LocalHold hold, struct defstruct *str)
{
	localhold_pushva_force(hold, str->instance, str->env, str->doc, str->slots,
			str->name, str->conc_name, str->copier, str->predicate,
			str->constructor, str->iname, str->iargs,
			str->print_object, str->print_function,
			str->type_vector, str->initial_offset, NULL);
}

void defstruct_clean(struct defstruct *str)
{
	clearpoint(str);
	str->conc_name = Unbound;
	str->copier = Nil;
	str->predicate = Nil;
	str->iname = Nil;
	str->iargs = Nil;
	str->constructor = Nil;
	str->type_vector = Unbound;
	str->print_function = Unbound;
	str->print_object = Unbound;
	str->size = 0;
	str->size_value = 0;
	str->offset = 0;
	str->named_index = 0;
}


/*
 *  structure-type object
 */
struct structure_type_struct *ptrstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return PtrStructureType_Low(pos);
}

void getinstancestructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetInstanceStructureType_Low(pos, ret);
}

void setinstancestructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetInstanceStructureType_Low(pos, value);
}

void getnamestructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetNameStructureType_Low(pos, ret);
}

void setnamestructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetNameStructureType_Low(pos, value);
}

void getslotstructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetSlotStructureType_Low(pos, ret);
}

void setslotstructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetSlotStructureType_Low(pos, value);
}

void getvectorstructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetVectorStructureType_Low(pos, ret);
}

void setvectorstructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetVectorStructureType_Low(pos, value);
}

int refnamedstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return RefNamedStructureType_Low(pos);
}

void getnamedstructuretype(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetNamedStructureType_Low(pos, ret);
}

void setnamedstructuretype(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetNamedStructureType_Low(pos, value);
}

int referrorpstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return RefErrorpStructureType_Low(pos);
}

void geterrorpstructuretype(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetErrorpStructureType_Low(pos, ret);
}

void seterrorpstructuretype(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetErrorpStructureType_Low(pos, value);
}

static void structure_type_heap_unsafe(addr *ret)
{
	heap_smallsize(ret, LISPSYSTEM_STRUCTURE_TYPE,
			StructureType_size, sizeoft(struct structure_type_struct));
}


/*
 *  access vector
 */
int structure_getdirect_(Execute ptr, addr vector, size_t i, addr type, addr *ret)
{
	int check;
	addr value;

	Return(getelt_sequence_(NULL, vector, i, &value));
	Return(parse_type(ptr, &type, type, Nil));
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		return fmte_("The value ~S don't match ~A type.", value, type, NULL);
	}

	return Result(ret, value);
}

int structure_setdirect_(Execute ptr, addr vector, size_t i, addr type, addr value)
{
	int check;

	Return(parse_type(ptr, &type, type, Nil));
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		return fmte_("The value ~S don't match ~A type.", value, type, NULL);
	}

	return setelt_sequence_(vector, i, value);
}

int structure_getarray_(Execute ptr, addr vector, addr slot, addr type, addr *ret)
{
	size_t i;
	GetAccessSlot(slot, &i);
	return structure_getdirect_(ptr, vector, i, type, ret);
}

int structure_setarray_(Execute ptr, addr vector, addr slot, addr type, addr value)
{
	size_t i;
	GetAccessSlot(slot, &i);
	return structure_setdirect_(ptr, vector, i, type, value);
}


/*
 *  structure-type
 */
void structure_type_heap(addr *ret)
{
	addr pos;
	structure_type_heap_unsafe(&pos);
	clearpoint(PtrStructureType(pos));
	*ret = pos;
}

static void structure_type_parameter(addr *ret,
		addr instance, addr name, addr slot, addr vector,
		size_t size, size_t value, unsigned named, size_t named_index)
{
	addr pos;
	struct structure_type_struct *str;

	structure_type_heap_unsafe(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slot);
	SetVectorStructureType(pos, vector);
	str = PtrStructureType(pos);
	str->size = size;
	str->size_value = value;
	str->named = named;
	str->named_index = named_index;
	str->errorp = 0;
	*ret = pos;
}

void structure_type(struct defstruct *str, addr slot, addr *ret)
{
	structure_type_parameter(ret,
			str->instance, str->name, slot, str->type_vector,
			str->size, str->size_value, str->named_p, str->named_index);
}

int structure_type_list_p(addr type, addr var, int *ret)
{
	struct structure_type_struct *str;
	size_t size;

	/* listp */
	str = PtrStructureType(type);
	if (length_list_p(var, &size))
		return Result(ret, 0);
	/* length */
	if (size < str->size_value)
		return Result(ret, 0);
	/* check */
	if (str->named) {
		GetNameStructureType(type, &type);
		Return(getnth_(var, str->named_index, &var));
		return Result(ret, var == type);
	}

	return Result(ret, 1);
}

int structure_type_vector_p(Execute ptr, addr type, addr var, int *ret)
{
	struct structure_type_struct *str;
	addr check;
	size_t size;

	/* vectorp */
	if (! vector_type_p(var)) {
		*ret = 0;
		return 0;
	}
	Return(length_sequence_(var, 1, &size));
	/* length */
	str = PtrStructureType(type);
	if (size < str->size_value) {
		*ret = 0;
		return 0;
	}
	/* check */
	if (str->named) {
		GetVectorStructureType(type, &check);
		Return(structure_getdirect_(ptr, var, str->named_index, check, &var));
		GetNameStructureType(type, &type);
		*ret = (var == type);
		return 0;
	}
	*ret = 1;
	return 0;
}


/************************************************************
 *  strvect.c
 ************************************************************/

/*
 *  buffer compare
 */
#define equal_code(p1, p2, s1, s2) { \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		if ((unicode)p1[i] != (unicode)p2[i]) { \
			return 0; \
		} \
	} \
	return 1; \
}

#define equalp_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a != b) return 0; \
	} \
	return 1; \
}

#define compare_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

#define comparep_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

static int memu_equal(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 != s2)
		return 0;
	return memcmp(p1, p2, s1 * sizeoft(unicode)) == 0;
}

static int memu_compare(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 < s2)
		return -1;
	if (s1 > s2)
		return 1;
	return memcmp(p1, p2, s1 * sizeoft(unicode));
}

static int memu_equalp(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

static int memu_comparep(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}

static int memu1_equal(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equal_code(p1, p2, s1, s2);
}

static int memu1_equalp(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

static int memu1_compare(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	compare_code(p1, p2, s1, s2);
}

static int memu1_comparep(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}


/*
 *  strvect
 */
void strvect_alloc(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	alloc_body(local, &pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}
void strvect_local(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Check(local == NULL, "local error");
	local_body(local, &pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}
void strvect_heap(addr *ret, size_t len)
{
	addr pos;

	heap_body(&pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}

void strvect_copy_alloc(LocalRoot local, addr *ret, addr value)
{
	addr pos;
	unicode *dst;
	const unicode *src;
	size_t size;

	CheckType(value, LISPTYPE_STRING);
	/* source */
	GetStringSize(value, &size);
	GetStringUnicode(value, (const unicode **)&src);
	/* destination */
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&dst);
	/* copy */
	memcpy(dst, src, sizeoft(unicode) * size);
	/* result */
	*ret = pos;
}
void strvect_copy_local(LocalRoot local, addr *ret, addr value)
{
	CheckLocal(local);
	strvect_copy_alloc(local, ret, value);
}
void strvect_copy_heap(addr *ret, addr value)
{
	strvect_copy_alloc(NULL, ret, value);
}

int strvect_character_alloc_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	GetCharacter(pos, &c);
	return strvect_sizeu_alloc_(local, ret, &c, 1);
}
int strvect_character_local_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	Check(local == NULL, "local error");
	GetCharacter(pos, &c);
	return strvect_sizeu_local_(local, ret, &c, 1);
}
int strvect_character_heap_(addr *ret, addr pos)
{
	unicode c;
	GetCharacter(pos, &c);
	return strvect_sizeu_heap_(ret, &c, 1);
}

void strvect_length(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type left error");
	GetStringSize(pos, ret);
}

void strvect_posbodylen(addr pos, const unicode **body, size_t *len)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type error");
	GetStringSize(pos, len);
	GetStringUnicode(pos, body);
}

enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u)
{
	if (type == CHARACTER_TYPE_EMPTY) {
		return character_type(u);
	}
	if (isStandardType(u)) {
		return type;
	}
	if (isBaseType(u)) {
		if (type == CHARACTER_TYPE_STANDARD)
			return CHARACTER_TYPE_BASE;
		return type;
	}
	if (isExtendedType(u)) {
		if (type != CHARACTER_TYPE_INVALID && type != CHARACTER_TYPE_EXTENDED)
			return CHARACTER_TYPE_EXTENDED;
		return type;
	}
	return CHARACTER_TYPE_INVALID;
}

int strvect_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	enum CHARACTER_TYPE type;
	const unicode *body;
	size_t i, size;

	strvect_posbodylen(pos, &body, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		type = unicode_character_type(type, body[i]);
		if (type == CHARACTER_TYPE_INVALID)
			return fmte_("Invalid character code.", NULL);
	}

	return Result(ret, type);
}

int strvectp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING;
}

int strvect_base_p_(addr pos, int *ret)
{
	enum CHARACTER_TYPE type;

	if (! strvectp(pos))
		return Result(ret, 0);
	Return(strvect_character_type_(pos, &type));
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return Result(ret, 1);

		default:
			return Result(ret, 0);
	}
}

int strvect_simple_p(addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	return 0;
}

void strvect_char_alloc(LocalRoot local, addr *ret, const char *arg)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	*ret = pos;
}
void strvect_char_local(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	strvect_char_alloc(local, ret, arg);
}
void strvect_char_heap(addr *ret, const char *arg)
{
	strvect_char_alloc(NULL, ret, arg);
}

addr stringh(const char *arg) /* for debug */
{
	addr pos;
	strvect_char_heap(&pos, arg);
	return pos;
}

int strvect_sizeu_alloc_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	addr pos;
	unicode *destroy;

	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	memcpy(destroy, arg, sizeoft(unicode) * size);
	return Result(ret, pos);
}
int strvect_sizeu_local_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strvect_sizeu_alloc_(local, ret, arg, size);
}
int strvect_sizeu_heap_(addr *ret, const unicode *arg, size_t size)
{
	return strvect_sizeu_alloc_(NULL, ret, arg, size);
}


/*
 *  strvect_equal
 */
int strvect_equal_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equal(body, right, size1, size2);
}

int strvect_equalp_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equalp(body, right, size1, size2);
}

int strvect_equal_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equal(body1, (const byte *)body2, size1, size2);
}

int strvect_equalp_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equalp(body1, (const byte *)body2, size1, size2);
}

int strvect_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equal_binary(left, body, size);
}

int strvect_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equalp_binary(left, body, size);
}

int strvect_character_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);

	return size == 1 && body[0] == RefCharacter(right);
}

int strvect_character_equalp(addr left, addr right)
{
	const unicode *body;
	unicode a, b;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);
	if (size != 1)
		return 0;
	a = body[0];
	GetCharacter(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}


/*
 *  strvect_compare
 */
int strvect_compare_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_compare(body, right, size1, size2);
}

int strvect_comparep_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_comparep(body, right, size1, size2);
}

int strvect_compare_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_compare(body1, (const byte *)body2, size1, size2);
}

int strvect_comparep_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_comparep(body1, (const byte *)body2, size1, size2);
}

int strvect_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_compare_binary(left, body, size);
}

int strvect_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_comparep_binary(left, body, size);
}

int strvect_designer_equal_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equal_char(left, right);
	if (strvectp(left))
		return strvect_equal_char(left, right);

	return 0;
}

int strvect_designer_equalp_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equalp_char(left, right);
	if (strvectp(left))
		return strvect_equalp_char(left, right);

	return 0;
}


/*
 *  getc/setc
 */
void strvect_getc(addr pos, size_t index, unicode *c)
{
	const unicode *body;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, &body);
	*c =  body[index];
}

void strvect_setc_unsafe(addr pos, size_t index, unicode c)
{
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;
}

int strvect_setc_(addr pos, size_t index, unicode c)
{
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif

	if (character_type(c) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;

	return 0;
}

int strvect_setall_(addr pos, unicode c)
{
	unicode *destroy;
	size_t size, i;

	strvect_length(pos, &size);
	if (size == 0)
		return 0;
	if (character_type(c) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = c;

	return 0;
}

int strvect_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	strvect_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	strvect_getc(pos, index, &c);
	character_alloc(local, ret, c);

	return 0;
}

int strvect_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (! consp(args)) {
		*ret = 0;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = 0;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = 0;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return strvect_get_(local, pos, index, ret);
}

int strvect_set_(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	if (! characterp(value))
		return fmte_("SETF arg ~S must be a character type.", value, NULL);
	strvect_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);

	return strvect_setc_(pos, index, RefCharacter(value));
}

int strvect_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return strvect_set_(pos, index, value);
}

int strvect_fill_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	unicode c;

	/* argument */
	if (! characterp(item))
		return fmte_("FILL tem ~S must be a character type.", item, NULL);
	GetCharacter(item, &c);
	strvect_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++)
		strvect_setc_unsafe(pos, index1, c);

	return 0;
}

int strvect_subseq_alloc_(LocalRoot local, addr *ret, addr pos, size_t x, size_t y)
{
	unicode *data1;
	const unicode *data2;
	addr root;
	size_t diff;

	Check(y < x, "index error");
	diff = y - x;
	strvect_alloc(local, &root, diff);
	GetStringUnicode(root, &data1);
	GetStringUnicode(pos, &data2);
	memcpy(data1, data2 + x, diff * sizeoft(unicode));

	return Result(ret, root);
}

int strvect_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2)
{
	return strvect_subseq_alloc_(NULL, ret, pos, index1, index2);
}

int strvect_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;

	strvect_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return strvect_subseq_index_(ret, pos, index1, index2);
}

int strvect_setget_(addr pos1, size_t index1, addr pos2, size_t index2)
{
	unicode value;

	strvect_getc(pos2, index2, &value);
	return strvect_setc_(pos1, index1, value);
}

int strvect_reverse_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, x, y;

	strvect_length(pos, &size);
	strvect_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		strvect_getc(pos, x, &c);
		Return(strvect_setc_(one, y, c));
	}

	return Result(ret, one);
}

int strvect_nreverse_(addr *ret, addr pos)
{
	unicode a, b;
	size_t size, x, y;

	strvect_length(pos, &size);
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		strvect_getc(pos, x, &a);
		strvect_getc(pos, y, &b);
		Return(strvect_setc_(pos, x, b));
		Return(strvect_setc_(pos, y, a));
		x++;
		y--;
	}

	return Result(ret, pos);
}


/*
 *  make
 */
int strvect_char1_heap_(addr *ret, const char *arg, unicode c)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_heap(&pos, size + 1ULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	destroy[i] = c;
	return Result(ret, pos);
}

int strvect_size1_heap_(addr *ret, const char *arg, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t i;

	strvect_alloc(NULL, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	return Result(ret, pos);
}


/************************************************************
 *  subtypep.c
 ************************************************************/

/*
 *  result
 */
static void subtypep_result_keyword(SubtypepResult value, addr *ret)
{
	switch (value) {
		case SUBTYPEP_INCLUDE:
			GetConst(SYSTEM_INCLUDE, ret);
			break;

		case SUBTYPEP_EXCLUDE:
			GetConst(SYSTEM_EXCLUDE, ret);
			break;

		case SUBTYPEP_FALSE:
			GetConst(SYSTEM_FALSE, ret);
			break;

		case SUBTYPEP_INVALID:
		default:
			GetConst(SYSTEM_INVALID, ret);
			break;
	}
}

static void subtypep_result_values(SubtypepResult value, int *ret, int *validp)
{
	int x, y;

	switch (value) {
		case SUBTYPEP_INCLUDE:
			x = 1; y = 1;
			break;

		case SUBTYPEP_FALSE:
		case SUBTYPEP_EXCLUDE:
			x = 0; y = 1;
			break;

		case SUBTYPEP_INVALID:
		default:
			x = 0; y = 0;
			break;
	}

	*ret = x;
	if (validp)
		*validp = y;
}


/*
 *  subtypep-atomic
 */
static int subtypep_parse_throw_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, rx, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, ry, y, env));
	localhold_set(hold, 1, y);

	return 0;
}

static int subtypep_extend_atomic_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_throw_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_table_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-atomic-not
 */
static int subtypep_parse_optimize_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	LocalRoot local;

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	local = ptr->local;
	Return(type_optimize_throw_heap_(local, x, rx));
	Return(type_optimize_throw_heap_(local, y, ry));

	return 0;
}

static int subtypep_extend_atomic_not_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_optimize_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_atomic_not_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-compound
 */
static int subtypep_extend_compound_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_optimize_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-force-number
 */
static int subtypep_parse_force_number_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	LocalRoot local;

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	local = ptr->local;
	Return(type_subtypep_throw_heap_(local, x, rx));
	Return(type_subtypep_throw_heap_(local, y, ry));

	return 0;
}

static int subtypep_extend_force_number_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_force_number_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-normal
 */
static int subtypep_parse_normal_type_(Execute ptr, addr x, addr env, addr *ret)
{
	LocalRoot local;

	local = ptr->local;
	Return(parse_type(ptr, &x, x, env));
	if (subtypep_number_p(x)) {
		Return(type_subtypep_throw_heap_(local, x, &x));
	}
	else {
		Return(type_optimize_throw_heap_(local, x, &x));
	}

	return Result(ret, x);
}

static int subtypep_parse_normal_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(subtypep_parse_normal_type_(ptr, x, env, rx));
	localhold_set(hold, 0, *rx);
	Return(subtypep_parse_normal_type_(ptr, y, env, ry));
	localhold_set(hold, 1, *ry);

	return 0;
}

static int subtypep_extend_normal_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_normal_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  system:subtypep!
 */
static int subtypep_extend_value_(addr pos, enum SubtypepExtend *ret)
{
	addr check;

	if (pos == Nil || pos == Unbound)
		return Result(ret, SubtypepExtend_Normal);

	/* normal */
	GetConst(SYSTEM_SUBTYPEP_NORMAL, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Normal);

	/* atomic */
	GetConst(SYSTEM_SUBTYPEP_ATOMIC, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Atomic);

	/* atomic-not */
	GetConst(SYSTEM_SUBTYPEP_ATOMIC_NOT, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_AtomicNot);

	/* compound */
	GetConst(SYSTEM_SUBTYPEP_COMPOUND, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Compound);

	/* force-number */
	GetConst(SYSTEM_SUBTYPEP_FORCE_NUMBER, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_ForceNumber);

	*ret = SubtypepExtend_Normal;
	return fmte_("Invalid subtypep! type ~S.", pos, NULL);
}

static int subtypep_extend_output_(Execute ptr, addr *ret)
{
	enum SubtypepExtend value;
	addr pos;

	/* *subtypep!* */
	GetConst(SYSTEM_SUBTYPEP_VALUE, &pos);
	getspecial_local(ptr, pos, &pos);

	/* normal */
	Return(subtypep_extend_value_(pos, &value));
	if (value == SubtypepExtend_Normal) {
		GetConst(SYSTEM_SUBTYPEP_NORMAL, &pos);
	}

	return Result(ret, pos);
}

static int subtypep_extend_type_(Execute ptr, addr pos, enum SubtypepExtend *ret)
{
	if (pos != Nil)
		return subtypep_extend_value_(pos, ret);

	/* *subtypep!* */
	GetConst(SYSTEM_SUBTYPEP_VALUE, &pos);
	getspecial_local(ptr, pos, &pos);
	return subtypep_extend_value_(pos, ret);
}

static int subtypep_extend_switch_(Execute ptr, addr x, addr y, addr env,
		enum SubtypepExtend type, SubtypepResult *ret)
{
	switch (type) {
		case SubtypepExtend_Normal:
			return subtypep_extend_normal_(ptr, x, y, env, ret);

		case SubtypepExtend_Atomic:
			return subtypep_extend_atomic_(ptr, x, y, env, ret);

		case SubtypepExtend_AtomicNot:
			return subtypep_extend_atomic_not_(ptr, x, y, env, ret);

		case SubtypepExtend_Compound:
			return subtypep_extend_compound_(ptr, x, y, env, ret);

		case SubtypepExtend_ForceNumber:
			return subtypep_extend_force_number_(ptr, x, y, env, ret);

		default:
			return subtypep_extend_normal_(ptr, x, y, env, ret);
	}
}

int subtypep_extend_(Execute ptr, addr x, addr y, addr env, addr check, addr *ret)
{
	enum SubtypepExtend type;
	SubtypepResult value;

	/* output */
	if (check == T)
		return subtypep_extend_output_(ptr, ret);

	/* subtypep */
	Return(subtypep_extend_type_(ptr, check, &type));
	Return(subtypep_extend_switch_(ptr, x, y, env, type, &value));
	subtypep_result_keyword(value, ret);
	return 0;
}


/*
 *  interface
 */
int subtypep_scope_(Execute ptr, addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_normal_(ptr, hold, x, y, env, &x, &y));
	if (type_asterisk_p(y))
		return Result(ret, SUBTYPEP_INCLUDE);
	if (type_asterisk_p(x))
		return Result(ret, SUBTYPEP_FALSE);

	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}

int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp)
{
	SubtypepResult value;
	Return(subtypep_extend_normal_(ptr, x, y, env, &value));
	subtypep_result_values(value, ret, validp);

	return 0;
}


/*
 *  initialize
 */
void init_subtypep(void)
{
	init_subtypep_table();
}


/************************************************************
 *  subtypep_andor.c
 ************************************************************/

/*
 *  reduce
 */
#define Return_subtypep_reduce_p(ptr, pos, ret, call) { \
	int __check; \
	Return(call(ptr, pos, &__check)); \
	if (__check) { \
		return Result(ret, 1); \
	} \
}

#define Return_subtypep_reduce(ptr, pos, value, ret, call) { \
	int __check; \
	Return(call(ptr, pos, value, &__check)); \
	if (__check) { \
		return Result(ret, 1); \
	} \
}

static int subtypep_reduce_p_(Execute ptr, addr pos, int *ret);
static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret);


/* (and ...) */
static int subtypep_reduce_vector_all_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr vect;
	size_t size, i;

	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		Return(subtypep_reduce_p_(ptr, vect, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and_all_p_(Execute ptr, addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_p_(ptr, pos, ret);
}

static int subtypep_reduce_vector_all_(Execute ptr,
		addr pos, addr *value, int *ret, enum LISPDECL make)
{
	int check;
	addr dst, vect;
	size_t size, i;
	LocalRoot local;

	local = ptr->local;
	LenArrayA4(pos, &size);
	vector4_local(local, &dst, size);

	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		Return(subtypep_reduce_(ptr, vect, &vect, &check));
		SetArrayA4(dst, i, vect);
	}

	/* make */
	type1_local(local, make, dst, value);
	return Result(ret, 1);
}

static int subtypep_reduce_and_all_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and_all_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_(ptr, pos, value, ret, LISPDECL_AND);
}


/* (and) -> t */
static int subtypep_reduce_and1_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 0);
}

static int subtypep_reduce_and1_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and1_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, T);
	return Result(ret, 1);
}


/* (and type) -> type */
static int subtypep_reduce_and2_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 1);
}

static int subtypep_reduce_and2_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and2_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	GetArrayA4(pos, 0, value);
	return Result(ret, 1);
}


/* (and ... nil ...) -> nil */
static int subtypep_reduce_find_p_(Execute ptr, addr pos,
		enum LISPDECL type, enum LISPDECL find, int *ret)
{
	addr check;
	size_t size, i;

	if (RefLispDecl(pos) != type)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == find)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and3_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_AND, LISPDECL_NIL, ret);
}

static int subtypep_reduce_and3_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and3_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (and ... t ...) -> (and ...)  remove t */
static int subtypep_reduce_and4_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_AND, LISPDECL_T, ret);
}

static int subtypep_reduce_remove_(Execute ptr, addr pos, addr *value, int *ret,
		enum LISPDECL equal, enum LISPDECL make)
{
	int exist;
	addr check, dst;
	size_t size, count, i;
	LocalRoot local;

	/* length */
	LenArrayA4(pos, &size);
	count = 0;
	exist = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == equal)
			exist = 1;
		else
			count++;
	}
	if (exist == 0)
		return Result(ret, 0);

	/* replace */
	local = ptr->local;
	vector4_local(local, &dst, count);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == equal)
			continue;
		SetArrayA4(dst, count++, check);
	}
	type1_local(local, make, dst, value);

	return Result(ret, 1);
}

static int subtypep_reduce_and4_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and4_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_remove_(ptr, pos, value, ret, LISPDECL_T, LISPDECL_AND);
}


/* (and [exclude]) -> nil */
static int subtypep_reduce_and5_p_(Execute ptr, addr pos, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (x = 0; x < size; x++) {
		GetArrayA4(pos, x, &left);
		for (y = x + 1; y < size; y++) {
			GetArrayA4(pos, y, &right);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and5_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and5_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (and ...) include remove */
static int subtypep_reduce_and6_index_(Execute ptr, addr pos, size_t *value, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	LenArrayA4(pos, &size);
	if (size <= 1)
		return Result(ret, 0);

	for (x = 0; x < size; x++) {
		GetArrayA4(pos, x, &left);
		for (y = 0; y < size; y++) {
			GetArrayA4(pos, y, &right);
			if (x == y)
				continue;
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_INCLUDE) {
				if (value)
					*value = y;
				return Result(ret, 1);
			}
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and6_p_(Execute ptr, addr pos, int *ret)
{
	int check;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	Return(subtypep_reduce_and6_index_(ptr, pos, NULL, &check));
	return Result(ret, check);
}

static int subtypep_reduce_and6_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	addr dst, vect;
	size_t size, index, i, count;
	LocalRoot local;

	Return(subtypep_reduce_and6_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	index = 0;
	Return(subtypep_reduce_and6_index_(ptr, pos, &index, &check));
	if (! check)
		return Result(ret, 0);

	/* new type */
	local = ptr->local;
	vector4_local(local, &dst, size - 1ULL);
	type1_local(local, LISPDECL_AND, dst, value);
	count = 0;
	for (i = 0; i < size; i++) {
		if (index != i) {
			GetArrayA4(pos, i, &vect);
			SetArrayA4(dst, count++, vect);
		}
	}

	return Result(ret, 1);
}


/* (and ... (or ...)) */
static int subtypep_reduce_andor_p_(Execute ptr, addr pos, size_t index, int *ret)
{
	SubtypepResult check;
	addr vector, left, right;
	size_t size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and7_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr vect;
	size_t size, i;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		if (RefLispDecl(vect) == LISPDECL_OR) {
			Return(subtypep_reduce_andor_p_(ptr, pos, i, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_andor_size_(Execute ptr,
		addr pos, size_t index, size_t *rsize, int *ret)
{
	int remove;
	SubtypepResult check;
	addr vector, left, right;
	size_t count, size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	count = 0;
	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		remove = 0;
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				remove = 1;
				break;
			}
		}
		if (! remove)
			count++;
	}

	*rsize = count;
	return Result(ret, size1 != count);
}

static int subtypep_reduce_andor_set_(Execute ptr,
		addr pos, size_t index, addr dst, size_t size)
{
	int remove;
	SubtypepResult check;
	addr vector, left, right;
	size_t count, size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	count = 0;
	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		remove = 0;
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				remove = 1;
				break;
			}
		}
		if (! remove) {
			SetArrayA4(dst, count, right);
			count++;
		}
	}

	return 0;
}

static int subtypep_reduce_andor_or_(Execute ptr,
		addr pos, size_t index, addr *value, int *ret)
{
	int check;
	addr dst;
	size_t size;
	LocalRoot local;

	Return(subtypep_reduce_andor_size_(ptr, pos, index, &size, &check));
	if (! check)
		return Result(ret, 0);

	/* value */
	local = ptr->local;
	vector4_local(local, &dst, size);
	type1_local(local, LISPDECL_OR, dst, value);
	Return(subtypep_reduce_andor_set_(ptr, pos, index, dst, size));

	return Result(ret, 1);
}

static int subtypep_reduce_andor_(Execute ptr, addr pos, addr *value)
{
	int check;
	addr dst, x, y;
	size_t size, i;
	LocalRoot local;

	/* value */
	local = ptr->local;
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	vector4_local(local, &dst, size);
	type1_local(local, LISPDECL_AND, dst, value);

	/* remove */
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &x);
		if (RefLispDecl(x) == LISPDECL_OR) {
			Return(subtypep_reduce_andor_or_(ptr, pos, i, &y, &check));
			if (check)
				x = y;
		}
		SetArrayA4(dst, i, x);
	}

	return 0;
}

static int subtypep_reduce_and7_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and7_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	*ret = 1;
	return subtypep_reduce_andor_(ptr, pos, value);
}


/* and */
static int subtypep_reduce_and_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and_all_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and1_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and2_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and3_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and4_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and5_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and6_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and7_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_and_(Execute ptr, addr pos, addr *value, int *ret)
{
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and_all_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and1_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and2_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and3_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and4_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and5_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and6_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and7_);

	return Result(ret, 0);
}


/* (or ...) */
static int subtypep_reduce_or_all_p_(Execute ptr, addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_p_(ptr, pos, ret);
}

static int subtypep_reduce_or_all_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or_all_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_(ptr, pos, value, ret, LISPDECL_OR);
}


/* (or) -> nil */
static int subtypep_reduce_or1_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 0);
}

static int subtypep_reduce_or1_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or1_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (or type) -> type */
static int subtypep_reduce_or2_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 1);
}

static int subtypep_reduce_or2_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or2_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	GetArrayA4(pos, 0, value);
	return Result(ret, 1);
}


/* (or ... t ...) -> t */
static int subtypep_reduce_or3_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_OR, LISPDECL_T, ret);
}

static int subtypep_reduce_or3_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or3_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, T);
	return Result(ret, 1);
}


/* (or ... nil ...) -> (or ...)  remove nil */
static int subtypep_reduce_or4_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_OR, LISPDECL_NIL, ret);
}

static int subtypep_reduce_or4_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or4_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_remove_(ptr, pos, value, ret, LISPDECL_NIL, LISPDECL_OR);
}


/* or */
static int subtypep_reduce_or_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or_all_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or1_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or2_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or3_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or4_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_or_(Execute ptr, addr pos, addr *value, int *ret)
{
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or_all_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or1_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or2_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or3_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or4_);

	return Result(ret, 0);
}


/* reduce interface */
static int subtypep_reduce_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check, loop, update;
	addr x;

	update = 0;
start:
	loop = 0;

	/* and */
	Return(subtypep_reduce_and_(ptr, pos, &x, &check));
	if (check) {
		loop = 1;
		pos = x;
	}

	/* or */
	Return(subtypep_reduce_or_(ptr, pos, &x, &check));
	if (check) {
		loop = 1;
		pos = x;
	}

	/* result */
	if (loop) {
		update = 1;
		goto start;
	}
	*value = pos;
	return Result(ret, update);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      -        any      any
 *  or       -        all      all
 *  -        and      all      any
 *  -        or       any      all
 *
 *  and      and      any/all  any/any
 *  and      or       any/any  any/all
 *  or       and      all/all  all/any
 *  or       or       all/any  all/all
 */
/*
 *  include
 */
static int subtypep_any_type_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	include = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_any_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_type_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_all_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  exclude
 */
static int subtypep_any_type_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_any_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_type_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(x, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &pos);
		Return(subtypep_compound_(ptr, pos, y, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_type_all_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr pos;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &pos);
		Return(subtypep_compound_(ptr, x, pos, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  any/all include
 */
static int subtypep_any_any_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_include_(ptr, x, value, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_any_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(x, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &value);
		Return(subtypep_type_any_include_(ptr, value, y, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_any_all_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_include_(ptr, x, value, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_all_include_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	include = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_all_type_include_(ptr, x, value, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  any/all include
 */
static int subtypep_any_any_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_exclude_(ptr, x, value, &result));
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_any_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(x, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &value);
		Return(subtypep_type_any_exclude_(ptr, value, y, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_any_all_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_any_type_exclude_(ptr, x, value, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_all_all_exclude_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int exclude, invalid;
	SubtypepResult result;
	addr value;
	size_t size, i;

	LenArrayA4(y, &size);
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &value);
		Return(subtypep_all_type_exclude_(ptr, x, value, &result));
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      and      any/all  any/any
 */
static int subtypep_and_and_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_any_all_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_any_any_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      or       any/any  any/all
 */
static int subtypep_and_or_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_any_any_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_any_all_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  or       and      all/all  all/any
 */
static int subtypep_or_and_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_all_all_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_all_any_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  or       or       all/any  all/all
 */
static int subtypep_or_or_vector_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;

	/* include */
	Return(subtypep_all_any_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_all_all_exclude_(ptr, x, y, ret);
}


/*
 *  interface
 */
static int subtypep_vector_call_(Execute ptr,
		addr x, addr y, SubtypepResult *ret,
		int (*call)(Execute, addr, addr, SubtypepResult *))
{
	int check;

	/* reduce */
	Return(subtypep_reduce_(ptr, x, &x, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	Return(subtypep_reduce_(ptr, y, &y, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);

	/* vector */
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	return (*call)(ptr, x, y, ret);
}

int subtypep_and_and_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_and_and_vector_);
}

int subtypep_and_or_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_and_or_vector_);
}

int subtypep_or_and_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_or_and_vector_);
}

int subtypep_or_or_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_vector_call_(ptr, x, y, ret, subtypep_or_or_vector_);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  and      -        any      any
 */
int subtypep_and_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, x, &x, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(x, 0, &x);

	/* include */
	Return(subtypep_any_type_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_any_type_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  or       -        all      all
 */
int subtypep_or_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, x, &x, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(x, 0, &x);

	/* include */
	Return(subtypep_all_type_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_all_type_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  -        and      all      any
 */
int subtypep_type_and_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, y, &y, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(y, 0, &y);

	/* include */
	Return(subtypep_type_all_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_type_any_exclude_(ptr, x, y, ret);
}


/*
 *  left     right    include  exclude
 *  ------------------------------------
 *  -        or       any      all
 */
int subtypep_type_or_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;
	SubtypepResult result;

	/* reduce */
	Return(subtypep_reduce_(ptr, y, &y, &check));
	if (check)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(y, 0, &y);

	/* include */
	Return(subtypep_type_any_include_(ptr, x, y, &result));
	if (result == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* exclude */
	return subtypep_type_all_exclude_(ptr, x, y, ret);
}


/************************************************************
 *  subtypep_atomic.c
 ************************************************************/

int ReturnReverse(SubtypepResult *ret, SubtypepResult check)
{
	switch (check) {
		case SUBTYPEP_INCLUDE:
			return ReturnFalse(ret);

		default:
			return Result(ret, check);
	}
}


/*
 *  type
 */
int subtypep_call_invalid_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return ReturnInvalid(ret);
}


/*
 *  clos
 */
int subtypep_call_clos_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	if (RefLispDecl(x) != LISPDECL_CLOS)
		return ReturnExclude(ret);
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	Return(clos_subclass_p_(x, y, &check));
	if (check)
		return ReturnInclude(ret);

	return ReturnFalse(ret);
}


/*
 *  asterisk
 */
int subtypep_call_asterisk_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_call_t_(ptr, x, y, ret);
}


/*
 *  nil
 */
int subtypep_call_nil_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  t
 */
int subtypep_call_t_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return ReturnInclude(ret);
}


/*
 *  null
 */
int subtypep_call_null_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NULL:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  eqltype
 */
int subtypep_call_eqltype_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == RefLispDecl(y))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  symbol
 */
int subtypep_call_symbol_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NULL:
		case LISPDECL_SYMBOL:
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  keyword
 */
int subtypep_call_keyword_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		case LISPDECL_SYMBOL:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  pathname
 */
int subtypep_call_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_PATHNAME:
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  logical-pathname
 */
int subtypep_call_logical_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		case LISPDECL_PATHNAME:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  array
 */
static int subtypep_call_aa_integer_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size1, size2;

	Return(getindex_integer_(x, &size1));
	Return(getindex_integer_(y, &size2));

	return ReturnIncludeExclude(ret, (size1 == size2));
}

static int subtypep_call_aa_vector_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size1, size2;

	LenArrayA4(x, &size1);
	Return(getindex_integer_(y, &size2));

	return ReturnIncludeExclude(ret, (size1 == size2));
}

static int subtypep_call_aa_integer_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check;
	size_t size1, size2, i;

	Return(getindex_integer_(x, &size1));
	LenArrayA4(y, &size2);
	if (size1 != size2)
		return ReturnExclude(ret);

	for (i = 0; i < size1; i++) {
		GetArrayA4(y, i, &check);
		if (! type_asterisk_p(check))
			return ReturnFalse(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_aa_vector_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check1, check2;
	size_t size1, size2, i, dim1, dim2;

	LenArrayA4(x, &size1);
	LenArrayA4(y, &size2);
	if (size1 != size2)
		return ReturnExclude(ret);

	for (i = 0; i < size1; i++) {
		/* asterisk */
		GetArrayA4(y, i, &check2);
		if (type_asterisk_p(check2))
			continue;
		GetArrayA4(x, i, &check1);
		if (type_asterisk_p(check1))
			return ReturnFalse(ret);

		/* dimension */
		Return(getindex_integer_(check1, &dim1));
		Return(getindex_integer_(check2, &dim2));
		if (dim1 != dim2)
			return ReturnExclude(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_aa_integer_(addr x, addr y, SubtypepResult *ret)
{
	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_integer_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_integer_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_aa_vector_(addr x, addr y, SubtypepResult *ret)
{
	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_vector_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_vector_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_array_array_dimension_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	switch (GetType(y)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_array_array_equal_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	if (upgraded_array0_equal(x, y))
		return ReturnInclude(ret);

	return ReturnExclude(ret);
}

static int subtypep_call_array_array_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr type1, type2, dim1, dim2;

	GetArrayType(x, 0, &type1);
	GetArrayType(y, 0, &type2);
	GetArrayType(x, 1, &dim1);
	GetArrayType(y, 1, &dim2);

	/* subtypep */
	Return(subtypep_call_array_array_equal_(type1, type2, &value1));
	ReturnSecondThrow(ret, value1);
	Return(subtypep_call_array_array_dimension_(dim1, dim2, &value2));
	ReturnSecondThrow(ret, value2);

	/* include */
	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* reverse */
	if (value1 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_array_array_equal_(type2, type1, &value1));
		return ReturnSecondValue(ret, value1);
	}
	if (value2 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_array_array_dimension_(dim2, dim1, &value2));
		return ReturnSecondValue(ret, value2);
	}

	return ReturnFalse(ret);
}

int subtypep_call_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  simple-array
 */
static int subtypep_call_simple_array_array_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_call_array_array_(y, x, &check));
	return ReturnReverse(ret, check);
}

int subtypep_call_simple_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		case LISPDECL_ARRAY:
			return subtypep_call_simple_array_array_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  character
 */
int subtypep_call_character_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  base-char
 */
int subtypep_call_base_char_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  standard-char
 */
int subtypep_call_standard_char_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_BASE_CHAR:
		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  real range
 */
static int subtypep_real_less_(addr x, addr y, int *ret)
{
	if (! range_any_right_p(x))
		return Result(ret, 0);

	return range_right_right_less_equal_(x, y, ret);
}

static int subtypep_real_greater_(addr x, addr y, int *ret)
{
	if (! range_left_any_p(x))
		return Result(ret, 0);

	return range_left_left_greater_equal_(x, y, ret);
}

static int subtypep_real_range_(addr x, addr y, int *ret)
{
	if (! range_between_p(x))
		return Result(ret, 0);

	return range_in_between_(x, y, ret);
}

static int subtypep_realcheck_(addr x, addr y, int *ret)
{
	addr check1, check2;

	if (range_asterisk_p(y))
		return Result(ret, 1);
	if (range_asterisk_p(x))
		return Result(ret, 0);

	GetArrayType(y, 0, &check1);
	if (type_asterisk_p(check1))
		return subtypep_real_less_(x, y, ret);

	GetArrayType(y, 2, &check2);
	if (type_asterisk_p(check2))
		return subtypep_real_greater_(x, y, ret);

	return subtypep_real_range_(x, y, ret);
}

static int realexclude_left_(addr x, addr y, int *ret)
{
	if (! range_any_right_p(x))
		return Result(ret, 0);
	if (! range_left_any_p(y))
		return Result(ret, 0);

	return range_right_left_less_(x, y, ret);
}

static int realexclude_right_(addr x, addr y, int *ret)
{
	if (! range_left_any_p(x))
		return Result(ret, 0);
	if (! range_any_right_p(y))
		return Result(ret, 0);

	return range_left_right_greater_(x, y, ret);
}

static int subtypep_realexlucde_(addr x, addr y, int *ret)
{
	int check;

	Return(realexclude_left_(x, y, &check));
	if (check)
		return Result(ret, 1);

	return realexclude_right_(x, y, ret);
}

static int subtypep_realparameter_(addr x, addr y, SubtypepResult *ret)
{
	int check;

	Return(subtypep_realcheck_(x, y, &check));
	if (check)
		return ReturnInclude(ret);

	Return(subtypep_realexlucde_(x, y, &check));
	if (check)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_realexclude_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_realparameter_(y, x, &check));
	return ReturnReverse(ret, check);
}


/*
 *  integer
 */
int subtypep_call_integer_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_INTEGER:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  rational
 */
static int subtypep_call_rational_ratio_(addr y, SubtypepResult *ret)
{
	if (range_asterisk_p(y))
		return ReturnInclude(ret);
	else
		return ReturnFalse(ret);
}

int subtypep_call_rational_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return subtypep_call_rational_ratio_(y, ret);

		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_RATIONAL:
		case LISPDECL_INTEGER:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  real
 */
int subtypep_call_real_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return subtypep_call_rational_ratio_(y, ret);

		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  float
 */
int subtypep_call_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  float range
 */
static int subtypep_float_type_(addr x, addr y,
		SubtypepResult *ret, enum LISPDECL check)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	if (type == check)
		return subtypep_realparameter_(x, y, ret);

	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
			return subtypep_realexclude_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  short-float
 */
int subtypep_call_short_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_SHORT_FLOAT);
}


/*
 *  single-float
 */
int subtypep_call_single_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_SINGLE_FLOAT);
}


/*
 *  double-float
 */
int subtypep_call_double_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_DOUBLE_FLOAT);
}


/*
 *  long-float
 */
int subtypep_call_long_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_LONG_FLOAT);
}


/*
 *  number
 */
int subtypep_call_number_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
		case LISPDECL_COMPLEX:
		case LISPDECL_RATIO:
		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  ratio
 */
int subtypep_call_ratio_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
		case LISPDECL_REAL:
		case LISPDECL_RATIONAL:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  stream
 */
int subtypep_call_stream_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_STREAM:
		case LISPDECL_BROADCAST_STREAM:
		case LISPDECL_CONCATENATED_STREAM:
		case LISPDECL_ECHO_STREAM:
		case LISPDECL_FILE_STREAM:
		case LISPDECL_STRING_STREAM:
		case LISPDECL_SYNONYM_STREAM:
		case LISPDECL_TWO_WAY_STREAM:
		case LISPDECL_PROMPT_STREAM:
		case LISPDECL_PRETTY_STREAM:
		case LISPDECL_MEMORY_STREAM:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  stream object
 */
int subtypep_call_stream_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;

	GetLispDecl(x, &type1);
	GetLispDecl(y, &type2);
	if (type1 == type2)
		return ReturnInclude(ret);
	if (type1 == LISPDECL_STREAM)
		return ReturnFalse(ret);

	return ReturnExclude(ret);
}


/************************************************************
 *  subtypep_check.c
 ************************************************************/

/*
 *  range
 */
static int subtypep_number_count(addr pos, int *count);

static int subtypep_number_count_optimized(addr pos, int *count)
{
	get_type_optimized(&pos, pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_subtypep(addr pos, int *count)
{
	get_type_subtypep(&pos, pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_not(addr pos, int *count)
{
	GetArrayType(pos, 0, &pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_andor(addr pos, int *count)
{
	addr value;
	size_t size, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		if (subtypep_number_count(value, count))
			return 1;
	}

	return 0;
}

static int subtypep_number_count_range(addr pos, int *count)
{
	if (! range_asterisk_p(pos))
		(*count)++;

	return (2 <= *count);
}

static int subtypep_number_count(addr pos, int *count)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return subtypep_number_count_optimized(pos, count);

		case LISPDECL_SUBTYPEP:
			return subtypep_number_count_subtypep(pos, count);

		case LISPDECL_NOT:
			return subtypep_number_count_not(pos, count);

		case LISPDECL_AND:
		case LISPDECL_OR:
			return subtypep_number_count_andor(pos, count);

		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SHORT_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
			return subtypep_number_count_range(pos, count);

		default:
			return 0;
	}
}


/*
 *  or
 */
static int subtypep_number_begin(addr pos);

static int subtypep_number_begin_optimized(addr pos)
{
	get_type_optimized(&pos, pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_subtypep(addr pos)
{
	get_type_subtypep(&pos, pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_not(addr pos)
{
	GetArrayType(pos, 0, &pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_or(addr pos)
{
	int count;
	addr value;
	size_t size, i;

	count = 0;
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		if (subtypep_number_count(value, &count))
			return 1;
	}

	return 0;
}

static int subtypep_number_begin(addr pos)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return subtypep_number_begin_optimized(pos);

		case LISPDECL_SUBTYPEP:
			return subtypep_number_begin_subtypep(pos);

		case LISPDECL_NOT:
			return subtypep_number_begin_not(pos);

		case LISPDECL_AND:
		case LISPDECL_OR:
			return subtypep_number_begin_or(pos);

		default:
			return 0;
	}
}


/*
 *  interface
 */
int subtypep_number_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return subtypep_number_begin(pos);
}


/************************************************************
 *  subtypep_compound.c
 ************************************************************/

/*
 *  subtypep_lisptype_
 */
static int subtypep_lisptype_normal_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, x, y, &value));
	GetNotDecl(y, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_EXCLUDE: SUBTYPEP_INCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_not_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, y, x, &value));  /* reverse */
	GetNotDecl(y, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, SUBTYPEP_FALSE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	if (RefNotDecl(x))
		return subtypep_lisptype_not_(ptr, x, y, ret, call);
	else
		return subtypep_lisptype_normal_(ptr, x, y, ret, call);
}


/*
 *  subtypep_eql
 */
static int subtypep_eql_eql_(addr x, addr y, SubtypepResult *ret)
{
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (eql_function(x, y))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(eql x) '(satisfies y)) */
	type_getvalues1(y, &y);
	if (RefLispDecl(y) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep '(eql x) y) */
	GetArrayType(x, 0, &x);
	Return(typep_table_(ptr, x, y, &check));
	if (check)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_type_eql_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(satisfies x) '(eql y)) */
	type_getvalues1(x, &x);
	if (RefLispDecl(x) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep x '(eql x)) */
	GetArrayType(y, 0, &y);
	Return(typep_table_(ptr, y, x, &check));
	if (check)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_call_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(x) == LISPDECL_EQL);
	check2 = (RefLispDecl(y) == LISPDECL_EQL);
	if (check1 && check2)
		return subtypep_eql_eql_(x, y, ret);
	if (check1)
		return subtypep_eql_type_(ptr, x, y, ret);
	if (check2)
		return subtypep_type_eql_(ptr, x, y, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int subtypep_eql_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, x, y, ret, subtypep_eql_call_);
}


/*
 *  subtypep_values
 */
static size_t getsize_values(addr pos)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	/* opt */
	GetArrayType(pos, 1, &check);
	size += length_list_unsafe(check);

	return size;
}

static int subtypep_values_var_size(addr x, addr y)
{
	addr check;
	size_t size1, size2;

	GetArrayType(x, 0, &check);
	size1 = length_list_unsafe(check);
	GetArrayType(y, 0, &check);
	size2 = length_list_unsafe(check);

	return size1 < size2;
}

static int gettype_values_(addr pos, size_t index, addr *ret)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);
	index -= size;

	/* opt */
	GetArrayType(pos, 1, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);

	/* rest */
	GetArrayType(pos, 2, ret);

	return 0;
}

static int subtypep_boolean_(Execute ptr, addr x, addr y, int *ret)
{
	SubtypepResult value;
	Return(subtypep_compound_(ptr, x, y, &value));
	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int subtypep_values_values_(Execute ptr, addr x, addr y, int *ret)
{
	int check;
	addr check1, check2;
	size_t size1, size2, size, i;

	Check(RefLispDecl(x) != LISPDECL_VALUES, "decl left error");
	Check(RefLispDecl(y) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(x), "left not error");
	Check(RefNotDecl(y), "right not error");

	/* var */
	if (subtypep_values_var_size(x, y))
		return Result(ret, 0);

	/* size */
	size1 = getsize_values(x);
	size2 = getsize_values(y);
	size = (size1 > size2)? size1: size2;
	size++; /* &rest check */

	/* check */
	for (i = 0; i < size; i++) {
		Return(gettype_values_(x, i, &check1));
		Return(gettype_values_(y, i, &check2));
		Return(subtypep_boolean_(ptr, check1, check2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static void subtypep_values_local(addr pos, addr *ret)
{
	LocalRoot local;
	addr rest;

	local = Local_Thread;
	conscar_local(local, &pos, pos);
	GetTypeTable(&rest, Null);
	type_values_local(local, pos, Nil, rest, Nil, ret);
}

static int subtypep_values_type_(Execute ptr, addr x, addr y, int *ret)
{
	Check(RefLispDecl(x) != LISPDECL_VALUES, "decl left error");
	Check(RefNotDecl(x), "left not error");
	subtypep_values_local(y, &y);
	return subtypep_values_values_(ptr, x, y, ret);
}

static int subtypep_type_values_(Execute ptr, addr x, addr y, int *ret)
{
	Check(RefLispDecl(y) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(y), "right not error");
	subtypep_values_local(x, &x);
	return subtypep_values_values_(ptr, x, y, ret);
}

static int subtypep_values_call_(Execute ptr, addr x, addr y, int *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(x) == LISPDECL_VALUES);
	check2 = (RefLispDecl(y) == LISPDECL_VALUES);
	if (check1 && check2)
		return subtypep_values_values_(ptr, x, y, ret);
	if (check1)
		return subtypep_values_type_(ptr, x, y, ret);
	if (check2)
		return subtypep_type_values_(ptr, x, y, ret);
	Abort("type error");
	return Result(ret, 0);
}

static int subtypep_values_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(subtypep_values_call_(ptr, x, y, &value));
	rollback_local(local, stack);
	return Result(ret, value? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE);
}


/*
 *  subtypep_call
 */
int subtypep_atomic_not_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, x, y, ret, subtypep_table_);
}

static int subtypep_clos_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int not1;

	GetNotDecl(y, &not1);
	if (not1)
		return Result(ret, SUBTYPEP_INVALID);
	else
		return subtypep_atomic_not_(ptr, x, y, ret);
}


/* left */
static int subtypep_satisfies_left_(addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(y) == LISPDECL_T)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	Check(GetType(x) != LISPTYPE_TYPE, "type left error");
	switch (RefLispDecl(x)) {
		case LISPDECL_EQL:
			return subtypep_eql_(ptr, x, y, ret);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, x, y, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_left_(x, y, ret);

		case LISPDECL_NIL:
			return ReturnInclude(ret);

		case LISPDECL_T:
			return ReturnFalse(ret);

		case LISPDECL_CLOS:
			return subtypep_clos_left_(ptr, x, y, ret);

		case LISPDECL_INVALID:
			return ReturnInvalid(ret);

		case LISPDECL_AND:
		case LISPDECL_OR:
		case LISPDECL_MEMBER:
		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The type illegal in this context.", NULL);

		default:
			return subtypep_atomic_not_(ptr, x, y, ret);
	}
}

/* right */
static int subtypep_satisfies_right_(addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_nil_right_(addr x, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	Check(GetType(y) != LISPTYPE_TYPE, "type right error");
	switch (RefLispDecl(y)) {
		case LISPDECL_EQL:
			return subtypep_eql_(ptr, x, y, ret);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, x, y, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_right_(x, y, ret);

		case LISPDECL_NIL:
			return subtypep_nil_right_(x, ret);

		case LISPDECL_T:
			return ReturnInclude(ret);

		case LISPDECL_INVALID:
			return ReturnInvalid(ret);

		case LISPDECL_AND:
		case LISPDECL_OR:
		case LISPDECL_MEMBER:
		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The type illegal in this context.", NULL);

		default:
			return subtypep_left_(ptr, x, y, ret);
	}
}

int subtypep_compound_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;
	int and1, and2, or1, or2;

	CheckType(x, LISPTYPE_TYPE);
	CheckType(y, LISPTYPE_TYPE);
	GetLispDecl(x, &type1);
	GetLispDecl(y, &type2);
	and1 = (type1 == LISPDECL_AND);
	and2 = (type2 == LISPDECL_AND);
	or1 = (type1 == LISPDECL_OR);
	or2 = (type2 == LISPDECL_OR);

	if (and1 && and2)
		return subtypep_and_and_(ptr, x, y, ret);
	if (and1 && or2)
		return subtypep_and_or_(ptr, x, y, ret);
	if (or1 && and2)
		return subtypep_or_and_(ptr, x, y, ret);
	if (or1 && or2)
		return subtypep_or_or_(ptr, x, y, ret);

	if (and1)
		return subtypep_and_type_(ptr, x, y, ret);
	if (or1)
		return subtypep_or_type_(ptr, x, y, ret);
	if (and2)
		return subtypep_type_and_(ptr, x, y, ret);
	if (or2)
		return subtypep_type_or_(ptr, x, y, ret);

	return subtypep_right_(ptr, x, y, ret);
}


/************************************************************
 *  subtypep_number.c
 ************************************************************/

/*
 *  coerce
 */
static int real_extract_integer_local_(LocalRoot local, addr pos, addr *ret)
{
	addr rem;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return Result(ret, pos);

		case LISPTYPE_RATIO:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return truncate1_common_(local, ret, &rem, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_rational_local_(LocalRoot local, addr pos, addr *ret)
{
	addr rem;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return Result(ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return truncate1_common_(local, ret, &rem, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_single_local_(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, pos);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_ds_value_(pos, &value));
			single_float_local(local, ret, value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ls_value_(pos, &value));
			single_float_local(local, ret, value);
			return 0;

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_double_local_(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			double_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return double_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sd_value_(pos, &value));
			double_float_local(local, ret, value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ld_value_(pos, &value));
			double_float_local(local, ret, value);
			return 0;

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_long_local_(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			long_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return long_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sl_value_(pos, &value));
			long_float_local(local, ret, value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dl_value_(pos, &value));
			long_float_local(local, ret, value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_float_local_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_lispdecl_local_(LocalRoot local,
		enum LISPDECL type, addr pos, addr *ret)
{
	if (type_asterisk_p(pos))
		return Result(ret, pos);

	switch (type) {
		case LISPDECL_INTEGER:
			return real_extract_integer_local_(local, pos, ret);

		case LISPDECL_RATIONAL:
			return real_extract_rational_local_(local, pos, ret);

		case LISPDECL_SHORT_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
			return real_extract_single_local_(local, pos, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return real_extract_double_local_(local, pos, ret);

		case LISPDECL_LONG_FLOAT:
			return real_extract_long_local_(local, pos, ret);

		case LISPDECL_FLOAT:
			return real_extract_float_local_(local, pos, ret);

		case LISPDECL_REAL:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type.", NULL);
	}
}


/*
 *  real_filter
 */
static int type_range_left_(LocalRoot local,
		addr *ret, enum LISPDECL type, addr left1, addr left2)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	Return(real_extract_lispdecl_local_(local, type, left2, &left2));
	type4_local(local, type, left1, left2, aster, aster, ret);

	return 0;
}

static int type_range_right_(LocalRoot local,
		addr *ret, enum LISPDECL type, addr right1, addr right2)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	Return(real_extract_lispdecl_local_(local, type, right2, &right2));
	type4_local(local, type, aster, aster, right1, right2, ret);

	return 0;
}

static int type_range_not_(LocalRoot local, addr *ret, enum LISPDECL type,
		addr left1, addr left2, addr right1, addr right2)
{
	addr pos;

	vector4_local(local, &pos, 2);
	Return(type_range_left_(local, &right1, type, (right1 == Nil)? T: Nil, right2));
	Return(type_range_right_(local, &left1, type, (left1 == Nil)? T: Nil, left2));
	SetArrayA4(pos, 0, right1);
	SetArrayA4(pos, 1, left1);
	type1_local(local, LISPDECL_OR, pos, ret);

	return 0;
}

static int real_filter_not_range_(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	int aster1, aster2;
	addr left1, left2, right1, right2;

	GetArrayType(type, 0, &left1);
	GetArrayType(type, 2, &right1);
	aster1 = type_asterisk_p(left1);
	aster2 = type_asterisk_p(right1);
	if (aster1 && aster2)
		return Result(ret, Nil);

	GetArrayType(type, 3, &right2);
	if (aster1)
		return type_range_left_(local, ret, decl, (right1 == Nil)? T: Nil, right2);

	GetArrayType(type, 1, &left2);
	if (aster2)
		return type_range_right_(local, ret, decl, (left1 == Nil)? T: Nil, left2);

	return type_range_not_(local, ret, decl, left1, left2, right1, right2);
}

static int real_filter_not_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER)
		return Result(ret, Nil);
	if (decl_subtypep_real(decl, right))
		return real_filter_not_range_(local, ret, type, decl);
	type4aster_local(local, decl, ret);

	return 0;
}

static int real_filter_normal_(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;
	addr left1, left2, right1, right2;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER) {
		type4aster_local(local, decl, ret);
		return 0;
	}
	if (decl_subtypep_real(decl, right)) {
		GetArrayType(type, 0, &left1);
		GetArrayType(type, 1, &left2);
		GetArrayType(type, 2, &right1);
		GetArrayType(type, 3, &right2);
		Return(real_extract_lispdecl_local_(local, decl, left2, &left2));
		Return(real_extract_lispdecl_local_(local, decl, right2, &right2));
		type4_local(local, decl, left1, left2, right1, right2, ret);
		return 0;
	}

	return Result(ret, Nil);
}

static int real_filter_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	if (RefNotDecl(type))
		return real_filter_not_(local, ret, type, decl);
	else
		return real_filter_normal_(local, ret, type, decl);
}

static void vector4_andor(LocalRoot local,
		addr *ret, addr src, size_t size, enum LISPDECL decl)
{
	addr dst, pos;
	size_t i;

	if (size == 0) {
		*ret = Nil;
		return;
	}
	vector4_local(local, &dst, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &pos);
		SetArrayA4(dst, i, pos);
	}
	type1_local(local, decl, dst, ret);
}

static int real_filter_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static int real_filter_and_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		Return(real_filter_(local, &pos, pos, decl));
		if (pos == Nil)
			return Result(ret, Nil);
		SetArrayA4(temp, count++, pos);
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_AND);
	return 0;
}

static int real_filter_or_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		Return(real_filter_(local, &pos, pos, decl));
		if (pos != Nil) {
			SetArrayA4(temp, count++, pos);
		}
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_OR);
	return 0;
}

static int real_filter_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			return real_filter_and_(local, ret, type, decl);

		case LISPDECL_OR:
			return real_filter_or_(local, ret, type, decl);

		default:
			return real_filter_type_(local, ret, type, decl);
	}
}


/*
 *  merge_range
 */
static void merge_range_cons(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	Check(RefLispDecl(type) != decl, "type error");
	Check(RefNotDecl(type), "not error");
	conscar_local(local, ret, type);
}

/* merge-range-and */
static int make_range_left_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	enum LISPDECL decl;
	addr left1, left2, right1, right2;

	range_left_value(left, &left1, &left2);
	range_right_value(right, &right1, &right2);
	GetLispDecl(left, &decl);
	Return(real_extract_lispdecl_local_(local, decl, left2, &left2));
	Return(real_extract_lispdecl_local_(local, decl, right2, &right2));
	type4_local(local, decl, left1, left2, right1, right2, ret);

	return 0;
}

static int make_range_left_aster_(LocalRoot local, addr *ret, addr left)
{
	addr left1, left2;

	range_left_value(left, &left1, &left2);
	return type_range_left_(local, ret, RefLispDecl(left), left1, left2);
}

static int make_range_aster_right_(LocalRoot local, addr *ret, addr right)
{
	addr right1, right2;

	range_right_value(right, &right1, &right2);
	return type_range_right_(local, ret, RefLispDecl(right), right1, right2);
}

/* (10 *) (20 *) */
static int range_and_left_left_(addr *ret, addr left, addr right)
{
	int check;
	Return(range_left_left_less_(left, right, &check));
	return Result(ret, check? right: left);
}

/* (* 20) (10 *) */
static int range_and_right_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_right_left_greater_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);

	return Result(ret, Nil);
}

/* (10 20) (15 *) */
static int range_and_between_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_left_left_less_equal_(right, left, &check));
	if (check)
		return Result(ret, left);

	Return(range_left_right_less_equal_(right, left, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);

	return Result(ret, Nil);
}

static int range_and_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_left_left_(ret, left, right);
	if (range_right_p(left))
		return range_and_right_left_(local, ret, left, right);
	if (range_between_p(left))
		return range_and_between_left_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

/* (10 *) (* 20) */
static int range_and_left_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_left_right_less_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

/* (* 10) (* 20) */
static int range_and_right_right_(addr *ret, addr left, addr right)
{
	int check;
	Return(range_right_right_less_(left, right, &check));
	return Result(ret, check? left: right);
}

/* (10 30) (* 20) */
static int range_and_between_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_right_right_less_equal_(left, right, &check));
	if (check)
		return Result(ret, left);

	Return(range_left_right_less_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

static int range_and_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_left_right_(local, ret, left, right);
	if (range_right_p(left))
		return range_and_right_right_(ret, left, right);
	if (range_between_p(left))
		return range_and_between_right_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

/* (10 40) (20 30) */
static int range_and_between_between_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_between_in_(left, right, &check));
	if (check)
		return Result(ret, right);
	Return(range_between_in_(right, left, &check));
	if (check)
		return Result(ret, left);
	Return(range_between_left_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);
	Return(range_between_right_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

static int range_and_between_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_between_left_(local, ret, right, left);
	if (range_right_p(left))
		return range_and_between_right_(local, ret, right, left);
	if (range_between_p(left))
		return range_and_between_between_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int range_and_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(right))
		return Result(ret, left);
	if (range_left_p(right))
		return range_and_left_(local, ret, left, right);
	if (range_right_p(right))
		return range_and_right_(local, ret, left, right);
	if (range_between_p(right))
		return range_and_between_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int map_range_and_(LocalRoot local, addr *ret, addr list, addr right)
{
	addr result, left;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &left, &list);
		Return(range_and_(local, &left, left, right));
		if (left != Nil)
			cons_local(local, &result, left, result);
	}
	nreverse(ret, result);

	return 0;
}

static int merge_range_andplus_(LocalRoot local, addr *ret, addr left, addr right)
{
	addr type;

	while (right != Nil) {
		GetCons(right, &type, &right);
		Return(map_range_and_(local, &left, left, type));
		if (left == Nil)
			break;
	}

	return Result(ret, left);
}

static int merge_range_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static int range_and_otherwise_(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t i, size;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		Return(merge_range_type_(local, &right, right, decl));
		if (right == Nil) {
			left = Nil;
			break;
		}
		else if (right == T) {
			continue;
		}
		else if (left == Nil) {
			left = right;
		}
		else {
			Return(merge_range_andplus_(local, &left, left, right));
			if (left == Nil)
				break;
		}
	}

	return Result(ret, left);
}

static int merge_range_and_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0)
		return Result(ret, T);
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return 0;
	}

	return range_and_otherwise_(local, ret, type, decl);
}

/* merge-range-or */
typedef int (*extpairtype)(LocalRoot, addr x, addr y, addr *value, int *ret);
static int extpaircall_right_(LocalRoot local,
		extpairtype call, addr left, addr cons, addr *value, int *ret)
{
	int check;
	addr right;

	while (cons != Nil) {
		GetCons(cons, &right, &cons);
		if (left != right) {
			Return((*call)(local, left, right, &right, &check));
			if (check) {
				*value = right;
				return Result(ret, check);
			}
		}
	}

	return Result(ret, 0);
}

static void extpaircall_pushlist(LocalRoot local, addr *ret, addr list, addr result)
{
	addr one;

	while (list != Nil) {
		GetCons(list, &one, &list);
		cons_local(local, &result, one, result);
	}
	*ret = result;
}

static int extpaircall_left_(
		LocalRoot local, extpairtype call, addr right, addr *value, int *ret)
{
	int update, check;
	addr left, result, pos, cons;

	result = Nil;
	update = 0;
	for (cons = right; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		if (update) {
			cons_local(local, &result, left, result);
		}
		else {
			Return(extpaircall_right_(local, call, left, right, &pos, &check));
			if (check < 0)
				extpaircall_pushlist(local, &result, pos, result);
			else
				cons_local(local, &result, check? pos: left, result);
			if (check)
				update = 1;
		}
	}
	if (update)
		nreverse(value, result);
	return Result(ret, update);
}

static int extpaircall_(LocalRoot local, extpairtype call, addr *cons, int *update)
{
	int loop, check;
	addr pos;

	pos = *cons;
	loop = 0;
	for (;;) {
		Return(extpaircall_left_(local, call, pos, &pos, &check));
		if (! check)
			break;
		loop = 1;
	}
	if (loop) {
		*cons = pos;
		*update = 1;
	}

	return 0;
}

/* check only */
static int range_or_check_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	Check(RefLispDecl(left) != RefLispDecl(right), "type error");
	Check(RefNotDecl(left) || RefNotDecl(right), "not error");
	return Result(ret, 0);
}

/* (? ?) (* *) -> delete */
static int range_or_aster_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	if (range_asterisk_p(right)) {
		*value = Nil;
		return Result(ret, -1); /* list */
	}

	return Result(ret, 0);
}

/* (20 ?) (10 *) -> delete */
static int range_or_left_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_left_any_p(left))
		return Result(ret, 0);
	if (! range_left_p(right))
		return Result(ret, 0);
	Return(range_left_left_less_equal_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (? 10) (* 20) -> delete */
static int range_or_right_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_any_right_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_right_right_less_equal_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (10 *) (* 20) -> (10 20) */
static int range_or_left_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_left_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_connect_right_left_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	type4aster_local(local, RefLispDecl(left), value);
	return Result(ret, 1);
}

/* (10 30) (20 *) -> (10 *) */
static int range_or_range_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_left_p(right))
		return Result(ret, 0);
	Return(range_connect_between_left_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_aster_(local, value, left));
	return Result(ret, 1);
}

/* (10 30) (* 20) -> (* 30) */
static int range_or_range_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_connect_between_right_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_aster_right_(local, value, left));
	return Result(ret, 1);
}

/* (21 22) (10 30) -> delete */
static int range_or_range_range_in_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_in_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (10 30) (20 40) -> (10 30) */
static int range_or_range_range_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_left_(left, right, &check));
	if (! check)
		return Result(ret, 0);
	Return(range_between_right_(left, right, &check));
	if (check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_right_(local, value, left, right));
	return Result(ret, 1);
}

/* (20 40) (10 30) -> (10 40) */
static int range_or_range_range_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_left_(left, right, &check));
	if (check)
		return Result(ret, 0);
	Return(range_between_right_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_right_(local, value, right, left));
	return Result(ret, 1);
}

#define Return_extpaircall(a,b,c,d) Return(extpaircall_((a),(b),(c),(d)))
static int merge_range_orplus_(LocalRoot local, addr *ret, addr left, addr right)
{
	int update, result;

	append2_local_unsafe(local, left, right, &left);
	for (result = 0; ; result |= update) {
		update = 0;
		Return_extpaircall(local, range_or_check_, &left, &update);
		Return_extpaircall(local, range_or_aster_, &left, &update);
		Return_extpaircall(local, range_or_left_left_, &left, &update);
		Return_extpaircall(local, range_or_right_right_, &left, &update);
		Return_extpaircall(local, range_or_left_right_, &left, &update);
		Return_extpaircall(local, range_or_range_left_, &left, &update);
		Return_extpaircall(local, range_or_range_right_, &left, &update);
		Return_extpaircall(local, range_or_range_range_in_, &left, &update);
		Return_extpaircall(local, range_or_range_range_left_, &left, &update);
		Return_extpaircall(local, range_or_range_range_right_, &left, &update);
		if (update == 0)
			break;
	}

	return Result(ret, left);
}

static int range_or_otherwise_(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t size, i;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		Return(merge_range_type_(local, &right, right, decl));
		if (right == Nil) {
			continue;
		}
		if (right == T) {
			left = T;
			break;
		}
		if (left == Nil) {
			left = right;
		}
		else {
			Return(merge_range_orplus_(local, &left, left, right));
			if (left == T)
				break;
		}
	}

	return Result(ret, left);
}

static int merge_range_or_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0)
		return Result(ret, Nil);
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return 0;
	}

	return range_or_otherwise_(local, ret, type, decl);
}

static int merge_range_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			return merge_range_and_(local, ret, type, decl);

		case LISPDECL_OR:
			return merge_range_or_(local, ret, type, decl);

		default:
			merge_range_cons(local, ret, type, decl);
			return 0;
	}
}

static void type_or_cons(LocalRoot local, addr *ret, addr cons)
{
	addr array, pos;
	size_t i, size;

	array = cons;
	for (size = 0; array != Nil; size++) {
		GetCdr(array, &array);
	}
	vector4_local(local, &array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static int make_merge_range_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	Return(merge_range_type_(local, &type, type, decl));
	if (type == Nil)
		return Result(ret, Nil);
	if (type == T) {
		type4aster_local(local, decl, ret);
		return 0;
	}
	if (singlep(type)) {
		GetCar(type, ret);
		return 0;
	}
	if (GetType(type) == LISPTYPE_CONS) {
		type_or_cons(local, ret, type);
		return 0;
	}

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int merge_range_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	int ignore;

	Return(real_filter_(local, &type, type, decl));
	if (type != Nil) {
		Return(type_optimize_local_(local, type, &type, &ignore));
		get_type_optimized(&type, type);
	}
	if ((type == Nil) || (RefLispDecl(type) == LISPDECL_NIL))
		return Result(ret, Nil);

	return make_merge_range_(local, ret, type, decl);
}


/*
 *  real_extract
 */
static const enum LISPDECL RealFilterDecls[] = {
	LISPDECL_INTEGER,
	LISPDECL_RATIONAL,
	LISPDECL_SHORT_FLOAT,
	LISPDECL_SINGLE_FLOAT,
	LISPDECL_DOUBLE_FLOAT,
	LISPDECL_LONG_FLOAT,
	LISPDECL_EMPTY
};

static int real_filter_range_list_(LocalRoot local, addr type,
		addr *value, size_t *ret)
{
	addr cons, check;
	size_t i, size;
	enum LISPDECL decl;

	cons = Nil;
	for (i = size = 0; ; i++) {
		decl = RealFilterDecls[i];
		if (decl == LISPDECL_EMPTY)
			break;
		Return(merge_range_(local, &check, type, decl));
		if (check != Nil) {
			cons_local(local, &cons, check, cons);
			size++;
		}
	}
	*value = cons;

	return Result(ret, size);
}

static void real_reject(LocalRoot local, addr *ret, addr type)
{
	addr pos;

	/* (and (not real) [type]) */
	vector4_local(local, &pos, 2);
	SetArrayA4(pos, 1, type);
	type4aster_local(local, LISPDECL_REAL, &type);
	SetNotDecl(type, 1);
	SetArrayA4(pos, 0, type);
	type1_local(local, LISPDECL_AND, pos, ret);
}

static void copy_cons_to_vector4_local(LocalRoot local,
		addr *ret, addr cons, size_t size)
{
	addr array, pos;
	size_t i;

	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	*ret = array;
}

static int make_real_filter_(LocalRoot local, addr *ret, addr type)
{
	addr pos;
	size_t size;

	Return(real_filter_range_list_(local, type, &pos, &size));
	if (size == 0)
		return Result(ret, type);
	real_reject(local, &type, type);
	cons_local(local, &pos, type, pos);
	nreverse(&pos, pos);
	copy_cons_to_vector4_local(local, &pos, pos, size + 1UL);
	type1_local(local, LISPDECL_OR, pos, ret);

	return 0;
}

static int real_extract_(LocalRoot local, addr *ret, addr type)
{
	int ignore;

	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(&type, type);
	Return(make_real_filter_(local, &type, type));
	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(ret, type);

	return 0;
}

int real_extract_local_(LocalRoot local, addr *ret, addr type)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_SUBTYPEP) {
		*ret = type;
	}
	else {
		Return(real_extract_(local, &type, type));
		type1_local(local, LISPDECL_SUBTYPEP, type, ret);
	}

	return 0;
}

int real_extract_heap_(LocalRoot local, addr *ret, addr type)
{
	LocalStack stack;

	CheckLocal(local);
	push_local(local, &stack);
	Return(real_extract_local_(local, &type, type));
	type_copy_heap(ret, type);
	rollback_local(local, stack);

	return 0;
}

int type_subtypep_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_SUBTYPEP;
}

int type_optimized_or_subtypep(addr type)
{
	return type_optimized_p(type) || type_subtypep_p(type);
}

void get_type_subtypep(addr *ret, addr type)
{
	if (type_subtypep_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

int type_subtypep_throw_heap_(LocalRoot local, addr type, addr *ret)
{
	Return(real_extract_heap_(local, &type, type));
	get_type_subtypep(ret, type);
	return 0;
}


/************************************************************
 *  subtypep_optimize.c
 ************************************************************/

/*
 *  macro
 */
static int type_optimize_(LocalRoot local, addr type, addr *value, int *ret);
static int check_optimize_(addr type, int *ret);
typedef int (*extractcalltype)(addr *, addr);

#define CheckNotDecl(x, type) (RefLispDecl(x) == type && RefNotDecl(x))

#define Return_or_optimize(call, type, ret) { \
	Return(call(type, ret)); \
	if (*ret) { \
		return 0; \
	} \
};

#define Return_check_optimize(call, type, ret) { \
	Return(call(type, ret)); \
	if (*ret == 0) { \
		return 0; \
	} \
};

#define extractcall(local, call, pos, update) { \
	int __check = 0; \
	addr __value = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	for (;;) { \
		Return(call(local, pos, &__value, &__check)); \
		if (! __check) { \
			break; \
		} \
		update = 1; \
		pos = __value; \
	} \
}

#define extractcallnot(local, call, pos, update) { \
	int __check = 0; \
	addr __value = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	for (;;) { \
		Return(call(local, pos, &__value, &__check)); \
		if (! __check) { \
			break; \
		}\
		update = 1; \
		if (RefNotDecl(pos)) { \
			type_revnotdecl(__value); \
		} \
		pos = __value; \
	} \
}


/*
 *  optimize
 */
static int check_type_error_(addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_ERROR)
		return Result(ret, 0);
	GetArrayType(pos, 1, &pos);
	if (pos == Nil)
		return Result(ret, 1);

	return check_optimize_(pos, ret);
}

static int optimize_type_error_(LocalRoot local, addr pos, addr *value, int *ret)
{
	Return_check_optimize(check_type_error_, pos, ret);
	Return(get_error_type_(Execute_Thread, pos, &pos));
	return type_optimize_(local, pos, value, ret);
}

static int check_optimized_(addr right, int *ret)
{
	enum LISPDECL type;

	GetLispDecl(right, &type);
	*ret = (type == LISPDECL_OPTIMIZED || type == LISPDECL_SUBTYPEP);

	return 0;
}

static int optimize_optimized_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_optimized_, right, ret);

	if (! RefNotDecl(right)) {
		GetArrayType(right, 0, value);
	}
	else {
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*value = right;
	}

	return Result(ret, 1);
}

static int check_not_asterisk_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_ASTERISK);
	return 0;
}
static int optimize_not_asterisk_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_asterisk_, right, ret);

	/* error */
	return fmte_("Don't allow to use (not *).", NULL);
}

static int check_not_nil_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_NIL);
	return 0;
}
static int optimize_not_nil_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_nil_, right, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

static int check_not_t_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_T);
	return 0;
}
static int optimize_not_t_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_t_, right, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (mod size) -> (integer 0 (size)) */
static int check_mod_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_MOD);
	return 0;
}
static int optimize_mod_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left, pos;

	Return_check_optimize(check_mod_, right, ret);
	GetArrayType(right, 0, &left);
	Check(! integerp(left), "type error");
	Check(! plusp_integer_debug(left), "plusp error");
	fixnum_local(local, &pos, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, pos, T, left, value);

	return Result(ret, 1);
}

/* atom -> (not cons) */
static int check_atom_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_ATOM);
	return 0;
}
static int optimize_atom_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_atom_, right, ret);
	type2aster_localall(local, LISPDECL_CONS, &left);
	SetNotDecl(left, 1);
	*value = left;

	return Result(ret, 1);
}

/* list -> (or null cons) */
static int check_list_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_LIST);
	return 0;
}
static int optimize_list_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array;

	Return_check_optimize(check_list_, right, ret);
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* boolean -> (or null (eql t)) */
static int check_boolean_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BOOLEAN);
	return 0;
}
static int optimize_boolean_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array;

	Return_check_optimize(check_boolean_, right, ret);
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* (eql t) */
	type_eql_local(local, T, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/*
 *  (... type *) -> (array type 1)
 *  (... type size) -> (array type (size))
 */
static int extract_vector(LocalRoot local,
		addr *ret, enum LISPDECL decl, addr type, addr size)
{
	addr array;

	if (type_asterisk_p(size)) {
		fixnum_local(local, &size, 1);
		type2_local(local, decl, type, size, ret);
		return 1;
	}
	if (GetType(size) == LISPTYPE_FIXNUM) {
		vector4_local(local, &array, 1);
		SetArrayA4(array, 0, size);
		type2_local(local, decl, type, array, ret);
		return 1;
	}

	return 0;
}

/* sequence -> (or null cons (array * 1)) */
static int check_sequence_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SEQUENCE);
	return 0;
}

static int optimize_sequence_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array, type;

	Return_check_optimize(check_sequence_, right, ret);
	vector4_local(local, &array, 3);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* (array * 1) */
	type0_local(local, LISPDECL_ASTERISK, &type);
	fixnum_local(local, &pos, 1);
	type2_local(local, LISPDECL_ARRAY, type, pos, &pos);
	SetArrayA4(array, 2, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* (vector type size) -> (array type (size)) */
static int check_vector_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_VECTOR)
		return Result(ret, 0);
	GetArrayType(right, 1, &right);
	*ret = (type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM);

	return 0;
}
static int optimize_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_vector_, right, ret);
	GetArrayType(right, 0, &type);
	GetArrayType(right, 1, &right);
	extract_vector(local, value, LISPDECL_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-vector size) -> (simple-array t (size)) */
static int check_vector_type(enum LISPDECL decl, addr right)
{
	if (RefLispDecl(right) != decl)
		return 0;
	GetArrayType(right, 0, &right);
	return type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM;
}
static int check_simple_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_SIMPLE_VECTOR, right);
	return 0;
}
static int optimize_simple_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_vector_, right, ret);
	upgraded_array_t_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (bit-vector size) -> (array bit (size)) */
static int check_bit_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_BIT_VECTOR, right);
	return 0;
}
static int optimize_bit_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_bit_vector_, right, ret);
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-bit-vector size) -> (simple-array bit (size)) */
static int check_simple_bit_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_SIMPLE_BIT_VECTOR, right);
	return 0;
}
static int optimize_simple_bit_vector_(
		LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_bit_vector_, right, ret);
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* extended-char -> (and character (not base-char)) */
static int check_extended_char_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_EXTENDED_CHAR);
	return 0;
}
static int optimize_extended_char_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr array;

	Return_check_optimize(check_extended_char_, right, ret);
	vector4_local(local, &array, 2);
	/* character */
	type0_local(local, LISPDECL_CHARACTER, &right);
	SetArrayA4(array, 0, right);
	/* (not base-char) */
	type0_local(local, LISPDECL_BASE_CHAR, &right);
	SetNotDecl(right, 1);
	SetArrayA4(array, 1, right);
	/* result */
	type1_local(local, LISPDECL_AND, array, value);

	return Result(ret, 1);
}

/* (string size) -> (vector character size) */
static void extract_string(LocalRoot local, addr *value, addr right, addr type)
{
	GetArrayType(right, 0, &right);
	type2_local(local, LISPDECL_VECTOR, type, right, value);
}

static int check_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_STRING);
	return 0;
}
static int optimize_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_string_, right, ret);
	upgraded_array_character_local(local, &type);
	extract_string(local, value, right, type);

	return Result(ret, 1);
}

/* (base-string size) -> (vector base-char size) */
static int check_base_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BASE_STRING);
	return 0;
}
static int optimize_base_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_base_string_, right, ret);
	upgraded_array_character_local(local, &type);
	extract_string(local, value, right, type);

	return Result(ret, 1);
}

/* (simple-string size) -> (simple-array character (size)) */
static int check_simple_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIMPLE_STRING);
	return 0;
}
static int optimize_simple_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_string_, right, ret);
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-base-string size) -> (simple-array base-char (size)) */
static int check_simple_base_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIMPLE_BASE_STRING);
	return 0;
}
static int optimize_simple_base_string_(
		LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_base_string_, right, ret);
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (signed-byte *) -> integer */
/* (signed-byte size) -> (integer -2^size-1 2^(size-1)-1) */
static int check_signed_byte_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIGNED_BYTE);
	return 0;
}
static int optimize_signed_byte_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr x, y;

	Return_check_optimize(check_signed_byte_, type, ret);

	/* asterisk */
	GetArrayType(type, 0, &y);
	if (type_asterisk_p(y)) {
		/* (signed-byte *) */
		type4aster_localall(local, LISPDECL_INTEGER, value);
		return Result(ret, 1);
	}

	/*  (let ((v (ash 1 (1- value))))
	 *    `(integer ,(- v) ,(1- v)))
	 */
	fixnum_heap(&x, 1);
	Return(oneminus_integer_common_(local, y, &y));
	Return(ash_integer_common_(local, x, y, &y));
	Return(sign_reverse_integer_common_(y, &x));
	Return(oneminus_integer_common_(local, y, &y));
	Return(integer_result_local_(local, x, &x));
	Return(integer_result_local_(local, y, &y));
	type4_local(local, LISPDECL_INTEGER, Nil, x, Nil, y, value);

	return Result(ret, 1);
}

/* (unsigned-byte *) -> (integer 0 *) */
/* (unsigned-byte size) -> (integer 0 2^size-1) */
static int check_unsigned_byte_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_UNSIGNED_BYTE);
	return 0;
}
static int optimize_unsigned_byte_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr x, y;

	Return_check_optimize(check_unsigned_byte_, type, ret);

	/* asterisk */
	GetArrayType(type, 0, &y);
	if (type_asterisk_p(y)) {
		/* (unsigned-byte *) */
		fixnum_local(local, &x, 0);
		type4_local(local, LISPDECL_INTEGER, Nil, x, y, y, value);
		return Result(ret, 1);
	}

	/*  (let ((v (ash 1 value)))
	 *    `(integer 0 (,v)))
	 */
	fixnum_heap(&x, 1);
	Return(ash_integer_common_(local, x, y, &y));
	Return(oneminus_integer_common_(local, y, &y));
	Return(integer_result_local_(local, y, &y));
	fixnum_heap(&x, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, x, Nil, y, value);

	return Result(ret, 1);
}

/* bit -> (integer 0 1) */
static int check_bit_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BIT);
	return 0;
}
static int optimize_bit_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_bit_, right, ret);
	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 1);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* fixnum -> (integer most-negative-fixnum most-positive-fixnum) */
static int check_fixnum_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_FIXNUM);
	return 0;
}
static int optimize_fixnum_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_fixnum_, right, ret);
	GetConst(FIXNUM_MIN, &left);
	GetConst(FIXNUM_MAX, &right);
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type left error");
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* bignum -> (and integer (not fixnum)) */
static int check_bignum_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BIGNUM);
	return 0;
}
static int optimize_bignum_(LocalRoot local, addr right, addr *value, int *ret)
{
	int ignore;
	addr array, pos;

	Return_check_optimize(check_bignum_, right, ret);
	vector4_local(local, &array, 2);
	/* integer */
	type4aster_localall(local, LISPDECL_INTEGER, &pos);
	SetArrayA4(array, 0, pos);
	/* (not fixnum) */
	type0_local(local, LISPDECL_FIXNUM, &pos);
	SetNotDecl(pos, 1);
	Return(type_optimize_(local, pos, &pos, &ignore));
	SetArrayA4(array, 1, pos);
	/* bignum */
	type1_local(local, LISPDECL_AND, array, value);

	return Result(ret, 1);
}

/* (integer (10) (20)) -> (integer 9 19) */
static int check_integer_p(addr pos)
{
	return (! type_asterisk_p(pos)) && (pos != Nil);
}

static int check_integer_(addr type, int *ret)
{
	addr pos;

	if (RefLispDecl(type) != LISPDECL_INTEGER)
		return Result(ret, 0);

	/* left */
	GetArrayType(type, 0, &pos);
	if (check_integer_p(pos))
		return Result(ret, 1);

	/* right */
	GetArrayType(type, 2, &pos);
	if (check_integer_p(pos))
		return Result(ret, 1);

	/* else */
	return Result(ret, 0);
}

static int optimize_integer_less_p(addr x, addr y, int *ret)
{
	if (integerp(x) && integerp(y))
		return less_integer_(y, x, ret);

	return Result(ret, 0);
}

static int optimize_integer_(LocalRoot local, addr type, addr *value, int *ret)
{
	int check;
	addr a1, a2, v1, v2, pos;

	Return_check_optimize(check_integer_, type, ret);
	GetArrayType(type, 0, &a1);
	GetArrayType(type, 1, &v1);
	GetArrayType(type, 2, &a2);
	GetArrayType(type, 3, &v2);

	/* left */
	GetArrayType(type, 0, &pos);
	if (check_integer_p(pos)) {
		Return(oneplus_integer_common_(local, v1, &v1));
		Return(integer_result_local_(local, v1, &v1));
		a1 = Nil;
	}

	/* right */
	GetArrayType(type, 2, &pos);
	if (check_integer_p(pos)) {
		Return(oneminus_integer_common_(local, v2, &v2));
		Return(integer_result_local_(local, v2, &v2));
		a2 = Nil;
	}

	/* result */
	Return(optimize_integer_less_p(v1, v2, &check));
	if (check)
		type0_local(local, LISPDECL_NIL, value);
	else
		type4_local(local, LISPDECL_INTEGER, a1, v1, a2, v2, value);
	type_setnotobject(*value, type);

	return Result(ret, 1);
}

/* (eql nil) -> null */
static int check_eql_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_EQL)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, right == Nil);
}
static int optimize_eql_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_eql_, right, ret);
	type0_local(local, LISPDECL_NULL, value);
	return Result(ret, 1);
}

/* (eql 10) -> (integer 10 10) */
static void optimize_eql_range_object(
		LocalRoot local, enum LISPDECL decl, addr pos, addr *value)
{
	type4_local(local, decl, Nil, pos, Nil, pos, value);
}

static int optimize_eql_range_type_(LocalRoot local, addr pos, addr *value, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			optimize_eql_range_object(local, LISPDECL_INTEGER, pos, value);
			break;

		case LISPTYPE_RATIO:
			optimize_eql_range_object(local, LISPDECL_RATIONAL, pos, value);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			optimize_eql_range_object(local, LISPDECL_SINGLE_FLOAT, pos, value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			optimize_eql_range_object(local, LISPDECL_DOUBLE_FLOAT, pos, value);
			break;

		case LISPTYPE_LONG_FLOAT:
			optimize_eql_range_object(local, LISPDECL_LONG_FLOAT, pos, value);
			break;

		default:
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_eql_range_(addr right, int *ret)
{
	enum LISPTYPE type;

	if (RefLispDecl(right) != LISPDECL_EQL)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	type = GetType(right);
	*ret = type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT;
	return 0;
}
static int optimize_eql_range_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_eql_range_, right, ret);
	GetArrayType(right, 0, &right);
	return optimize_eql_range_type_(local, right, value, ret);
}

/* (member) -> nil */
static int check_member1_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, LenArrayA4r(right) == 0);
}
static int optimize_member1_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_member1_, right, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (member arg) -> (eql arg) */
static int check_member2_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, LenArrayA4r(right) == 1);
}
static int optimize_member2_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_member2_, right, ret);
	GetArrayType(right, 0, &right);
	GetArrayA4(right, 0, &right);
	type_eql_local(local, right, value);
	return Result(ret, 1);
}

/* (member ...) -> (or (eql arg1) (eql arg2) ...) */
static int check_member3_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, 2 <= LenArrayA4r(right));
}
static int optimize_member3_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr array, child;
	size_t i, size;

	Return_check_optimize(check_member3_, right, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &child);
		type_eql_local(local, child, &child);
		SetArrayA4(array, i, child);
	}
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* (not x) -> x.not */
static int check_not_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_NOT);
	return 0;
}
static int optimize_not_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_, right, ret);
	if (RefNotDecl(right)) {
		/* not not */
		GetArrayType(right, 0, value);
	}
	else {
		/* not */
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*value = right;
	}

	return Result(ret, 1);
}

/* (not (and ... )) -> (or (not ...) (not ...) ...) */
/* (not (or ... )) -> (and (not ...) (not ...) ...) */
static int optimize_result_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int check;
	addr opt;

	Return(type_optimize_(local, pos, &opt, &check));
	*value = check? opt: pos;

	if (ret)
		return Result(ret, check);
	else
		return 0;
}

static int extract_not_andor_(LocalRoot local,
		addr *value, addr right, enum LISPDECL decl)
{
	addr array, pos;
	size_t size, i;

	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		type_copy_unsafe_local(local, &pos, pos);
		type_revnotdecl(pos);
		Return(optimize_result_(local, pos, &pos, NULL));
		SetArrayA4(array, i, pos);
	}
	type1_local(local, decl, array, value);

	return 0;
}

static int extract_array_andor_(LocalRoot local, addr right, addr *value, int *ret)
{
	int update, check;
	addr array, temp, pos;
	size_t size, i;

	GetArrayType(right, 0, &array);
	LenArrayA4(array, &size);
	vector4_local(local, &temp, size);
	update = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &pos);
		Return(optimize_result_(local, pos, &pos, &check));
		update |= check;
		SetArrayA4(temp, i, pos);
	}

	if (update) {
		type_copy_unsafe_local(local, &right, right);
		vector4_local(local, &array, size);
		for (i = 0; i < size; i++) {
			GetArrayA4(temp, i, &pos);
			SetArrayA4(array, i, pos);
		}
		SetArrayType(right, 0, array);
		*value = right;
	}

	return Result(ret, update);
}

static int extract_andor_(LocalRoot local,
		addr right, addr *value, int *ret,
		enum LISPDECL fromdecl, enum LISPDECL todecl)
{
	if (RefLispDecl(right) != fromdecl)
		return Result(ret, 0);
	if (RefNotDecl(right)) {
		Return(extract_not_andor_(local, value, right, todecl));
		return Result(ret, 1);
	}

	return extract_array_andor_(local, right, value, ret);
}

static int check_andor_(enum LISPDECL decl, addr right, int *ret)
{
	int check;
	addr pos;
	size_t size, i;

	if (RefLispDecl(right) != decl)
		return Result(ret, 0);
	if (RefNotDecl(right))
		return Result(ret, 1);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		Return(check_optimize_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int check_and_(addr right, int *ret)
{
	return check_andor_(LISPDECL_AND, right, ret);
}
static int optimize_and_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_and_, right, ret);
	return extract_andor_(local, right, value, ret, LISPDECL_AND, LISPDECL_OR);
}

static int check_or_(addr right, int *ret)
{
	return check_andor_(LISPDECL_OR, right, ret);
}
static int optimize_or_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_or_, right, ret);
	return extract_andor_(local, right, value, ret, LISPDECL_OR, LISPDECL_AND);
}


/*
 *  and
 */
static int normlispdecl(addr pos, enum LISPDECL type)
{
	return RefLispDecl(pos) == type && (! RefNotDecl(pos));
}

static int check_typeand(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

/* (and) -> t */
static int check_and1_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 0);
}
static int optimize_and1_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and1_, type, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

/* (and type) -> type */
static int check_and2_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 1);
}
static int optimize_and2_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and2_, type, ret);
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, value);
	return Result(ret, 1);
}

/* (and ... nil ...) -> nil */
static int check_and_vector_(enum LISPDECL decl, addr type, int *ret)
{
	addr check;
	size_t size, i;

	if (check_typeand(type, &type, &size))
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_and3_(addr type, int *ret)
{
	return check_and_vector_(LISPDECL_NIL, type, ret);
}
static int optimize_and3_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and3_, type, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (and ... t ...) -> (and ...)  remove t */
static void remove_type_vector(LocalRoot local,
		enum LISPDECL decl, enum LISPDECL checktype,
		addr array, size_t size1, size_t size2, addr *value)
{
	addr pos, check;
	size_t i, k;

	vector4_local(local, &pos, size2);
	k = 0;
	for (i = 0; i < size1; i++) {
		GetArrayA4(array, i, &check);
		if (! normlispdecl(check, checktype)) {
			Check(size2 <= k, "size2 error1");
			SetArrayA4(pos, k++, check);
		}
	}
	Check(k != size2, "size2 error2");
	type1_local(local, decl, pos, value);
}

static int check_and4_(addr type, int *ret)
{
	return check_and_vector_(LISPDECL_T, type, ret);
}
static int optimize_and4_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr check;
	size_t size, i, count;

	Return_check_optimize(check_and4_, type, ret);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_T))
			count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_AND, LISPDECL_T,
			type, size, size - count, value);

	return Result(ret, 1);
}

/* (and ... (and ...) ...) -> (and ...) */
static int count_andor(addr type, enum LISPDECL decl, size_t *index)
{
	int result;
	addr check;
	size_t size, i;

	Check(! normlispdecl(type, decl), "type error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	result = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			result = 1;
			count_andor(check, decl, index);
		}
		else {
			(*index)++;
		}
	}

	return result;
}

static void replace_andor(addr type, enum LISPDECL decl, addr array, size_t *index)
{
	addr check;
	size_t size, i;

	Check(! normlispdecl(type, decl), "type error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			replace_andor(check, decl, array, index);
		}
		else {
			SetArrayA4(array, (*index)++, check);
		}
	}
}

static int check_andor_type_(enum LISPDECL decl, addr type, int *ret)
{
	addr pos;
	size_t size, i;

	if (! normlispdecl(type, decl))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		if (normlispdecl(pos, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_and5_(addr type, int *ret)
{
	return check_andor_type_(LISPDECL_AND, type, ret);
}
static int optimize_and5_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr array;
	size_t size;

	/* check */
	Return_check_optimize(check_and5_, type, ret);
	GetArrayType(type, 0, &array);
	size = 0;
	count_andor(type, LISPDECL_AND, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_AND, array, value);
	size = 0;
	replace_andor(type, LISPDECL_AND, array, &size);

	return Result(ret, 1);
}


/*
 *  or
 */
/* (or) -> nil */
static int check_typeor(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

static int check_or1_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 0);
}
static int optimize_or1_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or1_, type, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (or type) -> type */
static int check_or2_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 1);
}
static int optimize_or2_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or2_, type, ret);
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, value);
	return Result(ret, 1);
}

/* (or ... t ...) -> t */
static int check_or_vector_(enum LISPDECL decl, addr type, int *ret)
{
	addr check;
	size_t size, i;

	if (check_typeor(type, &type, &size))
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_or3_(addr type, int *ret)
{
	return check_or_vector_(LISPDECL_T, type, ret);
}
static int optimize_or3_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or3_, type, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

/* (or ... nil ...) -> (or ...)  remove nil */
static int check_or4_(addr type, int *ret)
{
	return check_or_vector_(LISPDECL_NIL, type, ret);
}
static int optimize_or4_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr check;
	size_t size, i, count;

	Return_check_optimize(check_or4_, type, ret);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_NIL))
			count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_OR, LISPDECL_NIL,
			type, size, size - count, value);

	return Result(ret, 1);
}

/* (or ... (or ...) ...) -> (or ...) */
static int check_or5_(addr type, int *ret)
{
	return check_andor_type_(LISPDECL_OR, type, ret);
}
static int optimize_or5_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr array;
	size_t size;

	/* check */
	Return_check_optimize(check_or5_, type, ret);
	GetArrayType(type, 0, &array);
	LenArrayA4(array, &size);
	size = 0;
	count_andor(type, LISPDECL_OR, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_OR, array, value);
	size = 0;
	replace_andor(type, LISPDECL_OR, array, &size);

	return Result(ret, 1);
}

/* range check */
static int range_valid_p_(addr type, int *ret)
{
	addr left1, left2, right1, right2;
	LocalRoot local;

	local = Local_Thread;
	GetArrayType(type, 0, &left1);
	GetArrayType(type, 1, &left2);
	GetArrayType(type, 2, &right1);
	GetArrayType(type, 3, &right2);
	if (type_asterisk_p(left1) || type_asterisk_p(right1))
		return Result(ret, 1);
	if (left1 == Nil && right1 == Nil)
		return less_equal_real_(local, left2, right2, ret);
	else
		return less_real_(local, left2, right2, ret);
}
static int check_range_(addr right, int *ret)
{
	int check;

	if (! type_range_p(right))
		return Result(ret, 0);
	Return(range_valid_p_(right, &check));
	return Result(ret, ! check);
}
static int optimize_range_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_range_, right, ret);
	if (RefNotDecl(right))
		type0_local(local, LISPDECL_T, value);
	else
		type0_local(local, LISPDECL_NIL, value);

	return Result(ret, 1);
}


/*
 *  wake optimize
 */
static int extract_values_var_(LocalRoot local, addr right, addr *value)
{
	addr root, left;

	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(optimize_result_(local, left, &left, NULL));
		cons_local(local, &root, left, root);
	}
	nreverse(value, root);

	return 0;
}

static int extract_values_rest_(LocalRoot local, addr right, addr *value)
{
	int ignore;

	if (right == Nil)
		return 0;
	else
		return optimize_result_(local, right, value, &ignore);
}

static int check_some_(addr right, int *ret)
{
	int check;
	addr pos;

	while (right != Nil) {
		GetCons(right, &pos, &right);
		Return(check_optimize_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int check_values_rest_(addr right, int *ret)
{
	if (right == Nil)
		return Result(ret, 0);
	else
		return check_optimize_(right, ret);
}

static int check_values_(addr right, int *ret)
{
	int check;
	addr value;

	if (RefLispDecl(right) != LISPDECL_VALUES)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 1, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 2, &value);
	return check_values_rest_(value, ret);
}

static int optimize_values_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr var, opt, rest;

	/* extract */
	Return_check_optimize(check_values_, right, ret);
	GetArrayType(right, 0, &var);
	GetArrayType(right, 1, &opt);
	GetArrayType(right, 2, &rest);
	Return(extract_values_var_(local, var, &var));
	Return(extract_values_var_(local, opt, &opt));
	Return(extract_values_rest_(local, rest, &rest));

	/* result */
	type_copy_unsafe_local(local, &right, right);
	SetArrayType(right, 0, var);
	SetArrayType(right, 1, opt);
	SetArrayType(right, 2, rest);
	*value = right;

	return Result(ret, 1);
}

static int extract_function_key_(LocalRoot local, addr right, addr *value)
{
	addr root, left, key, type;

	if (right == T)
		return Result(value, T);
	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		GetCons(left, &key, &type);
		Return(optimize_result_(local, type, &type, NULL));
		cons_local(local, &left, key, type);
		cons_local(local, &root, left, root);
	}
	nreverse(value, root);

	return 0;
}

static int extract_function_(LocalRoot local, addr right, addr *value)
{
	addr var, opt, rest, key;

	/* extract */
	if (type_asterisk_p(right))
		return 0;
	GetArrayA2(right, 0, &var);
	GetArrayA2(right, 1, &opt);
	GetArrayA2(right, 2, &rest);
	GetArrayA2(right, 3, &key);
	Return(extract_values_var_(local, var, &var));
	Return(extract_values_var_(local, opt, &opt));
	Return(extract_values_rest_(local, rest, &rest));
	Return(extract_function_key_(local, key, &key));

	/* result */
	vector2_local(local, &right, 4);
	SetArrayA2(right, 0, var);
	SetArrayA2(right, 1, opt);
	SetArrayA2(right, 2, rest);
	SetArrayA2(right, 3, key);

	return Result(value, right);
}

static int check_function_key_(addr right, int *ret)
{
	int check;
	addr type;

	if (right == T)
		return Result(ret, 0);
	while (right != Nil) {
		GetCons(right, &type, &right);
		GetCdr(type, &type);
		Return(check_optimize_(type, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_function_args_(addr right, int *ret)
{
	int check;
	addr value;

	if (type_asterisk_p(right))
		return Result(ret, 0);
	GetArrayA2(right, 0, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 1, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 2, &value);
	Return(check_values_rest_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 3, &value);
	return check_function_key_(value, ret);
}
static int check_function_(addr right, int *ret)
{
	int check;
	enum LISPDECL decl;
	addr value;

	decl = RefLispDecl(right);
	if (decl != LISPDECL_FUNCTION && decl != LISPDECL_COMPILED_FUNCTION)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	Return(check_function_args_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 1, &value);

	return check_optimize_(value, ret);
}
static int optimize_function_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr args, values;

	Return_check_optimize(check_function_, right, ret);
	GetArrayType(right, 0, &args);
	GetArrayType(right, 1, &values);
	Return(extract_function_(local, args, &args));
	Return(optimize_result_(local, values, &values, NULL));
	type_copydecl_unsafe_local(local, &right, right);
	SetArrayType(right, 0, args);
	SetArrayType(right, 1, values);
	*value = right;

	return Result(ret, 1);
}

static int check_cons_(addr right, int *ret)
{
	int check;
	addr value;

	if (RefLispDecl(right) != LISPDECL_CONS)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	if (! type_asterisk_p(value)) {
		Return(check_optimize_(value, &check));
		if (check)
			return Result(ret, 1);
	}
	GetArrayType(right, 1, &value);
	if (! type_asterisk_p(value)) {
		Return(check_optimize_(value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int optimize_cons(LocalRoot local, addr right, addr *value, int *ret)
{
	addr car, cdr;

	Return_check_optimize(check_cons_, right, ret);
	GetArrayType(right, 0, &car);
	GetArrayType(right, 1, &cdr);
	if (! type_asterisk_p(car)) {
		Return(optimize_result_(local, car, &car, NULL));
	}
	if (! type_asterisk_p(cdr)) {
		Return(optimize_result_(local, cdr, &cdr, NULL));
	}
	type2_local(local, LISPDECL_CONS, car, cdr, value);

	return Result(ret, 1);
}


/*
 *  type-optimize
 */
static int check_optimize_(addr type, int *ret)
{
	Return_or_optimize(check_type_error_, type, ret);
	Return_or_optimize(check_optimized_, type, ret);
	Return_or_optimize(check_not_asterisk_, type, ret);
	Return_or_optimize(check_not_nil_, type, ret);
	Return_or_optimize(check_not_t_, type, ret);
	Return_or_optimize(check_mod_, type, ret);
	Return_or_optimize(check_atom_, type, ret);
	Return_or_optimize(check_list_, type, ret);
	Return_or_optimize(check_boolean_, type, ret);
	Return_or_optimize(check_sequence_, type, ret);
	Return_or_optimize(check_vector_, type, ret);
	Return_or_optimize(check_simple_vector_, type, ret);
	Return_or_optimize(check_bit_vector_, type, ret);
	Return_or_optimize(check_simple_bit_vector_, type, ret);
	Return_or_optimize(check_extended_char_, type, ret);
	Return_or_optimize(check_string_, type, ret);
	Return_or_optimize(check_base_string_, type, ret);
	Return_or_optimize(check_simple_string_, type, ret);
	Return_or_optimize(check_simple_base_string_, type, ret);
	Return_or_optimize(check_signed_byte_, type, ret);
	Return_or_optimize(check_unsigned_byte_, type, ret);
	Return_or_optimize(check_bit_, type, ret);
	Return_or_optimize(check_fixnum_, type, ret);
	Return_or_optimize(check_bignum_, type, ret);
	Return_or_optimize(check_integer_, type, ret);
	Return_or_optimize(check_eql_, type, ret);
	Return_or_optimize(check_eql_range_, type, ret);
	Return_or_optimize(check_member1_, type, ret);
	Return_or_optimize(check_member2_, type, ret);
	Return_or_optimize(check_member3_, type, ret);
	Return_or_optimize(check_not_, type, ret);
	Return_or_optimize(check_and_, type, ret);
	Return_or_optimize(check_or_, type, ret);
	Return_or_optimize(check_and1_, type, ret);
	Return_or_optimize(check_and2_, type, ret);
	Return_or_optimize(check_and3_, type, ret);
	Return_or_optimize(check_and4_, type, ret);
	Return_or_optimize(check_and5_, type, ret);
	Return_or_optimize(check_or1_, type, ret);
	Return_or_optimize(check_or2_, type, ret);
	Return_or_optimize(check_or3_, type, ret);
	Return_or_optimize(check_or4_, type, ret);
	Return_or_optimize(check_or5_, type, ret);
	Return_or_optimize(check_range_, type, ret);
	Return_or_optimize(check_values_, type, ret);
	Return_or_optimize(check_function_, type, ret);
	Return_or_optimize(check_cons_, type, ret);

	return Result(ret, 0);
}

static int type_optimize_(LocalRoot local, addr type, addr *value, int *ret)
{
	int update, loop;

	CheckType(type, LISPTYPE_TYPE);
	for (loop = 0; ; loop |= update) {
		update = 0;
		/* extract */
		extractcall(local, optimize_type_error_, type, update);
		extractcall(local, optimize_optimized_, type, update);
		extractcall(local, optimize_not_asterisk_, type, update);
		extractcall(local, optimize_not_nil_, type, update);
		extractcall(local, optimize_not_t_, type, update);
		extractcallnot(local, optimize_mod_, type, update);
		extractcallnot(local, optimize_atom_, type, update);
		extractcallnot(local, optimize_list_, type, update);
		extractcallnot(local, optimize_boolean_, type, update);
		extractcallnot(local, optimize_sequence_, type, update);
		extractcallnot(local, optimize_vector_, type, update);
		extractcallnot(local, optimize_simple_vector_, type, update);
		extractcallnot(local, optimize_bit_vector_, type, update);
		extractcallnot(local, optimize_simple_bit_vector_, type, update);
		extractcallnot(local, optimize_extended_char_, type, update);
		extractcallnot(local, optimize_string_, type, update);
		extractcallnot(local, optimize_base_string_, type, update);
		extractcallnot(local, optimize_simple_string_, type, update);
		extractcallnot(local, optimize_simple_base_string_, type, update);
		extractcallnot(local, optimize_signed_byte_, type, update);
		extractcallnot(local, optimize_unsigned_byte_, type, update);
		extractcallnot(local, optimize_bit_, type, update);
		extractcallnot(local, optimize_fixnum_, type, update);
		extractcallnot(local, optimize_bignum_, type, update);
		extractcallnot(local, optimize_integer_, type, update);
		extractcallnot(local, optimize_eql_, type, update);
		extractcallnot(local, optimize_eql_range_, type, update);
		extractcallnot(local, optimize_member1_, type, update);
		extractcallnot(local, optimize_member2_, type, update);
		extractcallnot(local, optimize_member3_, type, update);
		extractcall(local, optimize_not_, type, update);
		extractcall(local, optimize_and_, type, update);
		extractcall(local, optimize_or_, type, update);
		extractcall(local, optimize_and1_, type, update);
		extractcall(local, optimize_and2_, type, update);
		extractcall(local, optimize_and3_, type, update);
		extractcall(local, optimize_and4_, type, update);
		extractcall(local, optimize_and5_, type, update);
		extractcall(local, optimize_or1_, type, update);
		extractcall(local, optimize_or2_, type, update);
		extractcall(local, optimize_or3_, type, update);
		extractcall(local, optimize_or4_, type, update);
		extractcall(local, optimize_or5_, type, update);
		extractcall(local, optimize_range_, type, update);
		extractcallnot(local, optimize_values_, type, update);
		extractcallnot(local, optimize_function_, type, update);
		extractcallnot(local, optimize_cons, type, update);
		if (update == 0)
			break;
	}
	*value = type;

	return Result(ret, loop);
}

int type_optimize_local_(LocalRoot local, addr type, addr *value, int *ret)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_OPTIMIZED) {
		*value = type;
		return Result(ret, 0);
	}

	Return(type_optimize_(local, type, &type, ret));
	type1_local(local, LISPDECL_OPTIMIZED, type, value);
	return 0;
}

int type_optimize_heap_(LocalRoot local, addr type, addr *value, int *ret)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	push_local(local, &stack);
	Return(type_optimize_local_(local, type, &type, ret));
	type_copy_heap(value, type);
	rollback_local(local, stack);

	return 0;
}

int type_optimized_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_OPTIMIZED;
}

void get_type_optimized(addr *ret, addr type)
{
	if (type_optimized_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

int type_optimize_throw_heap_(LocalRoot local, addr type, addr *ret)
{
	int check;

	Return(type_optimize_heap_(local, type, &type, &check));
	get_type_optimized(ret, type);

	return 0;
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#ifdef __clang__
#pragma clang diagnostic pop
#endif
