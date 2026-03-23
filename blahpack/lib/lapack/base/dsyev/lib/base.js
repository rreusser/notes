/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

// MODULES //

var dlamch = require( '../../dlamch/lib/base.js' );
var dlansy = require( '../../dlansy/lib/base.js' );
var dlascl = require( '../../dlascl/lib/base.js' );
var dsytrd = require( '../../dsytrd/lib/base.js' );
var dorgtr = require( '../../dorgtr/lib/base.js' );
var dsterf = require( '../../dsterf/lib/base.js' );
var dsteqr = require( '../../dsteqr/lib/base.js' );
var dscal = require( '../../../../blas/base/dscal/lib/base.js' );


// VARIABLES //

var NB = 32; // block size (replaces ILAENV query)


// MAIN //

/**
* Computes all eigenvalues and, optionally, eigenvectors of a real symmetric
* matrix A.
*
* The eigenvalues are returned in ascending order. If eigenvectors are
* requested (JOBZ = 'V'), the matrix A is overwritten with the orthonormal
* eigenvectors.
*
* Algorithm:
* 1. Scale the matrix if the norm is outside safe range
* 2. Reduce to tridiagonal form via dsytrd
* 3. If eigenvalues only (JOBZ='N'): compute via dsterf
*    If eigenvectors too (JOBZ='V'): generate Q via dorgtr, then dsteqr
* 4. Undo scaling on eigenvalues if needed
*
* @private
* @param {string} jobz - 'N': eigenvalues only, 'V': eigenvalues and eigenvectors
* @param {string} uplo - 'U': upper triangle stored, 'L': lower triangle stored
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Float64Array} A - input/output symmetric matrix; on exit contains eigenvectors if JOBZ='V'
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} w - output array for eigenvalues (length N), in ascending order
* @param {integer} strideW - stride for w
* @param {NonNegativeInteger} offsetW - starting index for w
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of WORK array
* @returns {integer} info - 0 if successful, >0 if dsteqr/dsterf did not converge
*/
function dsyev( jobz, uplo, N, A, strideA1, strideA2, offsetA, w, strideW, offsetW, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	var wantz;
	var anrm;
	var safmin;
	var eps;
	var smlnum;
	var bignum;
	var rmin;
	var rmax;
	var sigma;
	var iscale;
	var info;
	var iinfo;
	var imax;
	var inde;
	var indtau;
	var indwrk;
	var llwork;

	wantz = ( jobz === 'compute' );

	// Quick return if possible
	if ( N === 0 ) {
		return 0;
	}

	if ( N === 1 ) {
		w[ offsetW ] = A[ offsetA ];
		if ( wantz ) {
			A[ offsetA ] = 1.0;
		}
		return 0;
	}

	// Get machine constants
	safmin = dlamch( 'S' );
	eps = dlamch( 'E' );
	smlnum = safmin / eps;
	bignum = 1.0 / smlnum;
	rmin = Math.sqrt( smlnum );
	rmax = Math.sqrt( bignum );

	// Scale matrix to allowable range, if necessary
	anrm = dlansy( 'max', uplo, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK );
	iscale = 0;
	sigma = 1.0;
	if ( anrm > 0.0 && anrm < rmin ) {
		iscale = 1;
		sigma = rmin / anrm;
	} else if ( anrm > rmax ) {
		iscale = 1;
		sigma = rmax / anrm;
	}
	if ( iscale === 1 ) {
		dlascl( uplo, 0, 0, 1.0, sigma, N, N, A, strideA1, strideA2, offsetA );
	}

	// Partition WORK into E, TAU, and scratch segments
	// Fortran: INDE=1, INDTAU=INDE+N, INDWRK=INDTAU+N
	// JS (0-based): inde=0, indtau=N, indwrk=2*N
	inde = offsetWORK;
	indtau = inde + N * strideWORK;
	indwrk = indtau + N * strideWORK;
	llwork = lwork - 2 * N;

	// Reduce to tridiagonal form: Q^T * A * Q = T
	// dsytrd(uplo, N, A, sa1, sa2, oA, d, sd, od, e, se, oe, TAU, sTAU, oTAU, WORK, sWORK, oWORK, lwork)
	// d (diagonal) goes into w, e (off-diagonal) into WORK[inde], TAU into WORK[indtau]
	dsytrd( uplo, N, A, strideA1, strideA2, offsetA, w, strideW, offsetW, WORK, strideWORK, inde, WORK, strideWORK, indtau, WORK, strideWORK, indwrk, llwork );

	info = 0;
	if ( !wantz ) {
		// Eigenvalues only — use dsterf on the tridiagonal
		info = dsterf( N, w, strideW, offsetW, WORK, strideWORK, inde );
	} else {
		// Generate orthogonal matrix Q from dsytrd output
		dorgtr( uplo, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, indtau, WORK, strideWORK, indwrk, llwork );

		// Compute eigenvalues and eigenvectors of the tridiagonal matrix
		// dsteqr uses WORK[indtau] as its scratch workspace (needs 2*(N-1) space)
		info = dsteqr( ( wantz ? 'initialize' : 'none' ), N, w, strideW, offsetW, WORK, strideWORK, inde, A, strideA1, strideA2, offsetA, WORK, strideWORK, indtau );
	}

	// If matrix was scaled, rescale eigenvalues
	if ( iscale === 1 ) {
		if ( info === 0 ) {
			imax = N;
		} else {
			imax = info - 1;
		}
		dscal( imax, 1.0 / sigma, w, strideW, offsetW );
	}

	return info;
}


// EXPORTS //

module.exports = dsyev;
