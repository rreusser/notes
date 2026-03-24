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

/* eslint-disable max-len, max-params, max-depth, max-statements */

'use strict';

// MODULES //

var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zcopy = require( '../../../../blas/base/zcopy/lib/base.js' );
var zlacpy = require( '../../zlacpy/lib/base.js' );
var zlahqr = require( '../../zlahqr/lib/base.js' );
// var zlaqr0 = require( '../../zlaqr0/lib/base.js' ); // TODO: implement for large matrices
var zlaset = require( '../../zlaset/lib/base.js' );


// VARIABLES //

var CZERO = new Complex128( 0.0, 0.0 );
var CONE = new Complex128( 1.0, 0.0 );
var NTINY = 15;
var NL = 49;

// iparmq ISPEC=12: crossover point for small-to-large strategy
var NMIN = 75;


// MAIN //

/**
* Computes the eigenvalues of a complex upper Hessenberg matrix H, and
* optionally the matrices T and Z from the Schur decomposition
* H = Z * T * Z^H, where T is an upper triangular matrix (the Schur form)
* and Z is the unitary matrix of Schur vectors.
*
* JOB = 'E': compute eigenvalues only.
* JOB = 'S': compute eigenvalues and the Schur form T.
*
* COMPZ = 'N': no Schur vectors are computed.
* COMPZ = 'I': Z is initialized to the identity matrix and the matrix Z
*              of Schur vectors of H is returned.
* COMPZ = 'V': Z must contain a unitary matrix Q on entry, and the
*              product Q*Z is returned.
*
* ILO and IHI are 1-based indices (Fortran convention). The active block
* is H(ILO:IHI, ILO:IHI). Elements outside this block are assumed already
* upper triangular. Normally ILO=1 and IHI=N.
*
* On exit, if INFO = 0, W contains the computed eigenvalues.
* If JOB = 'S', the eigenvalues are stored in the same order as on the
* diagonal of T.
*
* INFO = 0: success.
* INFO > 0: zhseqr failed to compute all eigenvalues. Elements
*           INFO+1:IHI of W contain those eigenvalues which
*           have been successfully computed.
*
* @private
* @param {string} job - 'E' for eigenvalues only, 'S' for Schur form
* @param {string} compz - 'N' no Z, 'I' initialize Z to identity, 'V' update Z
* @param {NonNegativeInteger} N - order of the matrix H
* @param {integer} ilo - first row/col of the active block (1-based)
* @param {integer} ihi - last row/col of the active block (1-based)
* @param {Complex128Array} H - upper Hessenberg matrix (N-by-N)
* @param {integer} strideH1 - stride of the first dimension of `H` (complex elements)
* @param {integer} strideH2 - stride of the second dimension of `H` (complex elements)
* @param {NonNegativeInteger} offsetH - starting index for `H` (complex elements)
* @param {Complex128Array} w - output array for eigenvalues
* @param {integer} strideW - stride length for `w` (complex elements)
* @param {NonNegativeInteger} offsetW - starting index for `w` (complex elements)
* @param {Complex128Array} Z - Schur vectors matrix (N-by-N)
* @param {integer} strideZ1 - stride of the first dimension of `Z` (complex elements)
* @param {integer} strideZ2 - stride of the second dimension of `Z` (complex elements)
* @param {NonNegativeInteger} offsetZ - starting index for `Z` (complex elements)
* @param {Complex128Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - offset for WORK
* @param {integer} lwork - workspace length
* @returns {integer} info - 0 on success, >0 if failed to converge
*/
function zhseqr( job, compz, N, ilo, ihi, H, strideH1, strideH2, offsetH, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork ) {
	var wantt;
	var initz;
	var wantz;
	var info;
	var kbot;
	var WORKL;
	var HL;
	var Hv;
	var wv;
	var sh1;
	var sh2;
	var sw;
	var oH;
	var ow;
	var i;

	// Decode parameters
	wantt = ( job === 'S' );
	initz = ( compz === 'I' );
	wantz = initz || ( compz === 'V' );

	// Quick return if N = 0
	if ( N === 0 ) {
		return 0;
	}

	Hv = reinterpret( H, 0 );
	wv = reinterpret( w, 0 );
	sh1 = strideH1 * 2;
	sh2 = strideH2 * 2;
	sw = strideW * 2;
	oH = offsetH * 2;
	ow = offsetW * 2;

	// Extract eigenvalues of rows/cols outside [ILO, IHI] from the diagonal of H
	// ZCOPY using the diagonal stride LDH+1 in complex elements = strideH1 + strideH2
	if ( ilo > 1 ) {
		zcopy( ilo - 1, H, strideH1 + strideH2, offsetH, w, strideW, offsetW );
	}
	if ( ihi < N ) {
		zcopy( N - ihi, H, strideH1 + strideH2, offsetH + ihi * strideH1 + ihi * strideH2, w, strideW, offsetW + ihi * strideW );
	}

	// Initialize Z to identity if COMPZ = 'I'
	if ( initz ) {
		zlaset( 'A', N, N, CZERO, CONE, Z, strideZ1, strideZ2, offsetZ );
	}

	// If active block is a single element, just read off the eigenvalue
	if ( ilo === ihi ) {
		wv[ ow + ( ilo - 1 ) * sw ] = Hv[ oH + ( ilo - 1 ) * sh1 + ( ilo - 1 ) * sh2 ];
		wv[ ow + ( ilo - 1 ) * sw + 1 ] = Hv[ oH + ( ilo - 1 ) * sh1 + ( ilo - 1 ) * sh2 + 1 ];
		return 0;
	}

	// Use zlahqr (single-shift QR) for all sizes.
	// TODO: For large matrices (N > NMIN), use zlaqr0 (aggressive early deflation).
	info = zlahqr( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, w, strideW, offsetW, ilo, ihi, Z, strideZ1, strideZ2, offsetZ );

	// Clean up the strictly lower triangular part of H (below subdiagonal)
	if ( ( wantt || info !== 0 ) && N > 2 ) {
		zlaset( 'lower', N - 2, N - 2, CZERO, CZERO, H, strideH1, strideH2, offsetH + 2 * strideH1 );
	}

	return info;
}


// EXPORTS //

module.exports = zhseqr;
