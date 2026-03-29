/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
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

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgbmv = require( './../lib' );
var base = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgbmv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// HELPERS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

/**
* Create the 4x4 test band matrix (KL=1, KU=2, LDA=4) as a Complex128Array.
*
* Band storage (column-major, 4 rows per column):
*   Row 1 (2nd superdiag): _        _        a13      a24
_   Row 2 (1st superdiag): _        a12      a23      a34
_   Row 3 (diagonal):      a11      a22      a33      a44
_   Row 4 (subdiag):       a21      a32      a43      _
* Full matrix (derived from actual Fortran index assignments):
*   [ (1,0.5)    (3,1)      (6,-0.5)    0        ]
*   [ (2,-1)     (4,0)      (7,1.5)     0        ]
*   [ 0          (5,2)      (8,-1)     (10,1)    ]
*   [ 0          0          (9,0)      (11,-2)   ]
*
* Note: Fortran a(16)=(12,0.5) lands in band row 4 of col 4, which maps to
* full row i=5 > M=4, so it is padding and ignored by the algorithm.
*
* @returns {Complex128Array} band matrix in band storage
*/
function createBandMatrix44() {
	// 4 rows x 4 cols = 16 complex elements, interleaved as 32 doubles
	// Matches Fortran: a(k) at 0-based complex index (k-1)
	var data = [
		// Col 1 (band rows 1-4): *, *, A(1,1), A(2,1)
		0,
		0,
		0,
		0,
		1,
		0.5,
		2,
		-1,

		// Col 2 (band rows 1-4): *, A(1,2), A(2,2), A(3,2)
		0,
		0,
		3,
		1,
		4,
		0,
		5,
		2,

		// Col 3 (band rows 1-4): A(1,3), A(2,3), A(3,3), A(4,3)
		6,
		-0.5,
		7,
		1.5,
		8,
		-1,
		9,
		0,

		// Col 4 (band rows 1-4): *, A(3,4), A(4,4), padding
		0,
		0,
		10,
		1,
		11,
		-2,
		12,
		0.5
	];
	return new Complex128Array( data );
}


// TESTS //

test( 'zgbmv: main export is a function', function t() {
	assert.strictEqual( typeof zgbmv, 'function' );
});

test( 'zgbmv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgbmv.ndarray, 'function' );
});

test( 'zgbmv: no-transpose basic (M=4, N=4, KL=1, KU=2, alpha=(1,0), beta=(0,0))', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'no_trans_basic' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( 4 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'no_trans_basic y' );
});

test( 'zgbmv: transpose basic (M=4, N=4, KL=1, KU=2)', function t() {
	var result = base( 'transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'trans_basic' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( 4 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'trans_basic y' );
});

test( 'zgbmv: conjugate-transpose basic (M=4, N=4, KL=1, KU=2)', function t() {
	var result = base( 'conjugate-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'conj_trans_basic' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( 4 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'conj_trans_basic y' );
});

test( 'zgbmv: complex alpha and beta (alpha=(2,1), beta=(0.5,-0.5))', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 0.5, -0.5 );
	var tc = findCase( 'complex_alpha_beta' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5, 3, 0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'complex_alpha_beta y' );
});

test( 'zgbmv: alpha=(0,0), beta=(2,0) — only scale y', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 2, 0 );
	var tc = findCase( 'alpha_zero' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'alpha_zero y' );
});

test( 'zgbmv: M=0 quick return', function t() {
	var result = base( 'no-transpose', 0, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'm_zero' );
	var yv = reinterpret( y, 0 );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 99, 0, 77, 66 ] );
	assert.strictEqual( yv[ 0 ], 99 );
	assert.strictEqual( yv[ 1 ], 0 );
});

test( 'zgbmv: N=0, beta=(0,0) — zero y since leny=M=4 but no matrix', function t() {
	var result = base( 'no-transpose', 4, 0, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var yv = reinterpret( y, 0 );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 99, 0, 77, 66, 55, 44, 33, 22 ] );
	assert.strictEqual( yv[ 0 ], 99 );
	assert.strictEqual( yv[ 1 ], 0 );
});

test( 'zgbmv: alpha=(0,0), beta=(0,0) — zero out y', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 99, 88, 77, 66, 55, 44, 33, 22 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'alpha_zero_beta_zero y' );
});

test( 'zgbmv: non-unit incx=2 (strideX=2)', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 2, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'incx_2' );
	var A = createBandMatrix44();
	var x = new Complex128Array([
		1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0
	]);
	var y = new Complex128Array( 4 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'incx_2 y' );
});

test( 'zgbmv: non-unit incy=2 (strideY=2)', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 2, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'incy_2' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( 8 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'incy_2 y' );
});

test( 'zgbmv: rectangular M<N (3x5 matrix, KL=1, KU=1, LDA=3)', function t() {
	var result = base( 'no-transpose', 3, 5, 1, 1, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'rect_m_lt_n' );
	var A = new Complex128Array([
		// Col 1: *, A(1,1)=(1,1), A(2,1)=(2,0)
		0,
		0,
		1,
		1,
		2,
		0,

		// Col 2: A(1,2)=(3,-1), A(2,2)=(4,0.5), A(3,2)=(5,1)
		3,
		-1,
		4,
		0.5,
		5,
		1,

		// Col 3: A(2,3)=(6,0), A(3,3)=(7,-0.5), *
		6,
		0,
		7,
		-0.5,
		0,
		0,

		// Col 4: A(3,4)=(8,1), *, *
		8,
		1,
		0,
		0,
		0,
		0,

		// Col 5: *, *, *
		0,
		0,
		0,
		0,
		0,
		0
	]);
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 0.5, 0.5, 1, -0.5 ] );
	var y = new Complex128Array( 3 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'rect_m_lt_n y' );
});

test( 'zgbmv: beta=(1,0) — add to existing y', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 1, 0 );
	var tc = findCase( 'beta_one' );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 10, 5, 20, -10, 30, 15, 40, -20 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'beta_one y' );
});

test( 'zgbmv: alpha=(0,0), beta=(1,0) quick return (y unchanged)', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 0 );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8, 9, 10, 11, 12 ] );
	assert.strictEqual( result, y );
	assert.deepStrictEqual( Array.from( reinterpret( y, 0 ) ), [ 5, 6, 7, 8, 9, 10, 11, 12 ] );
});

test( 'zgbmv: conjugate-transpose with non-unit strides', function t() {
	var result = base( 'conjugate-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 2, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'conj_trans_basic' );
	var A = createBandMatrix44();
	var x = new Complex128Array([
		1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0
	]);
	var y = new Complex128Array( 4 );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'conj_trans_stride y' );
});

test( 'zgbmv: transpose with non-unit strides', function t() {
	var result = base( 'transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 2, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var tc = findCase( 'trans_basic' );
	var A = createBandMatrix44();
	var x = new Complex128Array([
		1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0
	]);
	var y = new Complex128Array( 4 );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'trans_stride y' );
});

test( 'zgbmv: complex beta scaling (alpha=0, beta=(1,1))', function t() {
	var result = base( 'no-transpose', 4, 4, 1, 2, alpha, A, 1, 4, 0, x, 1, 0, beta, y, 1, 0 );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 1 );
	var yv = reinterpret( y, 0 );
	var A = createBandMatrix44();
	var x = new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 0, 2 ] );

	// y[0] = (1+1i)*(1+0i) = (1, 1)
	assertClose( yv[ 0 ], 1, 'beta_complex y[0] re' );
	assertClose( yv[ 1 ], 1, 'beta_complex y[0] im' );

	// y[1] = (1+1i)*(0+1i) = (0*1 - 1*1) + (0*1 + 1*1)i = (-1, 1) -- wait:

	// beta * y[1] = (1+i)*(0+i) = 0 + i + 0 + i^2 = -1 + i
	assertClose( yv[ 2 ], -1, 'beta_complex y[1] re' );
	assertClose( yv[ 3 ], 1, 'beta_complex y[1] im' );

	// y[2] = (1+i)*(2+0i) = (2, 2)
	assertClose( yv[ 4 ], 2, 'beta_complex y[2] re' );
	assertClose( yv[ 5 ], 2, 'beta_complex y[2] im' );

	// y[3] = (1+i)*(0+2i) = (-2, 2)
	assertClose( yv[ 6 ], -2, 'beta_complex y[3] re' );
	assertClose( yv[ 7 ], 2, 'beta_complex y[3] im' );
});
