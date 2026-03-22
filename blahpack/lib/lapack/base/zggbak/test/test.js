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

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zggbak = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zggbak.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertArrayClose( actual, expected, msg ) {
	var relErr;
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		if ( expected[ i ] === 0.0 ) {
			assert.ok( Math.abs( actual[ i ] ) <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		} else {
			relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
			assert.ok( relErr <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

/**
* Set complex element (i, j) in interleaved matrix (Float64 view).
*/
function cset( M, LDV, i, j, re, im ) {
	var idx = j * 2 * LDV + i * 2;
	M[ idx ] = re;
	M[ idx + 1 ] = im;
}

/**
* Extract the complex matrix as a flat interleaved array (column-by-column,
* matching Fortran fixture output format from print_cmatrix).
*
* @param {Float64Array} V - Float64 view of matrix
* @param {integer} LDV - leading dimension (allocated rows)
* @param {integer} n - number of rows to extract per column
* @param {integer} m - number of columns
* @returns {Array} flat array [col0_row0_re, col0_row0_im, col0_row1_re, ...]
*/
function extractCMatrix( V, LDV, n, m ) {
	var result = [];
	var i;
	var j;
	for ( j = 0; j < m; j++ ) {
		for ( i = 0; i < n; i++ ) {
			result.push( V[ j * 2 * LDV + i * 2 ] );
			result.push( V[ j * 2 * LDV + i * 2 + 1 ] );
		}
	}
	return result;
}

// TESTS //

test( 'zggbak: main export is a function', function t() {
	assert.strictEqual( typeof zggbak, 'function' );
});

test( 'zggbak: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zggbak.ndarray, 'function' );
});

test( 'zggbak: JOB=N quick return (no transformation)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'job_n'; } );
	var n = 3;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( [ 2.0, 3.0, 4.0 ] );
	var rscale = new Float64Array( [ 5.0, 6.0, 7.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 2.0 );
	cset( Vv, LDV, 1, 0, 3.0, 4.0 );
	cset( Vv, LDV, 2, 0, 5.0, 6.0 );
	cset( Vv, LDV, 0, 1, 7.0, 8.0 );
	cset( Vv, LDV, 1, 1, 9.0, 10.0 );
	cset( Vv, LDV, 2, 1, 11.0, 12.0 );

	info = base( 'N', 'R', n, 1, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=S, SIDE=R (scale right eigenvectors by RSCALE)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'scale_right'; } );
	var n = 3;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 3 );
	var rscale = new Float64Array( [ 2.0, 3.0, 0.5 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 2.0 );
	cset( Vv, LDV, 1, 0, 3.0, 4.0 );
	cset( Vv, LDV, 2, 0, 5.0, 6.0 );
	cset( Vv, LDV, 0, 1, 7.0, 8.0 );
	cset( Vv, LDV, 1, 1, 9.0, 10.0 );
	cset( Vv, LDV, 2, 1, 11.0, 12.0 );

	info = base( 'S', 'R', n, 1, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=S, SIDE=L (scale left eigenvectors by LSCALE)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'scale_left'; } );
	var n = 3;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( [ 2.0, 0.5, 3.0 ] );
	var rscale = new Float64Array( 3 );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 2.0 );
	cset( Vv, LDV, 1, 0, 3.0, 4.0 );
	cset( Vv, LDV, 2, 0, 5.0, 6.0 );
	cset( Vv, LDV, 0, 1, 7.0, 8.0 );
	cset( Vv, LDV, 1, 1, 9.0, 10.0 );
	cset( Vv, LDV, 2, 1, 11.0, 12.0 );

	info = base( 'S', 'L', n, 1, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=P, SIDE=R (permute right eigenvectors)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'permute_right'; } );
	var n = 4;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 4 );
	var rscale = new Float64Array( [ 3.0, 0.0, 0.0, 2.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 3, 0, 4.0, 0.0 );
	cset( Vv, LDV, 0, 1, 5.0, 0.0 );
	cset( Vv, LDV, 1, 1, 6.0, 0.0 );
	cset( Vv, LDV, 2, 1, 7.0, 0.0 );
	cset( Vv, LDV, 3, 1, 8.0, 0.0 );

	info = base( 'P', 'R', n, 2, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=P, SIDE=L (permute left eigenvectors)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'permute_left'; } );
	var n = 4;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( [ 4.0, 0.0, 0.0, 1.0 ] );
	var rscale = new Float64Array( 4 );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 3, 0, 4.0, 0.0 );
	cset( Vv, LDV, 0, 1, 5.0, 0.0 );
	cset( Vv, LDV, 1, 1, 6.0, 0.0 );
	cset( Vv, LDV, 2, 1, 7.0, 0.0 );
	cset( Vv, LDV, 3, 1, 8.0, 0.0 );

	info = base( 'P', 'L', n, 2, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=B, SIDE=R (both scale and permute, right)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'both_right'; } );
	var n = 4;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 4 );
	var rscale = new Float64Array( [ 3.0, 2.0, 0.5, 2.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 1.0 );
	cset( Vv, LDV, 1, 0, 2.0, 2.0 );
	cset( Vv, LDV, 2, 0, 3.0, 3.0 );
	cset( Vv, LDV, 3, 0, 4.0, 4.0 );
	cset( Vv, LDV, 0, 1, 5.0, 5.0 );
	cset( Vv, LDV, 1, 1, 6.0, 6.0 );
	cset( Vv, LDV, 2, 1, 7.0, 7.0 );
	cset( Vv, LDV, 3, 1, 8.0, 8.0 );

	info = base( 'B', 'R', n, 2, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=B, SIDE=L (both scale and permute, left)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'both_left'; } );
	var n = 4;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( [ 4.0, 3.0, 0.25, 1.0 ] );
	var rscale = new Float64Array( 4 );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 1.0 );
	cset( Vv, LDV, 1, 0, 2.0, 2.0 );
	cset( Vv, LDV, 2, 0, 3.0, 3.0 );
	cset( Vv, LDV, 3, 0, 4.0, 4.0 );
	cset( Vv, LDV, 0, 1, 5.0, 5.0 );
	cset( Vv, LDV, 1, 1, 6.0, 6.0 );
	cset( Vv, LDV, 2, 1, 7.0, 7.0 );
	cset( Vv, LDV, 3, 1, 8.0, 8.0 );

	info = base( 'B', 'L', n, 2, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: N=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'n_zero'; } );
	var V = new Complex128Array( 2 );
	var lscale = new Float64Array( 1 );
	var rscale = new Float64Array( 1 );
	var info;

	info = base( 'B', 'R', 0, 1, 0, lscale, 1, 0, rscale, 1, 0, 2, V, 1, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
});

test( 'zggbak: M=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'm_zero'; } );
	var V = new Complex128Array( 2 );
	var lscale = new Float64Array( 3 );
	var rscale = new Float64Array( 3 );
	var info;

	info = base( 'B', 'R', 3, 1, 3, lscale, 1, 0, rscale, 1, 0, 0, V, 1, 3, 0 );

	assert.strictEqual( info, tc.info, 'info' );
});

test( 'zggbak: ILO=IHI with valid permutation indices', function t() {
	// This test uses valid RSCALE values (all within [1, N]) to avoid
	// the undefined behavior that occurs when RSCALE contains 0
	// (an invalid 1-based index).
	var n = 4;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 4 );
	var rscale = new Float64Array( [ 3.0, 2.0, 3.0, 1.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 3, 0, 4.0, 0.0 );
	cset( Vv, LDV, 0, 1, 5.0, 0.0 );
	cset( Vv, LDV, 1, 1, 6.0, 0.0 );
	cset( Vv, LDV, 2, 1, 7.0, 0.0 );
	cset( Vv, LDV, 3, 1, 8.0, 0.0 );

	info = base( 'B', 'R', n, 2, 2, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, 0, 'info' );
	// Row 1 (index 1) is untouched, verify it
	assert.strictEqual( Vv[ 2 ], 2.0, 'row 1 col 0 re' );
	assert.strictEqual( Vv[ 3 ], 0.0, 'row 1 col 0 im' );
	assert.strictEqual( Vv[ 10 ], 6.0, 'row 1 col 1 re' );
	assert.strictEqual( Vv[ 11 ], 0.0, 'row 1 col 1 im' );
});

test( 'zggbak: ILO=1 (skip first permutation loop)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilo_one_permute'; } );
	var n = 3;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 3 );
	var rscale = new Float64Array( [ 1.0, 2.0, 1.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 0, 1, 4.0, 0.0 );
	cset( Vv, LDV, 1, 1, 5.0, 0.0 );
	cset( Vv, LDV, 2, 1, 6.0, 0.0 );

	info = base( 'P', 'R', n, 1, 2, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: IHI=N (skip second permutation loop)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ihi_n_permute'; } );
	var n = 3;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 3 );
	var rscale = new Float64Array( [ 3.0, 2.0, 3.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 0, 1, 4.0, 0.0 );
	cset( Vv, LDV, 1, 1, 5.0, 0.0 );
	cset( Vv, LDV, 2, 1, 6.0, 0.0 );

	info = base( 'P', 'R', n, 2, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: K=I (no-swap, continue case)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'k_eq_i'; } );
	var n = 3;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 3 );
	var rscale = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 0, 1, 4.0, 0.0 );
	cset( Vv, LDV, 1, 1, 5.0, 0.0 );
	cset( Vv, LDV, 2, 1, 6.0, 0.0 );

	info = base( 'P', 'R', n, 2, 2, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});

test( 'zggbak: JOB=P, SIDE=L, self-permutation (k===i) in both loops', function t() {
	// Use N=4, ilo=2, ihi=3 so both backward (i from ilo-2 downto 0)
	// and forward (i from ihi to N-1) loops run for left eigenvectors.
	// LSCALE contains 1-based self-referencing indices to trigger k===i continues.
	var n = 4;
	var m = 2;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( [ 1.0, 0.0, 0.0, 4.0 ] ); // l[0]=1 → k=0=i, l[3]=4 → k=3=i
	var rscale = new Float64Array( 4 );
	var info;

	cset( Vv, LDV, 0, 0, 1.0, 0.0 );
	cset( Vv, LDV, 1, 0, 2.0, 0.0 );
	cset( Vv, LDV, 2, 0, 3.0, 0.0 );
	cset( Vv, LDV, 3, 0, 4.0, 0.0 );
	cset( Vv, LDV, 0, 1, 5.0, 0.0 );
	cset( Vv, LDV, 1, 1, 6.0, 0.0 );
	cset( Vv, LDV, 2, 1, 7.0, 0.0 );
	cset( Vv, LDV, 3, 1, 8.0, 0.0 );

	// Save original V to compare
	var origV = Array.from( Vv );

	info = base( 'P', 'L', n, 2, 3, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, 0, 'info' );
	// Since lscale[0]=1 (→ self-permute row 0) and lscale[3]=4 (→ self-permute row 3),
	// V should be unchanged
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), origV, 'v unchanged' );
});

test( 'zggbak: N=1 edge case', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'n_one'; } );
	var n = 1;
	var m = 1;
	var LDV = n;
	var V = new Complex128Array( 1 );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 1 );
	var rscale = new Float64Array( [ 1.0 ] );
	var info;

	Vv[ 0 ] = 5.0;
	Vv[ 1 ] = 3.0;

	info = base( 'B', 'R', n, 1, 1, lscale, 1, 0, rscale, 1, 0, m, V, 1, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( [ Vv[ 0 ], Vv[ 1 ] ], tc.v, 'v' );
});

test( 'zggbak: larger matrix with complex values, JOB=B, SIDE=R', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'larger_both_right'; } );
	var n = 5;
	var m = 3;
	var LDV = n;
	var V = new Complex128Array( LDV * m );
	var Vv = reinterpret( V, 0 );
	var lscale = new Float64Array( 5 );
	var rscale = new Float64Array( [ 4.0, 2.0, 0.5, 3.0, 1.0 ] );
	var info;
	var vals = [
		[ 0, 0, 1, 1 ], [ 1, 0, 2, 2 ], [ 2, 0, 3, 3 ], [ 3, 0, 4, 4 ], [ 4, 0, 5, 5 ],
		[ 0, 1, 6, 6 ], [ 1, 1, 7, 7 ], [ 2, 1, 8, 8 ], [ 3, 1, 9, 9 ], [ 4, 1, 10, 10 ],
		[ 0, 2, 11, 11 ], [ 1, 2, 12, 12 ], [ 2, 2, 13, 13 ], [ 3, 2, 14, 14 ], [ 4, 2, 15, 15 ]
	];
	var v;
	var k;
	for ( k = 0; k < vals.length; k++ ) {
		v = vals[ k ];
		cset( Vv, LDV, v[ 0 ], v[ 1 ], v[ 2 ], v[ 3 ] );
	}

	info = base( 'B', 'R', n, 2, 4, lscale, 1, 0, rscale, 1, 0, m, V, 1, LDV, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCMatrix( Vv, LDV, n, m ), tc.v, 'v' );
});
