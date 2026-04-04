/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

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
var ztfsm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'ztfsm.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// VARIABLES //

var CONE = new Complex128( 1.0, 0.0 );
var TOL = 1e-13;


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Runs a generic ztfsm fixture test case.
*
* @private
* @param {string} name - fixture name
* @param {string} transr - TRANSR parameter
* @param {string} side - SIDE parameter
* @param {string} uplo - UPLO parameter
* @param {string} trans - TRANS parameter
* @param {string} diag - DIAG parameter
* @param {NonNegativeInteger} M - rows of B
* @param {NonNegativeInteger} N - cols of B
* @param {Complex128} alpha - scalar
*/
function runCase( name, transr, side, uplo, trans, diag, M, N, alpha ) {
	var tc;
	var Bv;
	var A;
	var B;

	tc = findCase( name );
	A = new Complex128Array( tc.a );
	B = new Complex128Array( tc.b_in );

	ztfsm( transr, side, uplo, trans, diag, M, N, alpha, A, 1, 0, B, 1, M, 0 );
	Bv = reinterpret( B, 0 );
	assertArrayClose( Bv, tc.b_out, TOL, name );
}


// TESTS //

// --- SIDE = Left, M odd (3), all TRANSR/UPLO/TRANS combos ---

test( 'ztfsm: left, M odd, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'left_modd_NLN', 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=N, UPLO=L, TRANS=C', function t() {
	runCase( 'left_modd_NLC', 'no-transpose', 'left', 'lower', 'conjugate-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'left_modd_NUN', 'no-transpose', 'left', 'upper', 'no-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=N, UPLO=U, TRANS=C', function t() {
	runCase( 'left_modd_NUC', 'no-transpose', 'left', 'upper', 'conjugate-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=C, UPLO=L, TRANS=N', function t() {
	runCase( 'left_modd_CLN', 'conjugate-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=C, UPLO=L, TRANS=C', function t() {
	runCase( 'left_modd_CLC', 'conjugate-transpose', 'left', 'lower', 'conjugate-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=C, UPLO=U, TRANS=N', function t() {
	runCase( 'left_modd_CUN', 'conjugate-transpose', 'left', 'upper', 'no-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M odd, TRANSR=C, UPLO=U, TRANS=C', function t() {
	runCase( 'left_modd_CUC', 'conjugate-transpose', 'left', 'upper', 'conjugate-transpose', 'non-unit', 3, 2, CONE ); // eslint-disable-line max-len
});

// --- SIDE = Left, M even (4), all TRANSR/UPLO/TRANS combos ---

test( 'ztfsm: left, M even, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'left_meven_NLN', 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=N, UPLO=L, TRANS=C', function t() {
	runCase( 'left_meven_NLC', 'no-transpose', 'left', 'lower', 'conjugate-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'left_meven_NUN', 'no-transpose', 'left', 'upper', 'no-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=N, UPLO=U, TRANS=C', function t() {
	runCase( 'left_meven_NUC', 'no-transpose', 'left', 'upper', 'conjugate-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=C, UPLO=L, TRANS=N', function t() {
	runCase( 'left_meven_CLN', 'conjugate-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=C, UPLO=L, TRANS=C', function t() {
	runCase( 'left_meven_CLC', 'conjugate-transpose', 'left', 'lower', 'conjugate-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=C, UPLO=U, TRANS=N', function t() {
	runCase( 'left_meven_CUN', 'conjugate-transpose', 'left', 'upper', 'no-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, TRANSR=C, UPLO=U, TRANS=C', function t() {
	runCase( 'left_meven_CUC', 'conjugate-transpose', 'left', 'upper', 'conjugate-transpose', 'non-unit', 4, 2, CONE ); // eslint-disable-line max-len
});

// --- SIDE = Right, N odd (3), all TRANSR/UPLO/TRANS combos ---

test( 'ztfsm: right, N odd, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'right_nodd_NLN', 'no-transpose', 'right', 'lower', 'no-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=N, UPLO=L, TRANS=C', function t() {
	runCase( 'right_nodd_NLC', 'no-transpose', 'right', 'lower', 'conjugate-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'right_nodd_NUN', 'no-transpose', 'right', 'upper', 'no-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=N, UPLO=U, TRANS=C', function t() {
	runCase( 'right_nodd_NUC', 'no-transpose', 'right', 'upper', 'conjugate-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=C, UPLO=L, TRANS=N', function t() {
	runCase( 'right_nodd_CLN', 'conjugate-transpose', 'right', 'lower', 'no-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=C, UPLO=L, TRANS=C', function t() {
	runCase( 'right_nodd_CLC', 'conjugate-transpose', 'right', 'lower', 'conjugate-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=C, UPLO=U, TRANS=N', function t() {
	runCase( 'right_nodd_CUN', 'conjugate-transpose', 'right', 'upper', 'no-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, TRANSR=C, UPLO=U, TRANS=C', function t() {
	runCase( 'right_nodd_CUC', 'conjugate-transpose', 'right', 'upper', 'conjugate-transpose', 'non-unit', 2, 3, CONE ); // eslint-disable-line max-len
});

// --- SIDE = Right, N even (4), all TRANSR/UPLO/TRANS combos ---

test( 'ztfsm: right, N even, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'right_neven_NLN', 'no-transpose', 'right', 'lower', 'no-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=N, UPLO=L, TRANS=C', function t() {
	runCase( 'right_neven_NLC', 'no-transpose', 'right', 'lower', 'conjugate-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'right_neven_NUN', 'no-transpose', 'right', 'upper', 'no-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=N, UPLO=U, TRANS=C', function t() {
	runCase( 'right_neven_NUC', 'no-transpose', 'right', 'upper', 'conjugate-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=C, UPLO=L, TRANS=N', function t() {
	runCase( 'right_neven_CLN', 'conjugate-transpose', 'right', 'lower', 'no-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=C, UPLO=L, TRANS=C', function t() {
	runCase( 'right_neven_CLC', 'conjugate-transpose', 'right', 'lower', 'conjugate-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=C, UPLO=U, TRANS=N', function t() {
	runCase( 'right_neven_CUN', 'conjugate-transpose', 'right', 'upper', 'no-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, TRANSR=C, UPLO=U, TRANS=C', function t() {
	runCase( 'right_neven_CUC', 'conjugate-transpose', 'right', 'upper', 'conjugate-transpose', 'non-unit', 2, 4, CONE ); // eslint-disable-line max-len
});

// --- Unit diagonal cases ---

test( 'ztfsm: left, M odd, unit diagonal', function t() {
	runCase( 'left_modd_NLN_unit', 'no-transpose', 'left', 'lower', 'no-transpose', 'unit', 3, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: left, M even, unit diagonal', function t() {
	runCase( 'left_meven_NLN_unit', 'no-transpose', 'left', 'lower', 'no-transpose', 'unit', 4, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N odd, unit diagonal', function t() {
	runCase( 'right_nodd_NLN_unit', 'no-transpose', 'right', 'lower', 'no-transpose', 'unit', 2, 3, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: right, N even, unit diagonal', function t() {
	runCase( 'right_neven_NLN_unit', 'no-transpose', 'right', 'lower', 'no-transpose', 'unit', 2, 4, CONE ); // eslint-disable-line max-len
});

// --- M=1 special cases ---

test( 'ztfsm: M=1, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'm1_NLN', 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 1, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: M=1, TRANSR=N, UPLO=L, TRANS=C', function t() {
	runCase( 'm1_NLC', 'no-transpose', 'left', 'lower', 'conjugate-transpose', 'non-unit', 1, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: M=1, TRANSR=C, UPLO=L, TRANS=N', function t() {
	runCase( 'm1_CLN', 'conjugate-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 1, 2, CONE ); // eslint-disable-line max-len
});

test( 'ztfsm: M=1, TRANSR=C, UPLO=L, TRANS=C', function t() {
	runCase( 'm1_CLC', 'conjugate-transpose', 'left', 'lower', 'conjugate-transpose', 'non-unit', 1, 2, CONE ); // eslint-disable-line max-len
});

// --- Alpha = 0 case ---

test( 'ztfsm: alpha = 0 sets B to zero', function t() {
	var alpha;
	var tc;
	var Bv;
	var B;

	alpha = new Complex128( 0.0, 0.0 );
	tc = findCase( 'alpha_zero' );
	B = new Complex128Array( tc.b_in );
	ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 2, alpha, new Complex128Array( 6 ), 1, 0, B, 1, 3, 0 ); // eslint-disable-line max-len
	Bv = reinterpret( B, 0 );
	assertArrayClose( Bv, tc.b_out, TOL, 'alpha_zero' );
});

// --- Empty cases: M=0 or N=0 ---

test( 'ztfsm: M=0 returns immediately', function t() {
	var Bv;
	var B;

	B = new Complex128Array( [ 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99 ] ); // eslint-disable-line max-len
	ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 0, 2, CONE, new Complex128Array( 1 ), 1, 0, B, 1, 3, 0 ); // eslint-disable-line max-len
	Bv = reinterpret( B, 0 );
	assert.equal( Bv[ 0 ], 99 );
	assert.equal( Bv[ 1 ], 99 );
});

test( 'ztfsm: N=0 returns immediately', function t() {
	var Bv;
	var B;

	B = new Complex128Array( [ 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99 ] ); // eslint-disable-line max-len
	ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 0, CONE, new Complex128Array( 1 ), 1, 0, B, 1, 3, 0 ); // eslint-disable-line max-len
	Bv = reinterpret( B, 0 );
	assert.equal( Bv[ 0 ], 99 );
	assert.equal( Bv[ 1 ], 99 );
});

// --- Non-trivial alpha ---

test( 'ztfsm: left, non-trivial alpha', function t() {
	var alpha;
	var tc;
	var Bv;
	var A;
	var B;

	alpha = new Complex128( 2.0, -1.0 );
	tc = findCase( 'left_alpha' );
	A = new Complex128Array( tc.a );
	B = new Complex128Array( tc.b_in );
	ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 2, alpha, A, 1, 0, B, 1, 3, 0 ); // eslint-disable-line max-len
	Bv = reinterpret( B, 0 );
	assertArrayClose( Bv, tc.b_out, TOL, 'left_alpha' );
});

test( 'ztfsm: right, non-trivial alpha', function t() {
	var alpha;
	var tc;
	var Bv;
	var A;
	var B;

	alpha = new Complex128( 0.5, 1.0 );
	tc = findCase( 'right_alpha' );
	A = new Complex128Array( tc.a );
	B = new Complex128Array( tc.b_in );
	ztfsm( 'no-transpose', 'right', 'lower', 'no-transpose', 'non-unit', 2, 3, alpha, A, 1, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	Bv = reinterpret( B, 0 );
	assertArrayClose( Bv, tc.b_out, TOL, 'right_alpha' );
});
