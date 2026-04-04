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

/* eslint-disable max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpftrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpftrs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Runs a solve test case.
*
* @private
* @param {string} name - fixture name
* @param {string} transr - 'no-transpose' or 'transpose'
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - matrix order
* @param {NonNegativeInteger} nrhs - number of right-hand sides
*/
function runSolveTest( name, transr, uplo, N, nrhs ) {
	var tc = findCase( name );
	var A = new Float64Array( tc.a );
	var B = new Float64Array( tc.b_in );
	var info;

	// B is column-major N-by-NRHS with LDB=N => strideB1=1, strideB2=N
	info = dpftrs( transr, uplo, N, nrhs, A, 1, 0, B, 1, N, 0 );
	assert.equal( info, 0, name + ': info' );
	assertArrayClose( Array.from( B ), tc.b_out, 1e-13, name + ': B' );
}


// TESTS //

test( 'dpftrs: lower_odd_normal_1rhs (N=3, TRANSR=N, UPLO=L, NRHS=1)', function t() {
	runSolveTest( 'lower_odd_normal_1rhs', 'no-transpose', 'lower', 3, 1 );
});

test( 'dpftrs: upper_odd_normal_1rhs (N=3, TRANSR=N, UPLO=U, NRHS=1)', function t() {
	runSolveTest( 'upper_odd_normal_1rhs', 'no-transpose', 'upper', 3, 1 );
});

test( 'dpftrs: lower_odd_trans_1rhs (N=3, TRANSR=T, UPLO=L, NRHS=1)', function t() {
	runSolveTest( 'lower_odd_trans_1rhs', 'transpose', 'lower', 3, 1 );
});

test( 'dpftrs: upper_odd_trans_1rhs (N=3, TRANSR=T, UPLO=U, NRHS=1)', function t() {
	runSolveTest( 'upper_odd_trans_1rhs', 'transpose', 'upper', 3, 1 );
});

test( 'dpftrs: lower_even_normal_1rhs (N=4, TRANSR=N, UPLO=L, NRHS=1)', function t() {
	runSolveTest( 'lower_even_normal_1rhs', 'no-transpose', 'lower', 4, 1 );
});

test( 'dpftrs: upper_even_normal_1rhs (N=4, TRANSR=N, UPLO=U, NRHS=1)', function t() {
	runSolveTest( 'upper_even_normal_1rhs', 'no-transpose', 'upper', 4, 1 );
});

test( 'dpftrs: lower_even_trans_1rhs (N=4, TRANSR=T, UPLO=L, NRHS=1)', function t() {
	runSolveTest( 'lower_even_trans_1rhs', 'transpose', 'lower', 4, 1 );
});

test( 'dpftrs: upper_even_trans_1rhs (N=4, TRANSR=T, UPLO=U, NRHS=1)', function t() {
	runSolveTest( 'upper_even_trans_1rhs', 'transpose', 'upper', 4, 1 );
});

test( 'dpftrs: lower_odd_normal_2rhs (N=3, TRANSR=N, UPLO=L, NRHS=2)', function t() {
	runSolveTest( 'lower_odd_normal_2rhs', 'no-transpose', 'lower', 3, 2 );
});

test( 'dpftrs: upper_even_trans_3rhs (N=4, TRANSR=T, UPLO=U, NRHS=3)', function t() {
	runSolveTest( 'upper_even_trans_3rhs', 'transpose', 'upper', 4, 3 );
});

test( 'dpftrs: lower_5_normal_1rhs (N=5, TRANSR=N, UPLO=L, NRHS=1)', function t() {
	runSolveTest( 'lower_5_normal_1rhs', 'no-transpose', 'lower', 5, 1 );
});

test( 'dpftrs: upper_5_trans_2rhs (N=5, TRANSR=T, UPLO=U, NRHS=2)', function t() {
	runSolveTest( 'upper_5_trans_2rhs', 'transpose', 'upper', 5, 2 );
});

test( 'dpftrs: N=0 returns 0 immediately', function t() {
	var A = new Float64Array( [ 1.0 ] );
	var B = new Float64Array( [ 1.0 ] );
	var info = dpftrs( 'no-transpose', 'lower', 0, 1, A, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
	assert.equal( B[ 0 ], 1.0, 'B unchanged' );
});

test( 'dpftrs: NRHS=0 returns 0 immediately', function t() {
	var A = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var info = dpftrs( 'no-transpose', 'lower', 3, 0, A, 1, 0, B, 1, 3, 0 );
	assert.equal( info, 0 );
	assert.equal( B[ 0 ], 1.0, 'B unchanged' );
});

test( 'dpftrs: N=1 case', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array( tc.a );
	var B = new Float64Array( tc.b_in );
	var info = dpftrs( 'no-transpose', 'lower', 1, 1, A, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
	assertArrayClose( Array.from( B ), tc.b_out, 1e-14, 'N=1 B' );
});
