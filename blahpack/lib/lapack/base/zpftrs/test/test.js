/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

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
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpftrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zpftrs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


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
* Runs a zpftrs fixture test.
*
* @private
* @param {string} name - fixture case name
* @param {string} transr - 'no-transpose' or 'conjugate-transpose'
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - order of matrix
* @param {NonNegativeInteger} nrhs - number of right-hand sides
*/
function runTest( name, transr, uplo, N, nrhs ) {
	var info;
	var tc;
	var Bv;
	var A;
	var B;

	tc = findCase( name );
	A = new Complex128Array( tc.a );
	B = new Complex128Array( tc.b_in );
	info = zpftrs( transr, uplo, N, nrhs, A, 1, 0, B, 1, N, 0 );
	Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Bv, tc.b_out, 1e-14, 'b_out' );
}


// TESTS //

test( 'zpftrs: n3_nrhs1_lower_normal (N=3, NRHS=1, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	runTest( 'n3_nrhs1_lower_normal', 'no-transpose', 'lower', 3, 1 );
});

test( 'zpftrs: n3_nrhs1_upper_normal (N=3, NRHS=1, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	runTest( 'n3_nrhs1_upper_normal', 'no-transpose', 'upper', 3, 1 );
});

test( 'zpftrs: n3_nrhs1_lower_conjtrans (N=3, NRHS=1, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	runTest( 'n3_nrhs1_lower_conjtrans', 'conjugate-transpose', 'lower', 3, 1 );
});

test( 'zpftrs: n3_nrhs1_upper_conjtrans (N=3, NRHS=1, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	runTest( 'n3_nrhs1_upper_conjtrans', 'conjugate-transpose', 'upper', 3, 1 );
});

test( 'zpftrs: n4_nrhs1_lower_normal (N=4, NRHS=1, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	runTest( 'n4_nrhs1_lower_normal', 'no-transpose', 'lower', 4, 1 );
});

test( 'zpftrs: n4_nrhs1_upper_normal (N=4, NRHS=1, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	runTest( 'n4_nrhs1_upper_normal', 'no-transpose', 'upper', 4, 1 );
});

test( 'zpftrs: n4_nrhs1_lower_conjtrans (N=4, NRHS=1, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	runTest( 'n4_nrhs1_lower_conjtrans', 'conjugate-transpose', 'lower', 4, 1 );
});

test( 'zpftrs: n4_nrhs1_upper_conjtrans (N=4, NRHS=1, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	runTest( 'n4_nrhs1_upper_conjtrans', 'conjugate-transpose', 'upper', 4, 1 );
});

test( 'zpftrs: n3_nrhs2_lower_normal (N=3, NRHS=2, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	runTest( 'n3_nrhs2_lower_normal', 'no-transpose', 'lower', 3, 2 );
});

test( 'zpftrs: n3_nrhs2_upper_conjtrans (N=3, NRHS=2, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	runTest( 'n3_nrhs2_upper_conjtrans', 'conjugate-transpose', 'upper', 3, 2 );
});

test( 'zpftrs: n4_nrhs2_lower_normal (N=4, NRHS=2, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	runTest( 'n4_nrhs2_lower_normal', 'no-transpose', 'lower', 4, 2 );
});

test( 'zpftrs: n4_nrhs2_upper_conjtrans (N=4, NRHS=2, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	runTest( 'n4_nrhs2_upper_conjtrans', 'conjugate-transpose', 'upper', 4, 2 );
});

test( 'zpftrs: n_zero (N=0 returns immediately)', function t() {
	var info;
	var B;

	B = new Complex128Array( 0 );
	info = zpftrs( 'no-transpose', 'lower', 0, 1, new Complex128Array( 0 ), 1, 0, B, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'zpftrs: n_one (N=1, NRHS=1)', function t() {
	var info;
	var tc;
	var Bv;
	var A;
	var B;

	tc = findCase( 'n_one' );
	A = new Complex128Array( tc.a );
	B = new Complex128Array( tc.b_in );
	info = zpftrs( 'no-transpose', 'lower', 1, 1, A, 1, 0, B, 1, 1, 0 );
	Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Bv, tc.b_out, 1e-14, 'b_out' );
});
