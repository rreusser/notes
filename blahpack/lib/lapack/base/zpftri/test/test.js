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
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpftrf = require( '../../zpftrf/lib/base.js' );
var zpftri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zpftri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Creates a Complex128Array from test case input data.
*
* @private
* @param {*} tc - test case
* @returns {Complex128Array} input array
*/
function makeInput( tc ) {
	return new Complex128Array( tc.input );
}

/**
* Helper: runs zpftrf then zpftri on the fixture input.
*
* @private
* @param {string} transr - transpose operation
* @param {string} uplo - matrix triangle
* @param {NonNegativeInteger} N - matrix order
* @param {Complex128Array} A - input array
* @returns {integer} info from zpftri
*/
function factorAndInvert( transr, uplo, N, A ) {
	var info;

	info = zpftrf( transr, uplo, N, A, 1, 0 );
	assert.equal( info, 0, 'zpftrf should succeed' );
	return zpftri( transr, uplo, N, A, 1, 0 );
}


// TESTS //

test( 'zpftri: lower_odd_normal (N=3, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_odd_normal' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'lower', 3, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: upper_odd_normal (N=3, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_odd_normal' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'upper', 3, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: lower_odd_conjtrans (N=3, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_odd_conjtrans' );
	A = makeInput( tc );
	info = factorAndInvert( 'conjugate-transpose', 'lower', 3, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: upper_odd_conjtrans (N=3, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_odd_conjtrans' );
	A = makeInput( tc );
	info = factorAndInvert( 'conjugate-transpose', 'upper', 3, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: lower_even_normal (N=4, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_even_normal' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'lower', 4, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: upper_even_normal (N=4, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_even_normal' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'upper', 4, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: lower_even_conjtrans (N=4, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_even_conjtrans' );
	A = makeInput( tc );
	info = factorAndInvert( 'conjugate-transpose', 'lower', 4, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: upper_even_conjtrans (N=4, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_even_conjtrans' );
	A = makeInput( tc );
	info = factorAndInvert( 'conjugate-transpose', 'upper', 4, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: n_zero', function t() {
	var info;
	var A;

	A = new Complex128Array( 0 );
	info = zpftri( 'no-transpose', 'lower', 0, A, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zpftri: n_one', function t() {
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'n_one' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'lower', 1, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftri: lower_5_normal (N=5, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_5_normal' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'lower', 5, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-13, 'a' );
});

test( 'zpftri: upper_5_conjtrans (N=5, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_5_conjtrans' );
	A = makeInput( tc );
	info = factorAndInvert( 'conjugate-transpose', 'upper', 5, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-13, 'a' );
});

test( 'zpftri: upper_5_normal (N=5, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_5_normal' );
	A = makeInput( tc );
	info = factorAndInvert( 'no-transpose', 'upper', 5, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-13, 'a' );
});

test( 'zpftri: lower_5_conjtrans (N=5, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_5_conjtrans' );
	A = makeInput( tc );
	info = factorAndInvert( 'conjugate-transpose', 'lower', 5, A );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-13, 'a' );
});
