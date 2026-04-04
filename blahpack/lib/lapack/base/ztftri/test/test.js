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
var ztftri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'ztftri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Constructs the Complex128Array from fixture input data.
*
* @private
* @param {*} tc - test case
* @returns {*} result
*/
function makeInput( tc ) {
	return new Complex128Array( tc.input );
}


// TESTS //

test( 'ztftri: lower_odd_normal_nonunit (N=3, TRANSR=N, UPLO=L, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_odd_normal_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'non-unit', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_odd_normal_nonunit (N=3, TRANSR=N, UPLO=U, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_odd_normal_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'upper', 'non-unit', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_odd_conjtrans_nonunit (N=3, TRANSR=C, UPLO=L, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_odd_conjtrans_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'lower', 'non-unit', 3, A, 1, 0 ); // eslint-disable-line max-len
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_odd_conjtrans_nonunit (N=3, TRANSR=C, UPLO=U, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_odd_conjtrans_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'upper', 'non-unit', 3, A, 1, 0 ); // eslint-disable-line max-len
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_even_normal_nonunit (N=4, TRANSR=N, UPLO=L, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_even_normal_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'non-unit', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_even_normal_nonunit (N=4, TRANSR=N, UPLO=U, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_even_normal_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'upper', 'non-unit', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_even_conjtrans_nonunit (N=4, TRANSR=C, UPLO=L, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_even_conjtrans_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'lower', 'non-unit', 4, A, 1, 0 ); // eslint-disable-line max-len
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_even_conjtrans_nonunit (N=4, TRANSR=C, UPLO=U, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_even_conjtrans_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'upper', 'non-unit', 4, A, 1, 0 ); // eslint-disable-line max-len
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_odd_normal_unit (N=3, TRANSR=N, UPLO=L, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_odd_normal_unit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'unit', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_odd_normal_unit (N=3, TRANSR=N, UPLO=U, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_odd_normal_unit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'upper', 'unit', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_even_normal_unit (N=4, TRANSR=N, UPLO=L, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_even_normal_unit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'unit', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_even_normal_unit (N=4, TRANSR=N, UPLO=U, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_even_normal_unit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'upper', 'unit', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_odd_conjtrans_unit (N=3, TRANSR=C, UPLO=L, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_odd_conjtrans_unit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'lower', 'unit', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_odd_conjtrans_unit (N=3, TRANSR=C, UPLO=U, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_odd_conjtrans_unit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'upper', 'unit', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_even_conjtrans_unit (N=4, TRANSR=C, UPLO=L, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_even_conjtrans_unit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'lower', 'unit', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_even_conjtrans_unit (N=4, TRANSR=C, UPLO=U, DIAG=U)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_even_conjtrans_unit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'upper', 'unit', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_5_normal_nonunit (N=5, TRANSR=N, UPLO=L, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_5_normal_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'non-unit', 5, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_5_conjtrans_nonunit (N=5, TRANSR=C, UPLO=U, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_5_conjtrans_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'upper', 'non-unit', 5, A, 1, 0 ); // eslint-disable-line max-len
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: upper_5_normal_nonunit (N=5, TRANSR=N, UPLO=U, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'upper_5_normal_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'upper', 'non-unit', 5, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: lower_5_conjtrans_nonunit (N=5, TRANSR=C, UPLO=L, DIAG=N)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'lower_5_conjtrans_nonunit' );
	A = makeInput( tc );
	info = ztftri( 'conjugate-transpose', 'lower', 'non-unit', 5, A, 1, 0 ); // eslint-disable-line max-len
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: n_zero', function t() {
	var info;
	var A;

	A = new Complex128Array( 0 );
	info = ztftri( 'no-transpose', 'lower', 'non-unit', 0, A, 1, 0 );
	assert.equal( info, 0 );
});

test( 'ztftri: n_one', function t() {
	var info;
	var tc;
	var Av;
	var A;

	tc = findCase( 'n_one' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'non-unit', 1, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-12, 'a' );
});

test( 'ztftri: singular (INFO > 0)', function t() {
	var info;
	var tc;
	var A;

	tc = findCase( 'singular' );
	A = makeInput( tc );
	info = ztftri( 'no-transpose', 'lower', 'non-unit', 3, A, 1, 0 );
	assert.equal( info, tc.info );
});
