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
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpftrf = require( './../lib/ndarray.js' );

// FIXTURES //

var lower_odd_normal = require( './fixtures/lower_odd_normal.json' );
var upper_odd_normal = require( './fixtures/upper_odd_normal.json' );
var lower_odd_conjtrans = require( './fixtures/lower_odd_conjtrans.json' );
var upper_odd_conjtrans = require( './fixtures/upper_odd_conjtrans.json' );
var lower_even_normal = require( './fixtures/lower_even_normal.json' );
var upper_even_normal = require( './fixtures/upper_even_normal.json' );
var lower_even_conjtrans = require( './fixtures/lower_even_conjtrans.json' );
var upper_even_conjtrans = require( './fixtures/upper_even_conjtrans.json' );
var n_one = require( './fixtures/n_one.json' );
var not_posdef = require( './fixtures/not_posdef.json' );
var lower_5_normal = require( './fixtures/lower_5_normal.json' );
var upper_5_conjtrans = require( './fixtures/upper_5_conjtrans.json' );
var notpd_odd_normal_upper = require( './fixtures/notpd_odd_normal_upper.json' );
var notpd_odd_conjtrans_lower = require( './fixtures/notpd_odd_conjtrans_lower.json' );
var notpd_odd_conjtrans_upper = require( './fixtures/notpd_odd_conjtrans_upper.json' );
var notpd_even_normal_lower = require( './fixtures/notpd_even_normal_lower.json' );
var notpd_even_normal_upper = require( './fixtures/notpd_even_normal_upper.json' );
var notpd_even_conjtrans_lower = require( './fixtures/notpd_even_conjtrans_lower.json' );
var notpd_even_conjtrans_upper = require( './fixtures/notpd_even_conjtrans_upper.json' );

// FUNCTIONS //

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
* MakeInput.
*
* @private
* @param {*} tc - tc
* @returns {*} result
*/
function makeInput( tc ) {
	return new Complex128Array( tc.input );
}

// TESTS //

test( 'zpftrf: lower_odd_normal (N=3, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = lower_odd_normal;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'lower', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: upper_odd_normal (N=3, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = upper_odd_normal;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'upper', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: lower_odd_conjtrans (N=3, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = lower_odd_conjtrans;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'lower', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: upper_odd_conjtrans (N=3, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = upper_odd_conjtrans;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'upper', 3, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: lower_even_normal (N=4, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = lower_even_normal;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'lower', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: upper_even_normal (N=4, TRANSR=no-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = upper_even_normal;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'upper', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: lower_even_conjtrans (N=4, TRANSR=conjugate-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = lower_even_conjtrans;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'lower', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: upper_even_conjtrans (N=4, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = upper_even_conjtrans;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'upper', 4, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: n_zero', function t() {
	var info;
	var A;

	A = new Complex128Array( 0 );
	info = zpftrf( 'no-transpose', 'lower', 0, A, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zpftrf: n_one', function t() {
	var info;
	var tc;
	var Av;
	var A;

	tc = n_one;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'lower', 1, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: not_posdef (INFO > 0)', function t() {
	var info;
	var tc;
	var A;

	tc = not_posdef;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'lower', 3, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: lower_5_normal (N=5, TRANSR=no-transpose, UPLO=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = lower_5_normal;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'lower', 5, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: upper_5_conjtrans (N=5, TRANSR=conjugate-transpose, UPLO=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var Av;
	var A;

	tc = upper_5_conjtrans;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'upper', 5, A, 1, 0 );
	Av = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Av, tc.a, 1e-14, 'a' );
});

test( 'zpftrf: notpd_odd_normal_upper', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_odd_normal_upper;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'upper', 3, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: notpd_odd_conjtrans_lower', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_odd_conjtrans_lower;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'lower', 3, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: notpd_odd_conjtrans_upper', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_odd_conjtrans_upper;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'upper', 3, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: notpd_even_normal_lower', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_even_normal_lower;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'lower', 4, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: notpd_even_normal_upper', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_even_normal_upper;
	A = makeInput( tc );
	info = zpftrf( 'no-transpose', 'upper', 4, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: notpd_even_conjtrans_lower', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_even_conjtrans_lower;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'lower', 4, A, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpftrf: notpd_even_conjtrans_upper', function t() {
	var info;
	var tc;
	var A;

	tc = notpd_even_conjtrans_upper;
	A = makeInput( tc );
	info = zpftrf( 'conjugate-transpose', 'upper', 4, A, 1, 0 );
	assert.equal( info, tc.info );
});
