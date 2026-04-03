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
var join = require( 'path' ).join;
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( './../../dsptrf/lib/base.js' );
var dsptri = require( './../lib/base.js' );


// FIXTURES //

var FIXTURE_DIR = join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var LINES = readFileSync( join( FIXTURE_DIR, 'dsptri.jsonl' ), 'utf8' ).trim().split( '\n' );
var FIXTURE = LINES.map( parse );

/**
* Parses a JSON line.
*
* @private
* @param {string} line - JSON line
* @returns {Object} parsed object
*/
function parse( line ) {
	return JSON.parse( line );
}


// FUNCTIONS //

/**
* Finds a test case by name.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case
*/
function findCase( name ) {
	var i;
	for ( i = 0; i < FIXTURE.length; i++ ) {
		if ( FIXTURE[ i ].name === name ) {
			return FIXTURE[ i ];
		}
	}
	return null;
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - relative tolerance
* @param {string} msg - error message prefix
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;

	relErr = Math.abs( actual - expected );
	relErr /= Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg +
		': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Float64Array} actual - actual values
* @param {Array} expected - expected values
* @param {number} tol - relative tolerance
* @param {string} msg - error message prefix
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;

	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Factors and inverts a packed symmetric matrix.
*
* @private
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} ap - packed matrix (modified in place)
* @returns {Object} result with info, ap, ipiv
*/
function factorAndInvert( uplo, N, ap ) {
	var ipiv;
	var work;
	var info;

	ipiv = new Int32Array( N );
	work = new Float64Array( N );
	info = dsptrf( uplo, N, ap, 1, 0, ipiv, 1, 0 );
	if ( info !== 0 ) {
		return {
			'info': info,
			'ap': ap,
			'ipiv': ipiv
		};
	}
	info = dsptri( uplo, N, ap, 1, 0, ipiv, 1, 0, work, 1, 0 );
	return {
		'info': info,
		'ap': ap,
		'ipiv': ipiv
	};
}


// TESTS //

test( 'dsptri is a function', function t() {
	assert.equal( typeof dsptri, 'function' );
});

test( 'dsptri: 3x3_upper', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '3x3_upper' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	result = factorAndInvert( 'upper', 3, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 3x3_lower', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '3x3_lower' );
	ap = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	result = factorAndInvert( 'lower', 3, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 4x4_upper', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '4x4_upper' );
	ap = new Float64Array([
		2.0, 1.0, 4.0, 0.0, 2.0, 6.0, 3.0, 1.0, 5.0, 8.0
	]);
	result = factorAndInvert( 'upper', 4, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 4x4_lower', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '4x4_lower' );
	ap = new Float64Array([
		2.0, 1.0, 0.0, 3.0, 4.0, 2.0, 1.0, 6.0, 5.0, 8.0
	]);
	result = factorAndInvert( 'lower', 4, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: n_zero', function t() {
	var ipiv;
	var work;
	var info;
	var tc;
	var ap;

	tc = findCase( 'n_zero' );
	ap = new Float64Array( 0 );
	ipiv = new Int32Array( 0 );
	work = new Float64Array( 0 );
	info = dsptri( 'upper', 0, ap, 1, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dsptri: n_one', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( 'n_one' );
	ap = new Float64Array( [ 5.0 ] );
	result = factorAndInvert( 'upper', 1, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 4x4_indef_2x2_upper', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '4x4_indef_2x2_upper' );
	ap = new Float64Array([
		0.0, 1.0, 0.0, 2.0, 4.0, 0.0, 3.0, 5.0, 6.0, 0.0
	]);
	result = factorAndInvert( 'upper', 4, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 4x4_indef_2x2_lower', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '4x4_indef_2x2_lower' );
	ap = new Float64Array([
		0.0, 1.0, 2.0, 3.0, 0.0, 4.0, 5.0, 0.0, 6.0, 0.0
	]);
	result = factorAndInvert( 'lower', 4, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 3x3_indef_upper', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '3x3_indef_upper' );
	ap = new Float64Array( [ 1.0, 2.0, 0.0, 3.0, 1.0, 1.0 ] );
	result = factorAndInvert( 'upper', 3, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: 3x3_indef_lower', function t() {
	var result;
	var tc;
	var ap;

	tc = findCase( '3x3_indef_lower' );
	ap = new Float64Array( [ 1.0, 2.0, 3.0, 0.0, 1.0, 1.0 ] );
	result = factorAndInvert( 'lower', 3, ap );
	assert.equal( result.info, tc.info );
	assertArrayClose( result.ap, tc.ap, 1e-14, 'ap' );
});

test( 'dsptri: ndarray wrapper validates uplo', function t() {
	var ndarray;
	var ipiv;
	var work;
	var ap;

	ndarray = require( './../lib/ndarray.js' );
	ap = new Float64Array( [ 1.0 ] );
	ipiv = new Int32Array( [ 0 ] );
	work = new Float64Array( [ 0.0 ] );
	assert.throws( function invalid() {
		ndarray( 'invalid', 1, ap, 1, 0, ipiv, 1, 0, work, 1, 0 );
	}, /invalid argument/ );
});
