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

/* eslint-disable max-len, no-restricted-syntax, stdlib/first-unit-test, require-jsdoc */

'use strict';

// MODULES //

var fs = require( 'fs' );
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var trim = require( '@stdlib/string/base/trim' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsprfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = trim( fs.readFileSync( path.join( fixtureDir, 'zsprfs.jsonl' ), 'utf8' ) ).split( '\n' ); // eslint-disable-line node/no-sync, no-restricted-syntax
var fixture = [];
var idx;
for ( idx = 0; idx < lines.length; idx++ ) {
	fixture.push( JSON.parse( lines[ idx ] ) );
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
	for ( i = 0; i < fixture.length; i++ ) {
		if ( fixture[ i ].name === name ) {
			return fixture[ i ];
		}
	}
	return null;
}

/**
* Asserts two numbers are close within relative tolerance.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - error message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts two arrays are element-wise close within relative tolerance.
*
* @private
* @param {(Array|TypedArray)} actual - actual array
* @param {(Array|TypedArray)} expected - expected array
* @param {number} tol - tolerance
* @param {string} msg - error message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Creates a Complex128Array from interleaved real/imag doubles.
*
* @private
* @param {Array} arr - interleaved array
* @returns {Complex128Array} complex array
*/
function toComplex128( arr ) {
	return new Complex128Array( new Float64Array( arr ) );
}

/**
* Runs a zsprfs test from fixture data.
*
* @private
* @param {string} uplo - 'upper' or 'lower'
* @param {Object} tc - test case from fixture
* @param {number} N - order of matrix
* @param {number} nrhs - number of right-hand sides
*/
function runTest( uplo, tc, N, nrhs ) {
	var RWORK;
	var WORK;
	var FERR;
	var BERR;
	var IPIV;
	var info;
	var AFP;
	var AP;
	var Xv;
	var B;
	var X;
	var i;

	AP = toComplex128( tc.AP );
	AFP = toComplex128( tc.AFP );
	IPIV = new Int32Array( N );
	for ( i = 0; i < N; i++ ) {
		IPIV[ i ] = tc.ipiv[ i ] - 1;
	}
	B = toComplex128( tc.B );
	X = toComplex128( tc.Xinit );
	FERR = new Float64Array( nrhs );
	BERR = new Float64Array( nrhs );
	WORK = new Complex128Array( 2 * N );
	RWORK = new Float64Array( N );

	info = zsprfs( uplo, N, nrhs, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, N, 0, X, 1, N, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, tc.info, 'info' );

	Xv = reinterpret( X, 0 );
	assertArrayClose( Xv, new Float64Array( tc.X ), 1e-12, 'X' );
	assertArrayClose( FERR, new Float64Array( tc.ferr ), 0.5, 'ferr' );
	assertArrayClose( BERR, new Float64Array( tc.berr ), 0.5, 'berr' );
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zsprfs, 'function' );
});

test( 'zsprfs: upper, 3x3, 1 RHS', function t() {
	var tc = findCase( 'upper_3x3' );
	runTest( 'upper', tc, 3, 1 );
});

test( 'zsprfs: lower, 3x3, 1 RHS', function t() {
	var tc = findCase( 'lower_3x3' );
	runTest( 'lower', tc, 3, 1 );
});

test( 'zsprfs: upper, 3x3, 2 RHS', function t() {
	var tc = findCase( 'upper_3x3_2rhs' );
	runTest( 'upper', tc, 3, 2 );
});

test( 'zsprfs: N=0', function t() {
	var RWORK;
	var WORK;
	var FERR;
	var BERR;
	var IPIV;
	var info;
	var AFP;
	var AP;
	var B;
	var X;

	AP = new Complex128Array( 1 );
	AFP = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );
	X = new Complex128Array( 1 );
	FERR = new Float64Array( [ 99.0 ] );
	BERR = new Float64Array( [ 99.0 ] );
	WORK = new Complex128Array( 1 );
	RWORK = new Float64Array( 1 );

	info = zsprfs( 'upper', 0, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	assert.equal( FERR[ 0 ], 0.0, 'ferr' );
	assert.equal( BERR[ 0 ], 0.0, 'berr' );
});

test( 'zsprfs: N=1', function t() {
	var tc = findCase( 'n1' );
	runTest( 'upper', tc, 1, 1 );
});
