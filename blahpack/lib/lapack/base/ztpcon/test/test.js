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
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var ztpcon = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztpcon.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

/**
* Finds a test case by name in the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case object
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

/**
* Asserts that two values are close within a relative tolerance.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - relative tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Creates a Complex128Array from a flat Float64 interleaved array.
*
* @private
* @param {Array<number>} arr - interleaved real/imaginary values
* @returns {Complex128Array} complex array
*/
function toComplex( arr ) {
	return new Complex128Array( new Float64Array( arr ) );
}


// TESTS //

test( 'ztpcon: main export is a function', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main, 'function' );
});

test( 'ztpcon: attached to the main export is an `ndarray` method', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main.ndarray, 'function' );
});

test( 'ztpcon: upper_nonunit_1norm', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'upper_nonunit_1norm' );
	var AP = toComplex( tc.AP );
	var WORK = new Complex128Array( 2 * 3 );
	var RWORK = new Float64Array( 3 );
	var info = ztpcon( 'one-norm', 'upper', 'non-unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: upper_nonunit_Inorm', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'upper_nonunit_Inorm' );
	var tcAP = findCase( 'upper_nonunit_1norm' );
	var AP = toComplex( tcAP.AP );
	var WORK = new Complex128Array( 2 * 3 );
	var RWORK = new Float64Array( 3 );
	var info = ztpcon( 'inf-norm', 'upper', 'non-unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: lower_nonunit_1norm', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'lower_nonunit_1norm' );
	var AP = toComplex( tc.AP );
	var WORK = new Complex128Array( 2 * 3 );
	var RWORK = new Float64Array( 3 );
	var info = ztpcon( 'one-norm', 'lower', 'non-unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: upper_unit_1norm', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'upper_unit_1norm' );
	var AP = toComplex( [ 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.5, 0.0, 1.0, -1.0, 1.0, 0.0 ] );
	var WORK = new Complex128Array( 2 * 3 );
	var RWORK = new Float64Array( 3 );
	var info = ztpcon( 'one-norm', 'upper', 'unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: identity', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'identity' );
	var AP = toComplex( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
	var WORK = new Complex128Array( 2 * 3 );
	var RWORK = new Float64Array( 3 );
	var info = ztpcon( 'one-norm', 'upper', 'non-unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: n_zero', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'n_zero' );
	var AP = new Complex128Array( 0 );
	var WORK = new Complex128Array( 0 );
	var RWORK = new Float64Array( 0 );
	var info = ztpcon( 'one-norm', 'upper', 'non-unit', 0, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: 4x4_lower_Inorm', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( '4x4_lower_Inorm' );
	var AP = toComplex( tc.AP );
	var WORK = new Complex128Array( 2 * 4 );
	var RWORK = new Float64Array( 4 );
	var info = ztpcon( 'inf-norm', 'lower', 'non-unit', 4, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: lower_unit_Inorm', function t() {
	var RCOND = new Float64Array( 1 );
	var tc = findCase( 'lower_unit_Inorm' );
	var AP = toComplex( [ 1.0, 0.0, 0.5, 0.5, 0.0, 0.0, 1.0, 0.0, 0.5, -0.5, 1.0, 0.0 ] );
	var WORK = new Complex128Array( 2 * 3 );
	var RWORK = new Float64Array( 3 );
	var info = ztpcon( 'inf-norm', 'lower', 'unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztpcon: ndarray validation - invalid uplo throws', function t() {
	var ndarray = require( './../lib/ndarray.js' );
	var AP = new Complex128Array( 6 );
	var WORK = new Complex128Array( 6 );
	var RWORK = new Float64Array( 3 );
	var RCOND = new Float64Array( 1 );
	assert.throws( function throws() {
		ndarray( 'one-norm', 'INVALID', 'non-unit', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	}, /invalid argument/ );
});

test( 'ztpcon: ndarray validation - invalid diag throws', function t() {
	var ndarray = require( './../lib/ndarray.js' );
	var AP = new Complex128Array( 6 );
	var WORK = new Complex128Array( 6 );
	var RWORK = new Float64Array( 3 );
	var RCOND = new Float64Array( 1 );
	assert.throws( function throws() {
		ndarray( 'one-norm', 'upper', 'INVALID', 3, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
	}, /invalid argument/ );
});
