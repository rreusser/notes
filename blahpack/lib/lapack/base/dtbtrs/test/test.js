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
var Float64Array = require( '@stdlib/array/float64' );
var dtbtrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtbtrs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) { return t.name === name;
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


// TESTS //

test( 'dtbtrs is a function', function t() {
	assert.equal( typeof dtbtrs, 'function' );
});

test( 'dtbtrs: upper_no_trans (upper triangular, no transpose, KD=2, N=4)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'upper_no_trans' );
	ab = new Float64Array([
		0,
		0,
		3,   // col 1: *, *, a11
		0,
		1,
		4,   // col 2: *, a12, a22
		2,
		1,
		5,   // col 3: a13, a23, a33
		0,
		3,
		6    // col 4: *, a24, a44
	]);
	b = new Float64Array([ 10, 20, 15, 12 ]);
	info = dtbtrs( 'upper', 'no-transpose', 'non-unit', 4, 2, 1, ab, 1, 3, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: lower_no_trans (lower triangular, no transpose, KD=2, N=4)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'lower_no_trans' );
	ab = new Float64Array([
		3,
		1,
		2,   // col 1: a11, a21, a31
		4,
		1,
		3,   // col 2: a22, a32, a42
		5,
		2,
		0,   // col 3: a33, a43, *
		6,
		0,
		0    // col 4: a44, *, *
	]);
	b = new Float64Array([ 10, 20, 15, 12 ]);
	info = dtbtrs( 'lower', 'no-transpose', 'non-unit', 4, 2, 1, ab, 1, 3, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: upper_trans (upper triangular, transpose, KD=2, N=4)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'upper_trans' );
	ab = new Float64Array([
		0,
		0,
		3,
		0,
		1,
		4,
		2,
		1,
		5,
		0,
		3,
		6
	]);
	b = new Float64Array([ 10, 20, 15, 12 ]);
	info = dtbtrs( 'upper', 'transpose', 'non-unit', 4, 2, 1, ab, 1, 3, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: lower_trans (lower triangular, transpose, KD=2, N=4)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'lower_trans' );
	ab = new Float64Array([
		3,
		1,
		2,
		4,
		1,
		3,
		5,
		2,
		0,
		6,
		0,
		0
	]);
	b = new Float64Array([ 10, 20, 15, 12 ]);
	info = dtbtrs( 'lower', 'transpose', 'non-unit', 4, 2, 1, ab, 1, 3, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: upper_unit_diag (unit diagonal, upper triangular, KD=1, N=4)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'upper_unit_diag' );
	ab = new Float64Array([
		0,
		1,   // col 1: *, a11(=1)
		2,
		1,   // col 2: a12, a22(=1)
		3,
		1,   // col 3: a23, a33(=1)
		4,
		1    // col 4: a34, a44(=1)
	]);
	b = new Float64Array([ 10, 6, 3, 1 ]);
	info = dtbtrs( 'upper', 'no-transpose', 'unit', 4, 1, 1, ab, 1, 2, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: n_zero (N=0 quick return)', function t() {
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'n_zero' );
	ab = new Float64Array([ 1.0 ]);
	b = new Float64Array([ 1.0 ]);
	info = dtbtrs( 'upper', 'no-transpose', 'non-unit', 0, 0, 1, ab, 1, 1, 0, b, 1, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtbtrs: singular (zero diagonal, upper, info=2)', function t() {
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'singular' );
	ab = new Float64Array([
		0,
		2,   // col 1: *, a11=2
		1,
		0,   // col 2: a12=1, a22=0
		1,
		3    // col 3: a23=1, a33=3
	]);
	b = new Float64Array([ 1, 2, 3 ]);
	info = dtbtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, 1, ab, 1, 2, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtbtrs: multi_rhs (NRHS=2, upper, KD=1, N=3)', function t() {
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'multi_rhs' );
	ab = new Float64Array([
		0,
		3,   // col 1: *, a11=3
		1,
		4,   // col 2: a12=1, a22=4
		2,
		5    // col 3: a23=2, a33=5
	]);
	b = new Float64Array([ 1, 2, 3, 7, 8, 9 ]);
	info = dtbtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, 2, ab, 1, 2, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: n_one (N=1 edge case)', function t() {
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'n_one' );
	ab = new Float64Array([ 5.0 ]);
	b = new Float64Array([ 15.0 ]);
	info = dtbtrs( 'upper', 'no-transpose', 'non-unit', 1, 0, 1, ab, 1, 1, 0, b, 1, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtbtrs: lower_singular (lower, zero diagonal at row 1, info=1)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'lower_singular' );
	ab = new Float64Array([
		0,
		1,   // col 1: a11=0, a21=1
		4,
		2,   // col 2: a22=4, a32=2
		5,
		0    // col 3: a33=5, *
	]);
	b = new Float64Array([ 1, 2, 3 ]);
	info = dtbtrs( 'lower', 'no-transpose', 'non-unit', 3, 1, 1, ab, 1, 2, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtbtrs: upper_conj_trans (conjugate-transpose, same as transpose for real)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ab;
	var b;

	tc = findCase( 'upper_conj_trans' );
	ab = new Float64Array([
		0,
		0,
		3,
		0,
		1,
		4,
		2,
		1,
		5,
		0,
		3,
		6
	]);
	b = new Float64Array([ 10, 20, 15, 12 ]);
	info = dtbtrs( 'upper', 'transpose', 'non-unit', 4, 2, 1, ab, 1, 3, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});
