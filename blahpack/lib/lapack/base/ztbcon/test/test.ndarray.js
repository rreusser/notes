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
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ztbcon = require( './../lib/base.js' );

// FIXTURES //

var upper_nonunit_1norm_k2 = require( './fixtures/upper_nonunit_1norm_k2.json' );
var upper_nonunit_inorm_k2 = require( './fixtures/upper_nonunit_inorm_k2.json' );
var lower_nonunit_1norm_k2 = require( './fixtures/lower_nonunit_1norm_k2.json' );
var lower_nonunit_inorm_k2 = require( './fixtures/lower_nonunit_inorm_k2.json' );
var upper_unit_1norm_k1 = require( './fixtures/upper_unit_1norm_k1.json' );
var lower_unit_inorm_k1 = require( './fixtures/lower_unit_inorm_k1.json' );
var n_zero = require( './fixtures/n_zero.json' );
var identity_k0 = require( './fixtures/identity_k0.json' );
var upper_nonunit_k1_1norm = require( './fixtures/upper_nonunit_k1_1norm.json' );
var upper_nonunit_k1_inorm = require( './fixtures/upper_nonunit_k1_inorm.json' );
var lower_nonunit_k1_1norm = require( './fixtures/lower_nonunit_k1_1norm.json' );
var lower_nonunit_k1_inorm = require( './fixtures/lower_nonunit_k1_inorm.json' );

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

// TESTS //

test( 'ztbcon: upper_nonunit_1norm_k2', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = upper_nonunit_1norm_k2;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 4 );
	WORK = new Complex128Array( 8 );
	AB = new Complex128Array([
		0,
		0,
		0,
		0,
		4,
		1,           // col 0: rows 0,1,2
		0,
		0,
		1,
		1,
		3,
		0,           // col 1
		0.5,
		0,
		1,
		-1,
		2,
		1,        // col 2
		0.5,
		1,
		1,
		0,
		3,
		-1         // col 3
	]);
	info = ztbcon( 'one-norm', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: upper_nonunit_Inorm_k2', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = upper_nonunit_inorm_k2;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 4 );
	WORK = new Complex128Array( 8 );
	AB = new Complex128Array([
		0,
		0,
		0,
		0,
		4,
		1,
		0,
		0,
		1,
		1,
		3,
		0,
		0.5,
		0,
		1,
		-1,
		2,
		1,
		0.5,
		1,
		1,
		0,
		3,
		-1
	]);
	info = ztbcon( 'inf-norm', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: lower_nonunit_1norm_k2', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = lower_nonunit_1norm_k2;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 4 );
	WORK = new Complex128Array( 8 );
	AB = new Complex128Array([
		3,
		1,
		1,
		0,
		0.5,
		1,
		4,
		-1,
		1,
		-1,
		0.5,
		0,
		2,
		0,
		1,
		1,
		0,
		0,
		3,
		1,
		0,
		0,
		0,
		0
	]);
	info = ztbcon( 'one-norm', 'lower', 'non-unit', 4, 2, AB, 1, 3, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: lower_nonunit_Inorm_k2', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = lower_nonunit_inorm_k2;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 4 );
	WORK = new Complex128Array( 8 );
	AB = new Complex128Array([
		3,
		1,
		1,
		0,
		0.5,
		1,
		4,
		-1,
		1,
		-1,
		0.5,
		0,
		2,
		0,
		1,
		1,
		0,
		0,
		3,
		1,
		0,
		0,
		0,
		0
	]);
	info = ztbcon( 'inf-norm', 'lower', 'non-unit', 4, 2, AB, 1, 3, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: upper_unit_1norm_k1', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = upper_unit_1norm_k1;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 3 );
	WORK = new Complex128Array( 6 );
	AB = new Complex128Array([
		0,
		0,
		99,
		99,        // col 0
		1,
		1,
		99,
		99,        // col 1
		1,
		-1,
		99,
		99        // col 2
	]);
	info = ztbcon( 'one-norm', 'upper', 'unit', 3, 1, AB, 1, 2, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: lower_unit_Inorm_k1', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = lower_unit_inorm_k1;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 3 );
	WORK = new Complex128Array( 6 );
	AB = new Complex128Array([
		99,
		99,
		0.5,
		0.5,      // col 0
		99,
		99,
		0.5,
		-0.5,     // col 1
		99,
		99,
		0,
		0           // col 2
	]);
	info = ztbcon( 'inf-norm', 'lower', 'unit', 3, 1, AB, 1, 2, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: n_zero', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = n_zero;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 1 );
	WORK = new Complex128Array( 1 );
	AB = new Complex128Array( 1 );
	info = ztbcon( 'one-norm', 'upper', 'non-unit', 0, 0, AB, 1, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: identity_k0', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = identity_k0;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 3 );
	WORK = new Complex128Array( 6 );
	AB = new Complex128Array([
		1,
		0,      // col 0
		1,
		0,      // col 1
		1,
		0       // col 2
	]);
	info = ztbcon( 'one-norm', 'upper', 'non-unit', 3, 0, AB, 1, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: upper_nonunit_k1_1norm', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = upper_nonunit_k1_1norm;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 4 );
	WORK = new Complex128Array( 8 );
	AB = new Complex128Array([
		0,
		0,
		2,
		1,           // col 0
		1,
		0,
		3,
		-1,          // col 1
		0.5,
		1,
		1,
		2,         // col 2
		1,
		-1,
		4,
		0           // col 3
	]);
	info = ztbcon( 'one-norm', 'upper', 'non-unit', 4, 1, AB, 1, 2, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: upper_nonunit_k1_Inorm', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = upper_nonunit_k1_inorm;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 4 );
	WORK = new Complex128Array( 8 );
	AB = new Complex128Array([
		0,
		0,
		2,
		1,
		1,
		0,
		3,
		-1,
		0.5,
		1,
		1,
		2,
		1,
		-1,
		4,
		0
	]);
	info = ztbcon( 'inf-norm', 'upper', 'non-unit', 4, 1, AB, 1, 2, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: lower_nonunit_k1_1norm', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = lower_nonunit_k1_1norm;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 5 );
	WORK = new Complex128Array( 10 );
	AB = new Complex128Array([
		2,
		1,
		1,
		1,
		3,
		0,
		0.5,
		-1,
		4,
		1,
		1,
		0,
		2,
		-1,
		0.5,
		1,
		5,
		0,
		0,
		0
	]);
	info = ztbcon( 'one-norm', 'lower', 'non-unit', 5, 1, AB, 1, 2, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ztbcon: lower_nonunit_k1_Inorm', function t() {
	var RCOND;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;

	tc = lower_nonunit_k1_inorm;
	RCOND = new Float64Array( 1 );
	RWORK = new Float64Array( 5 );
	WORK = new Complex128Array( 10 );
	AB = new Complex128Array([
		2,
		1,
		1,
		1,
		3,
		0,
		0.5,
		-1,
		4,
		1,
		1,
		0,
		2,
		-1,
		0.5,
		1,
		5,
		0,
		0,
		0
	]);
	info = ztbcon( 'inf-norm', 'lower', 'non-unit', 5, 1, AB, 1, 2, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assertClose( RCOND[ 0 ], tc.rcond, 1e-14, 'rcond' );
});
