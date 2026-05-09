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

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlasyfAa = require( './../lib/dlasyf_aa.js' );


// TESTS //

test( 'dlasyfAa is a function', function t() {
	assert.strictEqual( typeof dlasyfAa, 'function', 'is a function' );
});

test( 'dlasyfAa has expected arity', function t() {
	assert.strictEqual( dlasyfAa.length, 14, 'has expected arity' );
});

test( 'dlasyfAa throws TypeError for invalid order', function t() {
	var WORK = new Float64Array( 2 );
	var IPIV = new Int32Array( 2 );
	var A = new Float64Array( 4 );
	var H = new Float64Array( 4 );
	assert.throws( function throws() {
		dlasyfAa( 'invalid', 'upper', 1, 2, 2, A, 2, IPIV, 1, 0, H, 2, WORK, 1 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'dlasyfAa throws TypeError for invalid uplo', function t() {
	var WORK = new Float64Array( 2 );
	var IPIV = new Int32Array( 2 );
	var A = new Float64Array( 4 );
	var H = new Float64Array( 4 );
	assert.throws( function throws() {
		dlasyfAa( 'column-major', 'invalid', 1, 2, 2, A, 2, IPIV, 1, 0, H, 2, WORK, 1 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'dlasyfAa throws RangeError for negative M', function t() {
	var WORK = new Float64Array( 2 );
	var IPIV = new Int32Array( 2 );
	var A = new Float64Array( 4 );
	var H = new Float64Array( 4 );
	assert.throws( function throws() {
		dlasyfAa( 'column-major', 'upper', 1, -1, 2, A, 2, IPIV, 1, 0, H, 2, WORK, 1 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dlasyfAa throws RangeError for LDA < M', function t() {
	var WORK = new Float64Array( 2 );
	var IPIV = new Int32Array( 2 );
	var A = new Float64Array( 4 );
	var H = new Float64Array( 4 );
	assert.throws( function throws() {
		dlasyfAa( 'column-major', 'upper', 1, 2, 2, A, 1, IPIV, 1, 0, H, 2, WORK, 1 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dlasyfAa throws RangeError for LDH < M', function t() {
	var WORK = new Float64Array( 2 );
	var IPIV = new Int32Array( 2 );
	var A = new Float64Array( 4 );
	var H = new Float64Array( 4 );
	assert.throws( function throws() {
		dlasyfAa( 'column-major', 'upper', 1, 2, 2, A, 2, IPIV, 1, 0, H, 1, WORK, 1 ); // eslint-disable-line max-len
	}, RangeError );
});
