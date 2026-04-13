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

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlaHerfsxExtended = require( './../lib/zla_herfsx_extended.js' );


// FUNCTIONS //

/**
* Invokes the layout wrapper with the supplied `order`, `uplo`, and `N`.
*
* @private
* @param {string} order - storage layout
* @param {string} uplo - matrix triangle
* @param {integer} N - order of `A`
* @returns {Function} thunk suitable for `assert.throws`
*/
function call( order, uplo, N ) {
	return function thunk() {
		var nrhs;
		var IPIV;
		var BERR;
		var AYB;
		var EBN;
		var EBC;
		var RES;
		var AF;
		var DY;
		var YT;
		var B;
		var Y;
		var A;
		var c;
		nrhs = 1;
		A = new Complex128Array( 4 );
		AF = new Complex128Array( 4 );
		IPIV = new Int32Array( 2 );
		c = new Float64Array( 2 );
		B = new Complex128Array( 2 );
		Y = new Complex128Array( 2 );
		BERR = new Float64Array( 1 );
		EBN = new Float64Array( 3 );
		EBC = new Float64Array( 3 );
		RES = new Complex128Array( 2 );
		AYB = new Float64Array( 2 );
		DY = new Complex128Array( 2 );
		YT = new Complex128Array( 2 );
		zlaHerfsxExtended( order, 1, uplo, N, nrhs, A, 2, AF, 2, IPIV, false, c, B, 2, Y, 2, BERR, 2, EBN, 1, EBC, 1, RES, AYB, DY, YT, 1e-10, 10, 0.5, 0.25, false );
	};
}


// TESTS //

test( 'zlaHerfsxExtended is a function', function t() {
	assert.strictEqual( typeof zlaHerfsxExtended, 'function', 'is a function' );
});

test( 'zlaHerfsxExtended has expected arity', function t() {
	assert.strictEqual( zlaHerfsxExtended.length, 31, 'has expected arity' );
});

test( 'throws TypeError for invalid order', function t() {
	assert.throws( call( 'invalid', 'upper', 2 ), TypeError );
});

test( 'throws TypeError for invalid uplo', function t() {
	assert.throws( call( 'row-major', 'invalid', 2 ), TypeError );
});

test( 'throws RangeError for negative N', function t() {
	assert.throws( call( 'row-major', 'upper', -1 ), RangeError );
});
