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
var dormbr = require( './../lib/dormbr.js' );


// TESTS //

test( 'dormbr is a function', function t() {
	assert.strictEqual( typeof dormbr, 'function', 'is a function' );
});

test( 'dormbr has expected arity', function t() {
	assert.strictEqual( dormbr.length, 15, 'has expected arity' );
});

test( 'dormbr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dormbr( 'invalid', 'q', 'left', 'no-transpose', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'dormbr throws TypeError for invalid vect', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 'invalid', 'left', 'no-transpose', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'dormbr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 'q', 'invalid', 'no-transpose', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'dormbr throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 'q', 'left', 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'dormbr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 'q', 'left', 'no-transpose', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, RangeError );
});

test( 'dormbr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 'q', 'left', 'no-transpose', 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, RangeError );
});

test( 'dormbr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 'q', 'left', 'no-transpose', 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1 );
	}, RangeError );
});
