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

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrmm = require( './../lib' );
var base = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

test( 'ndarray: throws TypeError for invalid side', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'invalid', 'upper', 'no-transpose', 'non-unit', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'invalid', 'no-transpose', 'non-unit', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid transa', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'invalid', 'non-unit', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'invalid', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative M', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'non-unit', -1, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'non-unit', 2, -1, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});
