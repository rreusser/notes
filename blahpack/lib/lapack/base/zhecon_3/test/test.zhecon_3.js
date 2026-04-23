/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zhecon3 = require( './../lib/zhecon_3.js' );


// TESTS //

test( 'zhecon3 is a function', function t() {
	assert.strictEqual( typeof zhecon3, 'function', 'is a function' );
});

test( 'zhecon3 has expected arity', function t() {
	assert.strictEqual( zhecon3.length, 13, 'has expected arity' );
});

test( 'zhecon3 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhecon3( 'invalid', 'upper', 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 1.0, new Float64Array( 1 ), new Complex128Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhecon3 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhecon3( 'column-major', 'invalid', 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 1.0, new Float64Array( 1 ), new Complex128Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhecon3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhecon3( 'column-major', 'upper', -1, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 1.0, new Float64Array( 1 ), new Complex128Array( 4 ), 1 );
	}, RangeError );
});

test( 'zhecon3 throws RangeError for LDA < max(1,N)', function t() {
	assert.throws( function throws() {
		zhecon3( 'column-major', 'upper', 4, new Complex128Array( 16 ), 2, new Complex128Array( 4 ), 1, new Int32Array( 4 ), 1, 1.0, new Float64Array( 1 ), new Complex128Array( 8 ), 1 );
	}, RangeError );
});
