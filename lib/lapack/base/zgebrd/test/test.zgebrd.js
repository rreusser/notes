/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgebrd = require( './../lib/zgebrd.js' );


// TESTS //

test( 'zgebrd is a function', function t() {
	assert.strictEqual( typeof zgebrd, 'function', 'is a function' );
});

test( 'zgebrd has expected arity', function t() {
	assert.strictEqual( zgebrd.length, 16, 'has expected arity' );
});

test( 'zgebrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgebrd( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zgebrd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgebrd( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zgebrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgebrd( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
