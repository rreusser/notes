/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhegs2 = require( './../lib/zhegs2.js' );


// TESTS //

test( 'zhegs2 is a function', function t() {
	assert.strictEqual( typeof zhegs2, 'function', 'is a function' );
});

test( 'zhegs2 has expected arity', function t() {
	assert.strictEqual( zhegs2.length, 7, 'has expected arity' );
});

test( 'zhegs2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhegs2( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zhegs2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhegs2( 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
