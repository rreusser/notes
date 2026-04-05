/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhetri = require( './../lib/zhetri.js' );


// TESTS //

test( 'zhetri is a function', function t() {
	assert.strictEqual( typeof zhetri, 'function', 'is a function' );
});

test( 'zhetri has expected arity', function t() {
	assert.strictEqual( zhetri.length, 5, 'has expected arity' );
});

test( 'zhetri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhetri( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhetri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhetri( 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
