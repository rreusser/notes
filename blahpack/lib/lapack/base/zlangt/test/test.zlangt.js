/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlangt = require( './../lib/zlangt.js' );


// TESTS //

test( 'zlangt is a function', function t() {
	assert.strictEqual( typeof zlangt, 'function', 'is a function' );
});

test( 'zlangt has expected arity', function t() {
	assert.strictEqual( zlangt.length, 8, 'has expected arity' );
});

test( 'zlangt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlangt( 2, -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
