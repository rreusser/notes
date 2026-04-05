/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgerq2 = require( './../lib/zgerq2.js' );


// TESTS //

test( 'zgerq2 is a function', function t() {
	assert.strictEqual( typeof zgerq2, 'function', 'is a function' );
});

test( 'zgerq2 has expected arity', function t() {
	assert.strictEqual( zgerq2.length, 8, 'has expected arity' );
});

test( 'zgerq2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgerq2( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgerq2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgerq2( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
