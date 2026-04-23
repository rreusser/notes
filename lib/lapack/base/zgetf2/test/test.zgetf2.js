/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgetf2 = require( './../lib/zgetf2.js' );


// TESTS //

test( 'zgetf2 is a function', function t() {
	assert.strictEqual( typeof zgetf2, 'function', 'is a function' );
});

test( 'zgetf2 has expected arity', function t() {
	assert.strictEqual( zgetf2.length, 7, 'has expected arity' );
});

test( 'zgetf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgetf2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgetf2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgetf2( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgetf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgetf2( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
