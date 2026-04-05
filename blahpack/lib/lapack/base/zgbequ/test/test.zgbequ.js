/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbequ = require( './../lib/zgbequ.js' );


// TESTS //

test( 'zgbequ is a function', function t() {
	assert.strictEqual( typeof zgbequ, 'function', 'is a function' );
});

test( 'zgbequ has expected arity', function t() {
	assert.strictEqual( zgbequ.length, 10, 'has expected arity' );
});

test( 'zgbequ throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgbequ( -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1 );
	}, RangeError );
});

test( 'zgbequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgbequ( new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1 );
	}, RangeError );
});
