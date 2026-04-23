/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zggbal = require( './../lib/zggbal.js' );


// TESTS //

test( 'zggbal is a function', function t() {
	assert.strictEqual( typeof zggbal, 'function', 'is a function' );
});

test( 'zggbal has expected arity', function t() {
	assert.strictEqual( zggbal.length, 13, 'has expected arity' );
});

test( 'zggbal throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zggbal( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zggbal throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zggbal( 'row-major', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
