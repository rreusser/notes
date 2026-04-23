/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zptts2 = require( './../lib/zptts2.js' );


// TESTS //

test( 'zptts2 is a function', function t() {
	assert.strictEqual( typeof zptts2, 'function', 'is a function' );
});

test( 'zptts2 has expected arity', function t() {
	assert.strictEqual( zptts2.length, 9, 'has expected arity' );
});

test( 'zptts2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zptts2( 2, -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zptts2 throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zptts2( 2, new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
