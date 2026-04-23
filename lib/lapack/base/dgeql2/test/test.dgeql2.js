

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeql2 = require( './../lib/dgeql2.js' );


// TESTS //

test( 'dgeql2 is a function', function t() {
	assert.strictEqual( typeof dgeql2, 'function', 'is a function' );
});

test( 'dgeql2 has expected arity', function t() {
	assert.strictEqual( dgeql2.length, 9, 'has expected arity' );
});

test( 'dgeql2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgeql2( 'invalid', 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgeql2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgeql2( 'row-major', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgeql2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgeql2( 'row-major', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

