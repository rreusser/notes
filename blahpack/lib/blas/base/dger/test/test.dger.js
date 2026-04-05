/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dger = require( './../lib/dger.js' );


// TESTS //

test( 'dger is a function', function t() {
	assert.strictEqual( typeof dger, 'function', 'is a function' );
});

test( 'dger has expected arity', function t() {
	assert.strictEqual( dger.length, 10, 'has expected arity' );
});

test( 'dger throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dger( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dger throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dger( 'row-major', -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dger throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dger( 'row-major', new Float64Array( 4 ), -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
