/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasd2 = require( './../lib/dlasd2.js' );


// TESTS //

test( 'dlasd2 is a function', function t() {
	assert.strictEqual( typeof dlasd2, 'function', 'is a function' );
});

test( 'dlasd2 has expected arity', function t() {
	assert.strictEqual( dlasd2.length, 23, 'has expected arity' );
});

test( 'dlasd2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlasd2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dlasd2 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dlasd2( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
