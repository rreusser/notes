/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dormr2 = require( './../lib/dormr2.js' );


// TESTS //

test( 'dormr2 is a function', function t() {
	assert.strictEqual( typeof dormr2, 'function', 'is a function' );
});

test( 'dormr2 has expected arity', function t() {
	assert.strictEqual( dormr2.length, 13, 'has expected arity' );
});

test( 'dormr2 throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormr2( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormr2 throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormr2( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormr2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormr2( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormr2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormr2( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormr2 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dormr2( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
