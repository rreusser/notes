/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dormrq = require( './../lib/dormrq.js' );


// TESTS //

test( 'dormrq is a function', function t() {
	assert.strictEqual( typeof dormrq, 'function', 'is a function' );
});

test( 'dormrq has expected arity', function t() {
	assert.strictEqual( dormrq.length, 13, 'has expected arity' );
});

test( 'dormrq throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormrq( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormrq throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormrq( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormrq throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormrq( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormrq throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormrq( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormrq throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dormrq( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
