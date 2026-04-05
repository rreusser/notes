/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunmrq = require( './../lib/zunmrq.js' );


// TESTS //

test( 'zunmrq is a function', function t() {
	assert.strictEqual( typeof zunmrq, 'function', 'is a function' );
});

test( 'zunmrq has expected arity', function t() {
	assert.strictEqual( zunmrq.length, 14, 'has expected arity' );
});

test( 'zunmrq throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zunmrq( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zunmrq throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zunmrq( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zunmrq throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunmrq( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zunmrq throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunmrq( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zunmrq throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunmrq( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
