/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunmr2 = require( './../lib/zunmr2.js' );


// TESTS //

test( 'zunmr2 is a function', function t() {
	assert.strictEqual( typeof zunmr2, 'function', 'is a function' );
});

test( 'zunmr2 has expected arity', function t() {
	assert.strictEqual( zunmr2.length, 13, 'has expected arity' );
});

test( 'zunmr2 throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zunmr2( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmr2 throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zunmr2( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmr2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunmr2( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunmr2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunmr2( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunmr2 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunmr2( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
