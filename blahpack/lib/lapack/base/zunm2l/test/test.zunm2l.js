/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunm2l = require( './../lib/zunm2l.js' );


// TESTS //

test( 'zunm2l is a function', function t() {
	assert.strictEqual( typeof zunm2l, 'function', 'is a function' );
});

test( 'zunm2l has expected arity', function t() {
	assert.strictEqual( zunm2l.length, 13, 'has expected arity' );
});

test( 'zunm2l throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zunm2l( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunm2l throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zunm2l( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunm2l throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunm2l( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunm2l throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunm2l( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunm2l throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunm2l( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
