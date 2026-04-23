/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunmql = require( './../lib/zunmql.js' );


// TESTS //

test( 'zunmql is a function', function t() {
	assert.strictEqual( typeof zunmql, 'function', 'is a function' );
});

test( 'zunmql has expected arity', function t() {
	assert.strictEqual( zunmql.length, 14, 'has expected arity' );
});

test( 'zunmql throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zunmql( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zunmql throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zunmql( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zunmql throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunmql( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zunmql throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunmql( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zunmql throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunmql( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
