/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dormql = require( './../lib/dormql.js' );


// TESTS //

test( 'dormql is a function', function t() {
	assert.strictEqual( typeof dormql, 'function', 'is a function' );
});

test( 'dormql has expected arity', function t() {
	assert.strictEqual( dormql.length, 13, 'has expected arity' );
});

test( 'dormql throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormql( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormql throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormql( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormql throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormql( 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormql throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormql( 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormql throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dormql( 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
