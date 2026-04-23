

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrsna = require( './../lib/dtrsna.js' );


// TESTS //

test( 'dtrsna is a function', function t() {
	assert.strictEqual( typeof dtrsna, 'function', 'is a function' );
});

test( 'dtrsna has expected arity', function t() {
	assert.strictEqual( dtrsna.length, 23, 'has expected arity' );
});

test( 'dtrsna throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtrsna( 'invalid', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dtrsna throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrsna( 'row-major', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});

test( 'dtrsna throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtrsna( 'row-major', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});

