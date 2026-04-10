/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtgsna = require( './../lib/dtgsna.js' );


// TESTS //

test( 'dtgsna is a function', function t() {
	assert.strictEqual( typeof dtgsna, 'function', 'is a function' );
});

test( 'dtgsna has expected arity', function t() {
	assert.strictEqual( dtgsna.length, 26, 'has expected arity' );
});

test( 'dtgsna throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtgsna( 'invalid', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dtgsna throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtgsna( 'row-major', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, RangeError );
});

test( 'dtgsna throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtgsna( 'row-major', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, RangeError );
});
