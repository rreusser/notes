
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrsna = require( './../lib/ztrsna.js' );


// TESTS //

test( 'ztrsna is a function', function t() {
	assert.strictEqual( typeof ztrsna, 'function', 'is a function' );
});

test( 'ztrsna has expected arity', function t() {
	assert.strictEqual( ztrsna.length, 22, 'has expected arity' );
});

test( 'ztrsna throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrsna( 'invalid', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrsna throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrsna( 'row-major', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztrsna throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztrsna( 'row-major', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
