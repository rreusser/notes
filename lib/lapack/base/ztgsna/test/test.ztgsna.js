/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgsna = require( './../lib/ztgsna.js' );


// TESTS //

test( 'ztgsna is a function', function t() {
	assert.strictEqual( typeof ztgsna, 'function', 'is a function' );
});

test( 'ztgsna has expected arity', function t() {
	assert.strictEqual( ztgsna.length, 26, 'has expected arity' );
});

test( 'ztgsna throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztgsna( 'invalid', 'both', 'all', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'ztgsna throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgsna( 'row-major', 'both', 'all', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, RangeError );
});

test( 'ztgsna throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztgsna( 'row-major', 'both', 'all', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, RangeError );
});
