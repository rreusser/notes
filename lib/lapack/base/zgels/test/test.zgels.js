/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgels = require( './../lib/zgels.js' );


// TESTS //

test( 'zgels is a function', function t() {
	assert.strictEqual( typeof zgels, 'function', 'is a function' );
});

test( 'zgels has expected arity', function t() {
	assert.strictEqual( zgels.length, 11, 'has expected arity' );
});

test( 'zgels throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zgels( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zgels throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgels( 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zgels throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgels( 'no-transpose', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zgels throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgels( 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
