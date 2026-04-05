/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgttrs = require( './../lib/zgttrs.js' );


// TESTS //

test( 'zgttrs is a function', function t() {
	assert.strictEqual( typeof zgttrs, 'function', 'is a function' );
});

test( 'zgttrs has expected arity', function t() {
	assert.strictEqual( zgttrs.length, 15, 'has expected arity' );
});

test( 'zgttrs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zgttrs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zgttrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgttrs( 'no-transpose', -1, 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zgttrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgttrs( 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
