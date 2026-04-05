/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgerfs = require( './../lib/dgerfs.js' );


// TESTS //

test( 'dgerfs is a function', function t() {
	assert.strictEqual( typeof dgerfs, 'function', 'is a function' );
});

test( 'dgerfs has expected arity', function t() {
	assert.strictEqual( dgerfs.length, 17, 'has expected arity' );
});

test( 'dgerfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dgerfs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgerfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgerfs( 'no-transpose', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dgerfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dgerfs( 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
