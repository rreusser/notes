/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgesv = require( './../lib/dgesv.js' );


// TESTS //

test( 'dgesv is a function', function t() {
	assert.strictEqual( typeof dgesv, 'function', 'is a function' );
});

test( 'dgesv has expected arity', function t() {
	assert.strictEqual( dgesv.length, 9, 'has expected arity' );
});

test( 'dgesv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgesv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgesv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgesv( 'row-major', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgesv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dgesv( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
