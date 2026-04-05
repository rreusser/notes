/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dptsvx = require( './../lib/dptsvx.js' );


// TESTS //

test( 'dptsvx is a function', function t() {
	assert.strictEqual( typeof dptsvx, 'function', 'is a function' );
});

test( 'dptsvx has expected arity', function t() {
	assert.strictEqual( dptsvx.length, 15, 'has expected arity' );
});

test( 'dptsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dptsvx( 2, -1, 2, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'dptsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dptsvx( 2, new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
