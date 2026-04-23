/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlarrb = require( './../lib/dlarrb.js' );


// TESTS //

test( 'dlarrb is a function', function t() {
	assert.strictEqual( typeof dlarrb, 'function', 'is a function' );
});

test( 'dlarrb has expected arity', function t() {
	assert.strictEqual( dlarrb.length, 24, 'has expected arity' );
});

test( 'dlarrb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarrb( -1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 1, 1, 1e-8, 1e-14, 0, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 8 ), 1, new Int32Array( 8 ), 1, 0, 1e-30, 1.0, -1 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dlarrb: zero-size quick return via layout wrapper', function t() {
	var info = dlarrb( 0, new Float64Array( 1 ), 1, new Float64Array( 1 ), 1, 1, 0, 1e-8, 1e-14, 0, new Float64Array( 1 ), 1, new Float64Array( 1 ), 1, new Float64Array( 1 ), 1, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, 0, 1e-30, 1.0, -1 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info' );
});
