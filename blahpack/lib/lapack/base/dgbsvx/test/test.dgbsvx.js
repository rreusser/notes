/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgbsvx = require( './../lib/dgbsvx.js' );


// TESTS //

test( 'dgbsvx is a function', function t() {
	assert.strictEqual( typeof dgbsvx, 'function', 'is a function' );
});

test( 'dgbsvx has expected arity', function t() {
	assert.strictEqual( dgbsvx.length, 29, 'has expected arity' );
});

test( 'dgbsvx throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dgbsvx( 2, 'invalid', new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgbsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgbsvx( 2, 'no-transpose', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dgbsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dgbsvx( 2, 'no-transpose', new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
