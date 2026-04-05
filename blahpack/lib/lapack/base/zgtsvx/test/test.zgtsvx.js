/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgtsvx = require( './../lib/zgtsvx.js' );


// TESTS //

test( 'zgtsvx is a function', function t() {
	assert.strictEqual( typeof zgtsvx, 'function', 'is a function' );
});

test( 'zgtsvx has expected arity', function t() {
	assert.strictEqual( zgtsvx.length, 33, 'has expected arity' );
});

test( 'zgtsvx throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zgtsvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgtsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgtsvx( 2, 'no-transpose', -1, 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgtsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgtsvx( 2, 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
