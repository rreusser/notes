/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgesvx = require( './../lib/dgesvx.js' );


// TESTS //

test( 'dgesvx is a function', function t() {
	assert.strictEqual( typeof dgesvx, 'function', 'is a function' );
});

test( 'dgesvx has expected arity', function t() {
	assert.strictEqual( dgesvx.length, 23, 'has expected arity' );
});

test( 'dgesvx throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dgesvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgesvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgesvx( 2, 'no-transpose', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dgesvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dgesvx( 2, 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
