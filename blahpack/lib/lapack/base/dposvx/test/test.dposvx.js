/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dposvx = require( './../lib/dposvx.js' );


// TESTS //

test( 'dposvx is a function', function t() {
	assert.strictEqual( typeof dposvx, 'function', 'is a function' );
});

test( 'dposvx has expected arity', function t() {
	assert.strictEqual( dposvx.length, 23, 'has expected arity' );
});

test( 'dposvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dposvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dposvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dposvx( 2, 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dposvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dposvx( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
