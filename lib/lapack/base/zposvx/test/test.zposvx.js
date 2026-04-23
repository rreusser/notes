/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zposvx = require( './../lib/zposvx.js' );


// TESTS //

test( 'zposvx is a function', function t() {
	assert.strictEqual( typeof zposvx, 'function', 'is a function' );
});

test( 'zposvx has expected arity', function t() {
	assert.strictEqual( zposvx.length, 24, 'has expected arity' );
});

test( 'zposvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zposvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zposvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zposvx( 2, 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zposvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zposvx( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
