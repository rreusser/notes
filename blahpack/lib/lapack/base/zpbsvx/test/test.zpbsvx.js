/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbsvx = require( './../lib/zpbsvx.js' );


// TESTS //

test( 'zpbsvx is a function', function t() {
	assert.strictEqual( typeof zpbsvx, 'function', 'is a function' );
});

test( 'zpbsvx has expected arity', function t() {
	assert.strictEqual( zpbsvx.length, 25, 'has expected arity' );
});

test( 'zpbsvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbsvx( 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zpbsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbsvx( 2, 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zpbsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpbsvx( 2, 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
