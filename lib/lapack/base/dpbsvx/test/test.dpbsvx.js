/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbsvx = require( './../lib/dpbsvx.js' );


// TESTS //

test( 'dpbsvx is a function', function t() {
	assert.strictEqual( typeof dpbsvx, 'function', 'is a function' );
});

test( 'dpbsvx has expected arity', function t() {
	assert.strictEqual( dpbsvx.length, 25, 'has expected arity' );
});

test( 'dpbsvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpbsvx( 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dpbsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpbsvx( 2, 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dpbsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dpbsvx( 2, 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
