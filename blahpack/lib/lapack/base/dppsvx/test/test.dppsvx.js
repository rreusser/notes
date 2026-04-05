/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dppsvx = require( './../lib/dppsvx.js' );


// TESTS //

test( 'dppsvx is a function', function t() {
	assert.strictEqual( typeof dppsvx, 'function', 'is a function' );
});

test( 'dppsvx has expected arity', function t() {
	assert.strictEqual( dppsvx.length, 22, 'has expected arity' );
});

test( 'dppsvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dppsvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dppsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dppsvx( 2, 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dppsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dppsvx( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
