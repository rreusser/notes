/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpsvx = require( './../lib/zhpsvx.js' );


// TESTS //

test( 'zhpsvx is a function', function t() {
	assert.strictEqual( typeof zhpsvx, 'function', 'is a function' );
});

test( 'zhpsvx has expected arity', function t() {
	assert.strictEqual( zhpsvx.length, 21, 'has expected arity' );
});

test( 'zhpsvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpsvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhpsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpsvx( 2, 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zhpsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zhpsvx( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
