/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zheevx = require( './../lib/zheevx.js' );


// TESTS //

test( 'zheevx is a function', function t() {
	assert.strictEqual( typeof zheevx, 'function', 'is a function' );
});

test( 'zheevx has expected arity', function t() {
	assert.strictEqual( zheevx.length, 25, 'has expected arity' );
});

test( 'zheevx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zheevx( 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zheevx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zheevx( 2, 2, 'upper', -1, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
