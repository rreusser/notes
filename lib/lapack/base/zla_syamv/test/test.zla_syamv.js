/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_syamv = require( './../lib/zla_syamv.js' );


// TESTS //

test( 'zla_syamv is a function', function t() {
	assert.strictEqual( typeof zla_syamv, 'function', 'is a function' );
});

test( 'zla_syamv has expected arity', function t() {
	assert.strictEqual( zla_syamv.length, 11, 'has expected arity' );
});

test( 'zla_syamv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zla_syamv( 'invalid', 'upper', 2, 1.0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, 0.0, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'zla_syamv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zla_syamv( 'row-major', 'invalid', 2, 1.0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, 0.0, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'zla_syamv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_syamv( 'row-major', 'upper', -1, 1.0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, 0.0, new Float64Array( 2 ), 1 );
	}, RangeError );
});

test( 'zla_syamv throws RangeError for invalid LDA', function t() {
	assert.throws( function throws() {
		zla_syamv( 'row-major', 'upper', 3, 1.0, new Complex128Array( 9 ), 1, new Complex128Array( 3 ), 1, 0.0, new Float64Array( 3 ), 1 );
	}, RangeError );
});

test( 'zla_syamv throws RangeError for zero strideX', function t() {
	assert.throws( function throws() {
		zla_syamv( 'row-major', 'upper', 2, 1.0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 0, 0.0, new Float64Array( 2 ), 1 );
	}, RangeError );
});

test( 'zla_syamv throws RangeError for zero strideY', function t() {
	assert.throws( function throws() {
		zla_syamv( 'row-major', 'upper', 2, 1.0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, 0.0, new Float64Array( 2 ), 0 );
	}, RangeError );
});
