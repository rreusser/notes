/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zla_herpvgrw = require( './../lib/zla_herpvgrw.js' );


// TESTS //

test( 'zla_herpvgrw is a function', function t() {
	assert.strictEqual( typeof zla_herpvgrw, 'function', 'is a function' );
});

test( 'zla_herpvgrw has expected arity', function t() {
	assert.strictEqual( zla_herpvgrw.length, 9, 'has expected arity' );
});

test( 'zla_herpvgrw throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zla_herpvgrw( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zla_herpvgrw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_herpvgrw( 'upper', -1, 2, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Int32Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zla_herpvgrw throws RangeError for LDA < max(1,N)', function t() {
	assert.throws( function throws() {
		zla_herpvgrw( 'upper', 2, 0, new Complex128Array( 4 ), 1, new Complex128Array( 4 ), 2, new Int32Array( 2 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zla_herpvgrw throws RangeError for LDAF < max(1,N)', function t() {
	assert.throws( function throws() {
		zla_herpvgrw( 'upper', 2, 0, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 1, new Int32Array( 2 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zla_herpvgrw computes for valid 2x2 lower', function t() {
	var Aflat = new Float64Array( [ 1, 0, 0.5, 0, 0, 0, 1, 0 ] );
	var AFflat = new Float64Array( [ 1, 0, 0.5, 0, 0, 0, 0.75, 0 ] );
	var A = new Complex128Array( Aflat.buffer );
	var AF = new Complex128Array( AFflat.buffer );
	var IPIV = new Int32Array( [ 0, 1 ] );
	var WORK = new Float64Array( 4 );
	var r = zla_herpvgrw( 'lower', 2, 0, A, 2, AF, 2, IPIV, WORK );
	assert.strictEqual( typeof r, 'number' );
});
