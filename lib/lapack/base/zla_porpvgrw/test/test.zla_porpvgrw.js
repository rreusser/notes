/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zla_porpvgrw = require( './../lib/zla_porpvgrw.js' );


// TESTS //

test( 'zla_porpvgrw is a function', function t() {
	assert.strictEqual( typeof zla_porpvgrw, 'function', 'is a function' );
});

test( 'zla_porpvgrw has expected arity', function t() {
	assert.strictEqual( zla_porpvgrw.length, 7, 'has expected arity' );
});

test( 'zla_porpvgrw throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zla_porpvgrw( 'invalid', 2, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zla_porpvgrw computes for valid 2x2', function t() {
	var Aflat = new Float64Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var AFflat = new Float64Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var A = new Complex128Array( Aflat.buffer );
	var AF = new Complex128Array( AFflat.buffer );
	var WORK = new Float64Array( 4 );
	var r = zla_porpvgrw( 'lower', 2, A, 2, AF, 2, WORK );
	assert.strictEqual( typeof r, 'number' );
});
