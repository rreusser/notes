/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfgp = require( './../lib/dlarfgp.js' );


// TESTS //

test( 'dlarfgp is a function', function t() {
	assert.strictEqual( typeof dlarfgp, 'function', 'is a function' );
});

test( 'dlarfgp has expected arity', function t() {
	assert.strictEqual( dlarfgp.length, 7, 'has expected arity' );
});

test( 'dlarfgp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarfgp( -1, new Float64Array( 1 ), 0, new Float64Array( 4 ), 1, new Float64Array( 1 ), 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dlarfgp computes a non-negative beta with positive stride', function t() {
	var alpha = new Float64Array( [ 3.0 ] );
	var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );
	var tau = new Float64Array( 1 );
	dlarfgp( 4, alpha, 0, x, 1, tau, 0 );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
	assert.ok( tau[ 0 ] >= 0.0 && tau[ 0 ] <= 2.0, 'tau in [0, 2]' );
});

test( 'dlarfgp N=0 sets tau=0', function t() {
	var alpha = new Float64Array( [ 3.0 ] );
	var x = new Float64Array( 1 );
	var tau = new Float64Array( [ 99.0 ] );
	dlarfgp( 0, alpha, 0, x, 1, tau, 0 );
	assert.strictEqual( tau[ 0 ], 0.0 );
});
