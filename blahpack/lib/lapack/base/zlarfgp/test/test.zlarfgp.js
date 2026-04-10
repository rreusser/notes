'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarfgp = require( './../lib/zlarfgp.js' );


// TESTS //

test( 'zlarfgp is a function', function t() {
	assert.strictEqual( typeof zlarfgp, 'function', 'is a function' );
} );

test( 'zlarfgp: main wrapper computes beta (positive strideX)', function t() {
	var alpha = new Complex128Array( [ 3.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, tau, 0 );
	var av = reinterpret( alpha, 0 );
	// beta = sqrt(9+1+4) = sqrt(14)
	assert.ok( Math.abs( av[ 0 ] - 3.74165738677394089 ) < 1e-13 );
	assert.strictEqual( av[ 1 ], 0.0 );
} );

test( 'zlarfgp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarfgp( -1, new Complex128Array( 1 ), 0, new Complex128Array( 0 ), 1, new Complex128Array( 1 ), 0 );
	}, RangeError );
} );
