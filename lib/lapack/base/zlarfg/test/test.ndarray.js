'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarfg = require( './../lib/base.js' );

// Fixtures
var zlarfgBasicReal = require( './fixtures/zlarfg_basic_real.json' );
var zlarfgComplex = require( './fixtures/zlarfg_complex.json' );
var zlarfgNOne = require( './fixtures/zlarfg_n_one.json' );
var zlarfgXZeroAlphaComplex = require( './fixtures/zlarfg_x_zero_alpha_complex.json' );
var zlarfgStride2 = require( './fixtures/zlarfg_stride2.json' );
var zlarfgLarger = require( './fixtures/zlarfg_larger.json' );
var zlarfgRescaling = require( './fixtures/zlarfg_rescaling.json' );

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1e-30 );
	assert.ok( relErr <= 1e-12, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, label ) {
	var i;
	assert.strictEqual( actual.length, expected.length, label + ' length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], label + '[' + i + ']' );
	}
}

test( 'zlarfg: basic real case', function t() {
	var tc = zlarfgBasicReal;
	var alpha = new Complex128Array( [ 3.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'zlarfg: complex case', function t() {
	var tc = zlarfgComplex;
	var alpha = new Complex128Array( [ 2.0, 1.0 ] );
	var x = new Complex128Array( [ 1.0, -1.0, 0.5, 0.5 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'zlarfg: n=1 (no x vector)', function t() {
	var tc = zlarfgNOne;
	var alpha = new Complex128Array( [ 5.0, 3.0 ] );
	var x = new Complex128Array( 0 );
	var tau = new Complex128Array( 1 );

	zlarfg( 1, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
});

test( 'zlarfg: n=0 (quick return)', function t() {
	var alpha = new Complex128Array( [ 5.0, 3.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 0, alpha, 0, new Complex128Array( 0 ), 1, 0, tau, 0 );
	var tauv = reinterpret( tau, 0 );
	assert.strictEqual( tauv[ 0 ], 0.0 );
	assert.strictEqual( tauv[ 1 ], 0.0 );
});

test( 'zlarfg: x=0, alpha real => tau=0', function t() {
	var alpha = new Complex128Array( [ 4.0, 0.0 ] );
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	var tauv = reinterpret( tau, 0 );
	assert.strictEqual( tauv[ 0 ], 0.0 );
	assert.strictEqual( tauv[ 1 ], 0.0 );
});

test( 'zlarfg: x=0, alpha complex => non-trivial', function t() {
	var tc = zlarfgXZeroAlphaComplex;
	var alpha = new Complex128Array( [ 4.0, 3.0 ] );
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
});

test( 'zlarfg: stride=2', function t() {
	var tc = zlarfgStride2;
	var alpha = new Complex128Array( [ 2.0, -1.0 ] );
	var x = new Complex128Array( [ 1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 3, alpha, 0, x, 2, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'zlarfg: larger case n=5', function t() {
	var tc = zlarfgLarger;
	var alpha = new Complex128Array( [ 1.0, 1.0 ] );
	var x = new Complex128Array( [ 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 5, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'zlarfg: very small values trigger rescaling loop', function t() {
	var tc = zlarfgRescaling;
	var alpha = new Complex128Array( [ 1e-310, 0.0 ] );
	var x = new Complex128Array( [ 1e-310, 0.0, 1e-310, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});
