'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarfgp = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarfgp.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zlarfgp.ndarray: basic real positive alpha', function t() {
	var tc = findCase( 'zlarfgp_basic_real_pos' );
	var alpha = new Complex128Array( [ 3.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: basic real negative alpha', function t() {
	var tc = findCase( 'zlarfgp_basic_real_neg' );
	var alpha = new Complex128Array( [ -3.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: complex alpha and x', function t() {
	var tc = findCase( 'zlarfgp_complex' );
	var alpha = new Complex128Array( [ 2.0, 1.0 ] );
	var x = new Complex128Array( [ 1.0, -1.0, 0.5, 0.5 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: n=1 complex alpha', function t() {
	var tc = findCase( 'zlarfgp_n_one_complex' );
	var alpha = new Complex128Array( [ 5.0, 3.0 ] );
	var x = new Complex128Array( 0 );
	var tau = new Complex128Array( 1 );

	zlarfgp( 1, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
});

test( 'zlarfgp.ndarray: n=1 negative real alpha', function t() {
	var tc = findCase( 'zlarfgp_n_one_neg_real' );
	var alpha = new Complex128Array( [ -5.0, 0.0 ] );
	var x = new Complex128Array( 0 );
	var tau = new Complex128Array( 1 );

	zlarfgp( 1, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
});

test( 'zlarfgp.ndarray: n=1 positive real alpha', function t() {
	var tc = findCase( 'zlarfgp_n_one_pos_real' );
	var alpha = new Complex128Array( [ 5.0, 0.0 ] );
	var x = new Complex128Array( 0 );
	var tau = new Complex128Array( 1 );

	zlarfgp( 1, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
});

test( 'zlarfgp.ndarray: n=0 quick return', function t() {
	var alpha = new Complex128Array( [ 5.0, 3.0 ] );
	var tau = new Complex128Array( [ 99.0, 99.0 ] );

	zlarfgp( 0, alpha, 0, new Complex128Array( 0 ), 1, 0, tau, 0 );
	var tauv = reinterpret( tau, 0 );
	assert.strictEqual( tauv[ 0 ], 0.0 );
	assert.strictEqual( tauv[ 1 ], 0.0 );
});

test( 'zlarfgp.ndarray: x=0, alpha real positive => tau=0', function t() {
	var tc = findCase( 'zlarfgp_x_zero_alpha_real_pos' );
	var alpha = new Complex128Array( [ 4.0, 0.0 ] );
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: x=0, alpha real negative => tau=2', function t() {
	var tc = findCase( 'zlarfgp_x_zero_alpha_real_neg' );
	var alpha = new Complex128Array( [ -4.0, 0.0 ] );
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: x=0, alpha complex', function t() {
	var tc = findCase( 'zlarfgp_x_zero_alpha_complex' );
	var alpha = new Complex128Array( [ 4.0, 3.0 ] );
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: stride=2', function t() {
	var tc = findCase( 'zlarfgp_stride2' );
	var alpha = new Complex128Array( [ 2.0, -1.0 ] );
	var x = new Complex128Array( [ 1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 2, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: larger case n=5', function t() {
	var tc = findCase( 'zlarfgp_larger' );
	var alpha = new Complex128Array( [ 1.0, 1.0 ] );
	var x = new Complex128Array( [ 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 5, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: negative real alpha with real x', function t() {
	var tc = findCase( 'zlarfgp_neg_real_alpha_real_x' );
	var alpha = new Complex128Array( [ -2.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 5, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: very small values trigger rescaling loop', function t() {
	var tc = findCase( 'zlarfgp_rescaling' );
	var alpha = new Complex128Array( [ 1e-310, 0.0 ] );
	var x = new Complex128Array( [ 1e-310, 0.0, 1e-310, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-10, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
});

test( 'zlarfgp.ndarray: offset for alpha/tau/x', function t() {
	var alpha = new Complex128Array( [ 99.0, 99.0, 3.0, 0.0 ] );
	var x = new Complex128Array( [ 99.0, 99.0, 1.0, 0.0, 2.0, 0.0 ] );
	var tau = new Complex128Array( [ 99.0, 99.0, 0.0, 0.0 ] );

	zlarfgp( 3, alpha, 1, x, 1, 1, tau, 1 );
	var av = reinterpret( alpha, 0 );
	var tauv = reinterpret( tau, 0 );
	// beta = sqrt(9+1+4) = sqrt(14) = 3.74165...
	assertClose( av[ 2 ], 3.74165738677394089, 1e-13, 'alpha[1].re' );
	assertClose( av[ 3 ], 0.0, 1e-13, 'alpha[1].im' );
	assertClose( tauv[ 2 ], 1.98216274262726921e-1, 1e-13, 'tau[1].re' );
	// preserved:
	assert.strictEqual( av[ 0 ], 99.0 );
	assert.strictEqual( tauv[ 0 ], 99.0 );
});

test( 'zlarfgp.ndarray: throws for negative N', function t() {
	assert.throws( function throws() {
		zlarfgp( -1, new Complex128Array( 1 ), 0, new Complex128Array( 0 ), 1, 0, new Complex128Array( 1 ), 0 );
	}, RangeError );
});
