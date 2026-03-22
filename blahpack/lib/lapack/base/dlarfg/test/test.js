'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarfg = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarfg.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	if ( relErr > tol ) {
		throw new Error( msg + ': expected ' + expected + ', got ' + actual );
	}
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

test( 'dlarfg: basic', function t() {
	var tc = findCase( 'basic' );
	var alpha = new Float64Array( [ 3.0 ] );
	var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );
	var tau = new Float64Array( 1 );
	dlarfg( 4, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: alpha=0', function t() {
	var tc = findCase( 'alpha_zero' );
	var alpha = new Float64Array( [ 0.0 ] );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	var tau = new Float64Array( 1 );
	dlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: n=1 (tau=0)', function t() {
	var tc = findCase( 'n_one' );
	var alpha = new Float64Array( [ 5.0 ] );
	var x = new Float64Array( 1 );
	var tau = new Float64Array( 1 );
	dlarfg( 1, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: x all zero (tau=0)', function t() {
	var tc = findCase( 'x_all_zero' );
	var alpha = new Float64Array( [ 5.0 ] );
	var x = new Float64Array( [ 0.0, 0.0 ] );
	var tau = new Float64Array( 1 );
	dlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: negative alpha', function t() {
	var tc = findCase( 'negative_alpha' );
	var alpha = new Float64Array( [ -3.0 ] );
	var x = new Float64Array( [ 4.0 ] );
	var tau = new Float64Array( 1 );
	dlarfg( 2, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: n=0 (tau=0)', function t() {
	var tc = findCase( 'n_zero' );
	var alpha = new Float64Array( [ 5.0 ] );
	var x = new Float64Array( 1 );
	var tau = new Float64Array( 1 );
	dlarfg( 0, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: tiny inputs (sfmin scaling loop)', function t() {
	// Values small enough that |beta| = dlapy2(alpha, xnorm) < safmin (~2e-292),
	// triggering the sfmin scaling loop (lines 57-69) and beta rescale (lines 76-77).
	var alpha = new Float64Array( [ 1e-300 ] );
	var x = new Float64Array( [ 1e-300, 1e-300 ] );
	var tau = new Float64Array( 1 );
	dlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );

	// Verify the reflector properties:
	// 1. tau should be between 1 and 2 for a valid non-trivial reflector
	if ( tau[ 0 ] <= 0.0 || tau[ 0 ] > 2.0 ) {
		throw new Error( 'tau out of range: ' + tau[ 0 ] );
	}

	// 2. alpha (beta) should be negative (since original alpha > 0) and non-zero
	if ( alpha[ 0 ] >= 0.0 ) {
		throw new Error( 'expected negative beta, got: ' + alpha[ 0 ] );
	}

	// 3. Verify H * [alpha; x] = [beta; 0] by checking the reflector identity:
	//    tau * (1 + v^T * v) should be close to 2 * (1 - alpha_orig / beta) ...
	//    Simpler check: |beta| should equal norm([alpha_orig, x_orig])
	var origNorm = Math.sqrt( 1e-300*1e-300 + 1e-300*1e-300 + 1e-300*1e-300 );
	assertClose( Math.abs( alpha[ 0 ] ), origNorm, 1e-10, 'beta magnitude' );
});

test( 'dlarfg: tiny negative alpha (sfmin scaling loop)', function t() {
	// Tiny negative alpha to also cover the negative sign path in the scaling branch
	var alpha = new Float64Array( [ -2e-300 ] );
	var x = new Float64Array( [ 1e-300 ] );
	var tau = new Float64Array( 1 );
	dlarfg( 2, alpha, 0, x, 1, 0, tau, 0 );

	// tau should be valid
	if ( tau[ 0 ] <= 0.0 || tau[ 0 ] > 2.0 ) {
		throw new Error( 'tau out of range: ' + tau[ 0 ] );
	}

	// beta should be positive (since alpha is negative, sign flips)
	if ( alpha[ 0 ] <= 0.0 ) {
		throw new Error( 'expected positive beta for negative alpha, got: ' + alpha[ 0 ] );
	}

	var origNorm = Math.sqrt( 4e-600 + 1e-600 );
	assertClose( Math.abs( alpha[ 0 ] ), origNorm, 1e-10, 'beta magnitude' );
});
