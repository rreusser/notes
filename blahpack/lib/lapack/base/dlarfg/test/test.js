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
