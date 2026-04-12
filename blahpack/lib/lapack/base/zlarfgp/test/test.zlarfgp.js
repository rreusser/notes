'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarfgp = require( './../lib/zlarfgp.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarfgp.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	var rel;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		rel = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( rel <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}


// TESTS //

test( 'zlarfgp is a function', function t() {
	assert.strictEqual( typeof zlarfgp, 'function', 'is a function' );
} );

test( 'zlarfgp: basic real positive alpha matches fixture', function t() {
	var tc = findCase( 'zlarfgp_basic_real_pos' );
	var alpha = new Complex128Array( [ 3.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
} );

test( 'zlarfgp: complex alpha and x matches fixture', function t() {
	var tc = findCase( 'zlarfgp_complex' );
	var alpha = new Complex128Array( [ 2.0, 1.0 ] );
	var x = new Complex128Array( [ 1.0, -1.0, 0.5, 0.5 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 1, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
} );

test( 'zlarfgp: stride=2 with sentinel padding matches fixture', function t() {
	var tc = findCase( 'zlarfgp_stride2' );
	var alpha = new Complex128Array( [ 2.0, -1.0 ] );
	var x = new Complex128Array( [ 1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0 ] );
	var tau = new Complex128Array( 1 );

	zlarfgp( 3, alpha, 0, x, 2, tau, 0 );
	assertArrayClose( Array.from( reinterpret( alpha, 0 ) ), tc.alpha, 1e-13, 'alpha' );
	assertArrayClose( Array.from( reinterpret( tau, 0 ) ), tc.tau, 1e-13, 'tau' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-13, 'x' );
} );

test( 'zlarfgp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarfgp( -1, new Complex128Array( 1 ), 0, new Complex128Array( 0 ), 1, new Complex128Array( 1 ), 0 );
	}, RangeError );
} );
