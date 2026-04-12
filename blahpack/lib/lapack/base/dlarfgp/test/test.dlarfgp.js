/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfgp = require( './../lib/dlarfgp.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlarfgp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' ); // eslint-disable-line max-len
	}
}


// TESTS //

test( 'dlarfgp is a function', function t() {
	assert.strictEqual( typeof dlarfgp, 'function', 'is a function' );
} );

test( 'dlarfgp has expected arity', function t() {
	assert.strictEqual( dlarfgp.length, 7, 'has expected arity' );
} );

test( 'dlarfgp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarfgp( -1, new Float64Array( 1 ), 0, new Float64Array( 4 ), 1, new Float64Array( 1 ), 0 ); // eslint-disable-line max-len
	}, RangeError );
} );

test( 'dlarfgp: basic (matches fixture)', function t() {
	var tc = findCase( 'basic' );
	var alpha = new Float64Array( [ 3.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );

	dlarfgp( 4, alpha, 0, x, 1, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: alpha=0 (matches fixture)', function t() {
	var tc = findCase( 'alpha_zero' );
	var alpha = new Float64Array( [ 0.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 3.0, 4.0 ] );

	dlarfgp( 3, alpha, 0, x, 1, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: negative alpha (matches fixture)', function t() {
	var tc = findCase( 'negative_alpha' );
	var alpha = new Float64Array( [ -3.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 4.0 ] );

	dlarfgp( 2, alpha, 0, x, 1, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: stride2 (matches fixture)', function t() {
	var tc = findCase( 'stride2' );
	var alpha = new Float64Array( [ 2.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 1.0, 99.0, 2.0, 99.0, 2.0 ] );

	dlarfgp( 4, alpha, 0, x, 2, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.strictEqual( x[ 1 ], 99.0, 'sentinel preserved' );
	assert.strictEqual( x[ 3 ], 99.0, 'sentinel preserved' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: larger_n (matches fixture)', function t() {
	var tc = findCase( 'larger_n' );
	var alpha = new Float64Array( [ 1.5 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 0.5, -0.25, 0.75, -0.125, 0.0625 ] );

	dlarfgp( 6, alpha, 0, x, 1, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: N=0 sets tau=0', function t() {
	var tc = findCase( 'n_zero' );
	var alpha = new Float64Array( [ 5.0 ] );
	var x = new Float64Array( 1 );
	var tau = new Float64Array( [ 99.0 ] );

	dlarfgp( 0, alpha, 0, x, 1, tau, 0 );

	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assert.strictEqual( tau[ 0 ], 0.0 );
} );

test( 'dlarfgp: N=1 sets tau=0', function t() {
	var tc = findCase( 'n_one' );
	var alpha = new Float64Array( [ 5.0 ] );
	var x = new Float64Array( 1 );
	var tau = new Float64Array( 1 );

	dlarfgp( 1, alpha, 0, x, 1, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
} );
