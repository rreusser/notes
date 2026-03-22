

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarfg = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarfg.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlarfg: basic', function t() {
	var tc = findCase( 'basic' );
	// TODO: set up inputs and call dlarfg(...)
	// assertClose( result, tc.alpha, 1e-14, 'alpha' );
	// assertClose( result, tc.tau, 1e-14, 'tau' );
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	// TODO: set up inputs and call dlarfg(...)
	// assertClose( result, tc.alpha, 1e-14, 'alpha' );
	// assertClose( result, tc.tau, 1e-14, 'tau' );
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: n_one', function t() {
	var tc = findCase( 'n_one' );
	// TODO: set up inputs and call dlarfg(...)
	// assertClose( result, tc.alpha, 1e-14, 'alpha' );
	// assertClose( result, tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: x_all_zero', function t() {
	var tc = findCase( 'x_all_zero' );
	// TODO: set up inputs and call dlarfg(...)
	// assertClose( result, tc.alpha, 1e-14, 'alpha' );
	// assertClose( result, tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: negative_alpha', function t() {
	var tc = findCase( 'negative_alpha' );
	// TODO: set up inputs and call dlarfg(...)
	// assertClose( result, tc.alpha, 1e-14, 'alpha' );
	// assertClose( result, tc.tau, 1e-14, 'tau' );
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call dlarfg(...)
	// assertClose( result, tc.alpha, 1e-14, 'alpha' );
	// assertClose( result, tc.tau, 1e-14, 'tau' );
});

