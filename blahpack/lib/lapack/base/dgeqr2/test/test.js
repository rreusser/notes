

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgeqr2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqr2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgeqr2: 3x2', function t() {
	var tc = findCase( '3x2' );
	// TODO: set up inputs and call dgeqr2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.INFO, 1e-14, 'INFO' );
	// assertArrayClose( result, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqr2: 2x2', function t() {
	var tc = findCase( '2x2' );
	// TODO: set up inputs and call dgeqr2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.INFO, 1e-14, 'INFO' );
	// assertArrayClose( result, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqr2: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call dgeqr2(...)
	// assertClose( result, tc.INFO, 1e-14, 'INFO' );
});

test( 'dgeqr2: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call dgeqr2(...)
	// assertClose( result, tc.INFO, 1e-14, 'INFO' );
});

test( 'dgeqr2: 4x3', function t() {
	var tc = findCase( '4x3' );
	// TODO: set up inputs and call dgeqr2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.INFO, 1e-14, 'INFO' );
	// assertArrayClose( result, tc.TAU, 1e-14, 'TAU' );
});

