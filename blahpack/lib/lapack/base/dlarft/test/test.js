

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarft = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarft.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlarft: fwd_col_5x3', function t() {
	var tc = findCase( 'fwd_col_5x3' );
	// TODO: set up inputs and call dlarft(...)
	// assertArrayClose( result, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd_col_3x2', function t() {
	var tc = findCase( 'fwd_col_3x2' );
	// TODO: set up inputs and call dlarft(...)
	// assertArrayClose( result, tc.T, 1e-14, 'T' );
});

test( 'dlarft: bwd_col_5x2', function t() {
	var tc = findCase( 'bwd_col_5x2' );
	// TODO: set up inputs and call dlarft(...)
	// assertArrayClose( result, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd_col_tau_zero', function t() {
	var tc = findCase( 'fwd_col_tau_zero' );
	// TODO: set up inputs and call dlarft(...)
	// assertArrayClose( result, tc.T, 1e-14, 'T' );
});

