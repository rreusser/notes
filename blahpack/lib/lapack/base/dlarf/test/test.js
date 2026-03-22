

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlarf: left_3x3', function t() {
	var tc = findCase( 'left_3x3' );
	// TODO: set up inputs and call dlarf(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dlarf: right_3x3', function t() {
	var tc = findCase( 'right_3x3' );
	// TODO: set up inputs and call dlarf(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dlarf: tau_zero', function t() {
	var tc = findCase( 'tau_zero' );
	// TODO: set up inputs and call dlarf(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dlarf: left_2x3', function t() {
	var tc = findCase( 'left_2x3' );
	// TODO: set up inputs and call dlarf(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

