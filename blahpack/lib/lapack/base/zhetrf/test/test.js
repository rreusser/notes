

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zhetrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhetrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zhetrf: upper_4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	// TODO: set up inputs and call zhetrf(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertClose( result, tc.n, 1e-14, 'n' );
});

test( 'zhetrf: lower_4x4', function t() {
	var tc = findCase( 'lower_4x4' );
	// TODO: set up inputs and call zhetrf(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertClose( result, tc.n, 1e-14, 'n' );
});

test( 'zhetrf: n0', function t() {
	var tc = findCase( 'n0' );
	// TODO: set up inputs and call zhetrf(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zhetrf: lower_6x6', function t() {
	var tc = findCase( 'lower_6x6' );
	// TODO: set up inputs and call zhetrf(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertClose( result, tc.n, 1e-14, 'n' );
});

