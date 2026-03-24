

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgetc2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgetc2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgetc2: basic_2x2', function t() {
	var tc = findCase( 'basic_2x2' );
	// TODO: set up inputs and call dgetc2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertArrayClose( result, tc.jpiv, 1e-14, 'jpiv' );
});

test( 'dgetc2: basic_3x3', function t() {
	var tc = findCase( 'basic_3x3' );
	// TODO: set up inputs and call dgetc2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertArrayClose( result, tc.jpiv, 1e-14, 'jpiv' );
});

test( 'dgetc2: basic_4x4', function t() {
	var tc = findCase( 'basic_4x4' );
	// TODO: set up inputs and call dgetc2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertArrayClose( result, tc.jpiv, 1e-14, 'jpiv' );
});

test( 'dgetc2: n_equals_1', function t() {
	var tc = findCase( 'n_equals_1' );
	// TODO: set up inputs and call dgetc2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertArrayClose( result, tc.jpiv, 1e-14, 'jpiv' );
});

test( 'dgetc2: near_singular', function t() {
	var tc = findCase( 'near_singular' );
	// TODO: set up inputs and call dgetc2(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertArrayClose( result, tc.jpiv, 1e-14, 'jpiv' );
});

