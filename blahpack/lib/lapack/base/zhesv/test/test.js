

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zhesv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhesv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zhesv: upper_4x4_1rhs', function t() {
	var tc = findCase( 'upper_4x4_1rhs' );
	// TODO: set up inputs and call zhesv(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertClose( result, tc.n, 1e-14, 'n' );
	// assertClose( result, tc.nrhs, 1e-14, 'nrhs' );
});

test( 'zhesv: lower_4x4_2rhs', function t() {
	var tc = findCase( 'lower_4x4_2rhs' );
	// TODO: set up inputs and call zhesv(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertClose( result, tc.n, 1e-14, 'n' );
	// assertClose( result, tc.nrhs, 1e-14, 'nrhs' );
});

test( 'zhesv: n0', function t() {
	var tc = findCase( 'n0' );
	// TODO: set up inputs and call zhesv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zhesv: n1', function t() {
	var tc = findCase( 'n1' );
	// TODO: set up inputs and call zhesv(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

