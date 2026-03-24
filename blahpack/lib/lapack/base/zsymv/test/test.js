

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zsymv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zsymv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zsymv: upper_a1_b0', function t() {
	var tc = findCase( 'upper_a1_b0' );
	// TODO: set up inputs and call zsymv(...)
	// assertClose( result, tc.n, 1e-14, 'n' );
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

test( 'zsymv: lower_a2_b05', function t() {
	var tc = findCase( 'lower_a2_b05' );
	// TODO: set up inputs and call zsymv(...)
	// assertClose( result, tc.n, 1e-14, 'n' );
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

test( 'zsymv: n0', function t() {
	var tc = findCase( 'n0' );
	// TODO: set up inputs and call zsymv(...)
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

test( 'zsymv: a0_b1', function t() {
	var tc = findCase( 'a0_b1' );
	// TODO: set up inputs and call zsymv(...)
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

