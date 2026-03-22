

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgemm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgemm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgemm: basic_nn', function t() {
	var tc = findCase( 'basic_nn' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: tn', function t() {
	var tc = findCase( 'tn' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: nt', function t() {
	var tc = findCase( 'nt' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: alpha_beta', function t() {
	var tc = findCase( 'alpha_beta' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: nonsquare', function t() {
	var tc = findCase( 'nonsquare' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

test( 'dgemm: tt', function t() {
	var tc = findCase( 'tt' );
	// TODO: set up inputs and call dgemm(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
});

