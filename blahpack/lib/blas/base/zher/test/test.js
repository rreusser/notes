

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zher = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zher.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zher: upper_basic', function t() {
	var tc = findCase( 'upper_basic' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: lower_basic', function t() {
	var tc = findCase( 'lower_basic' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: n_one', function t() {
	var tc = findCase( 'n_one' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: upper_stride', function t() {
	var tc = findCase( 'upper_stride' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: lower_stride', function t() {
	var tc = findCase( 'lower_stride' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: upper_alpha2', function t() {
	var tc = findCase( 'upper_alpha2' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: upper_zeros', function t() {
	var tc = findCase( 'upper_zeros' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: lower_zeros', function t() {
	var tc = findCase( 'lower_zeros' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

test( 'zher: negative_stride', function t() {
	var tc = findCase( 'negative_stride' );
	// TODO: set up inputs and call zher(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
});

