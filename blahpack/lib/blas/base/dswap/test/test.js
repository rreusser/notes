

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dswap = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dswap.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dswap: basic', function t() {
	var tc = findCase( 'basic' );
	// TODO: set up inputs and call dswap(...)
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

test( 'dswap: negative_stride', function t() {
	var tc = findCase( 'negative_stride' );
	// TODO: set up inputs and call dswap(...)
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

test( 'dswap: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call dswap(...)
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

test( 'dswap: n_one', function t() {
	var tc = findCase( 'n_one' );
	// TODO: set up inputs and call dswap(...)
	// assertArrayClose( result, tc.x, 1e-14, 'x' );
	// assertArrayClose( result, tc.y, 1e-14, 'y' );
});

