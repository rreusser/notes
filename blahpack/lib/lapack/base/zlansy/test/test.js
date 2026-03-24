

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlansy = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlansy.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlansy: max_upper', function t() {
	var tc = findCase( 'max_upper' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: one_upper', function t() {
	var tc = findCase( 'one_upper' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: inf_upper', function t() {
	var tc = findCase( 'inf_upper' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: fro_upper', function t() {
	var tc = findCase( 'fro_upper' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: one_lower', function t() {
	var tc = findCase( 'one_lower' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: fro_lower', function t() {
	var tc = findCase( 'fro_lower' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: n0', function t() {
	var tc = findCase( 'n0' );
	// TODO: set up inputs and call zlansy(...)
	// assertClose( result, tc.val, 1e-14, 'val' );
});

