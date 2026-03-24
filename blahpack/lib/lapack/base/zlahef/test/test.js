

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlahef = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlahef.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlahef: lower_6x6_nb3', function t() {
	var tc = findCase( 'lower_6x6_nb3' );
	// TODO: set up inputs and call zlahef(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.ipiv, 1e-14, 'ipiv' );
	// assertClose( result, tc.kb, 1e-14, 'kb' );
	// assertClose( result, tc.n, 1e-14, 'n' );
	// assertClose( result, tc.nb, 1e-14, 'nb' );
});

