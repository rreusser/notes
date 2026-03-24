

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgesc2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgesc2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgesc2: basic_2x2', function t() {
	var tc = findCase( 'basic_2x2' );
	// TODO: set up inputs and call dgesc2(...)
	// assertArrayClose( result, tc.rhs, 1e-14, 'rhs' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dgesc2: basic_3x3', function t() {
	var tc = findCase( 'basic_3x3' );
	// TODO: set up inputs and call dgesc2(...)
	// assertArrayClose( result, tc.rhs, 1e-14, 'rhs' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dgesc2: basic_4x4', function t() {
	var tc = findCase( 'basic_4x4' );
	// TODO: set up inputs and call dgesc2(...)
	// assertArrayClose( result, tc.rhs, 1e-14, 'rhs' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dgesc2: n_equals_1', function t() {
	var tc = findCase( 'n_equals_1' );
	// TODO: set up inputs and call dgesc2(...)
	// assertArrayClose( result, tc.rhs, 1e-14, 'rhs' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

