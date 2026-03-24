

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zunmr2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zunmr2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zunmr2: left_notrans_3x2', function t() {
	var tc = findCase( 'left_notrans_3x2' );
	// TODO: set up inputs and call zunmr2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmr2: left_conjtrans_3x2', function t() {
	var tc = findCase( 'left_conjtrans_3x2' );
	// TODO: set up inputs and call zunmr2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmr2: right_notrans_2x3', function t() {
	var tc = findCase( 'right_notrans_2x3' );
	// TODO: set up inputs and call zunmr2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmr2: right_conjtrans_2x3', function t() {
	var tc = findCase( 'right_conjtrans_2x3' );
	// TODO: set up inputs and call zunmr2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmr2: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call zunmr2(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmr2: single_reflector', function t() {
	var tc = findCase( 'single_reflector' );
	// TODO: set up inputs and call zunmr2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

