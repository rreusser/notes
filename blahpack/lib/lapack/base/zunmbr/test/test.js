

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zunmbr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zunmbr.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zunmbr: q_left_notrans', function t() {
	var tc = findCase( 'q_left_notrans' );
	// TODO: set up inputs and call zunmbr(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmbr: q_left_conjtrans', function t() {
	var tc = findCase( 'q_left_conjtrans' );
	// TODO: set up inputs and call zunmbr(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmbr: p_right_notrans', function t() {
	var tc = findCase( 'p_right_notrans' );
	// TODO: set up inputs and call zunmbr(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmbr: p_right_conjtrans', function t() {
	var tc = findCase( 'p_right_conjtrans' );
	// TODO: set up inputs and call zunmbr(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmbr: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call zunmbr(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmbr: q_right_notrans', function t() {
	var tc = findCase( 'q_right_notrans' );
	// TODO: set up inputs and call zunmbr(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmbr: p_left_conjtrans', function t() {
	var tc = findCase( 'p_left_conjtrans' );
	// TODO: set up inputs and call zunmbr(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

