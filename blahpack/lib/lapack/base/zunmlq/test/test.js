

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zunmlq = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zunmlq.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zunmlq: left_notrans_5x5', function t() {
	var tc = findCase( 'left_notrans_5x5' );
	// TODO: set up inputs and call zunmlq(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmlq: left_conjtrans_5x5', function t() {
	var tc = findCase( 'left_conjtrans_5x5' );
	// TODO: set up inputs and call zunmlq(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmlq: right_notrans_5x5', function t() {
	var tc = findCase( 'right_notrans_5x5' );
	// TODO: set up inputs and call zunmlq(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmlq: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call zunmlq(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmlq: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call zunmlq(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmlq: k_zero', function t() {
	var tc = findCase( 'k_zero' );
	// TODO: set up inputs and call zunmlq(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zunmlq: right_conjtrans_rect', function t() {
	var tc = findCase( 'right_conjtrans_rect' );
	// TODO: set up inputs and call zunmlq(...)
	// assertArrayClose( result, tc.c, 1e-14, 'c' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

