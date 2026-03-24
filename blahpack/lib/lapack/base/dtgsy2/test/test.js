

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtgsy2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtgsy2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtgsy2: notrans_2x2_diag', function t() {
	var tc = findCase( 'notrans_2x2_diag' );
	// TODO: set up inputs and call dtgsy2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertArrayClose( result, tc.F, 1e-14, 'F' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.pq, 1e-14, 'pq' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtgsy2: notrans_3x2_quasi', function t() {
	var tc = findCase( 'notrans_3x2_quasi' );
	// TODO: set up inputs and call dtgsy2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertArrayClose( result, tc.F, 1e-14, 'F' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.pq, 1e-14, 'pq' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtgsy2: trans_2x2', function t() {
	var tc = findCase( 'trans_2x2' );
	// TODO: set up inputs and call dtgsy2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertArrayClose( result, tc.F, 1e-14, 'F' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtgsy2: notrans_2x3_bblock', function t() {
	var tc = findCase( 'notrans_2x3_bblock' );
	// TODO: set up inputs and call dtgsy2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertArrayClose( result, tc.F, 1e-14, 'F' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtgsy2: notrans_3x3_both_quasi', function t() {
	var tc = findCase( 'notrans_3x3_both_quasi' );
	// TODO: set up inputs and call dtgsy2(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertArrayClose( result, tc.F, 1e-14, 'F' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

