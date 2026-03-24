

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zhegv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhegv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zhegv: itype1_v_upper', function t() {
	var tc = findCase( 'itype1_v_upper' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.w, 1e-14, 'w' );
});

test( 'zhegv: itype1_v_lower', function t() {
	var tc = findCase( 'itype1_v_lower' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.w, 1e-14, 'w' );
});

test( 'zhegv: itype1_n_lower', function t() {
	var tc = findCase( 'itype1_n_lower' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.w, 1e-14, 'w' );
});

test( 'zhegv: itype2_v_upper', function t() {
	var tc = findCase( 'itype2_v_upper' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.w, 1e-14, 'w' );
});

test( 'zhegv: itype3_v_lower', function t() {
	var tc = findCase( 'itype3_v_lower' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.w, 1e-14, 'w' );
});

test( 'zhegv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zhegv: n_one', function t() {
	var tc = findCase( 'n_one' );
	// TODO: set up inputs and call zhegv(...)
	// assertArrayClose( result, tc.A, 1e-14, 'A' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertArrayClose( result, tc.w, 1e-14, 'w' );
});

test( 'zhegv: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	// TODO: set up inputs and call zhegv(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

