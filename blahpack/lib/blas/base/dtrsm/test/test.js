

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrsm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrsm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtrsm: left_upper_n_n', function t() {
	var tc = findCase( 'left_upper_n_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: left_lower_n_n', function t() {
	var tc = findCase( 'left_lower_n_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: right_upper_n_n', function t() {
	var tc = findCase( 'right_upper_n_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: unit_diag', function t() {
	var tc = findCase( 'unit_diag' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: alpha_scale', function t() {
	var tc = findCase( 'alpha_scale' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: left_upper_t_n', function t() {
	var tc = findCase( 'left_upper_t_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: left_lower_t_n', function t() {
	var tc = findCase( 'left_lower_t_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: right_lower_n_n', function t() {
	var tc = findCase( 'right_lower_n_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: right_upper_t_n', function t() {
	var tc = findCase( 'right_upper_t_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: right_lower_t_n', function t() {
	var tc = findCase( 'right_lower_t_n' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'dtrsm: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call dtrsm(...)
	// assertArrayClose( result, tc.B, 1e-14, 'B' );
});

