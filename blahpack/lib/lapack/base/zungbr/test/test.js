

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zungbr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zungbr.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zungbr: vect_q_m_ge_k', function t() {
	var tc = findCase( 'vect_q_m_ge_k' );
	// TODO: set up inputs and call zungbr(...)
	// assertArrayClose( result, tc.a, 1e-14, 'a' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zungbr: vect_p_k_lt_n', function t() {
	var tc = findCase( 'vect_p_k_lt_n' );
	// TODO: set up inputs and call zungbr(...)
	// assertArrayClose( result, tc.a, 1e-14, 'a' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zungbr: vect_q_m_lt_k', function t() {
	var tc = findCase( 'vect_q_m_lt_k' );
	// TODO: set up inputs and call zungbr(...)
	// assertArrayClose( result, tc.a, 1e-14, 'a' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zungbr: vect_p_k_ge_n', function t() {
	var tc = findCase( 'vect_p_k_ge_n' );
	// TODO: set up inputs and call zungbr(...)
	// assertArrayClose( result, tc.a, 1e-14, 'a' );
	// assertClose( result, tc.info, 1e-14, 'info' );
});

test( 'zungbr: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	// TODO: set up inputs and call zungbr(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
});

