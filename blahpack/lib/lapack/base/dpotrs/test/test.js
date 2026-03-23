'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpotrf = require( './../../dpotrf/lib/base.js' );
var dpotrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpotrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpotrs: lower_single_rhs', function t() {
	var tc = findCase( 'lower_single_rhs' );
	// A = [4 2 1; 2 5 3; 1 3 9] col-major
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	dpotrf( 'lower', 3, A, 1, 3, 0 );
	var info = dpotrs( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: upper_single_rhs', function t() {
	var tc = findCase( 'upper_single_rhs' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	dpotrf( 'upper', 3, A, 1, 3, 0 );
	var info = dpotrs( 'upper', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: lower_multi_rhs', function t() {
	var tc = findCase( 'lower_multi_rhs' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 0, 0, 0, 1, 0 ] );
	dpotrf( 'lower', 3, A, 1, 3, 0 );
	var info = dpotrs( 'lower', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dpotrs( 'lower', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrs: nrhs_zero', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Float64Array( 9 );
	var B = new Float64Array( 3 );
	var info = dpotrs( 'lower', 3, 0, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrs: one_by_one', function t() {
	var tc = findCase( 'one_by_one' );
	// L = 2 (so A = L*L^T = 4)
	var A = new Float64Array( [ 2 ] );
	var B = new Float64Array( [ 6 ] );
	var info = dpotrs( 'lower', 1, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: upper_multi_rhs_3', function t() {
	var tc = findCase( 'upper_multi_rhs_3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dpotrf( 'upper', 3, A, 1, 3, 0 );
	var info = dpotrs( 'upper', 3, 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});
