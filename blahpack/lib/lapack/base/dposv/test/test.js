'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dposv = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dposv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dposv: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dposv( 'upper', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var A = new Float64Array( [ 1, 2, 3, 2, 1, 4, 3, 4, 1 ] );
	var B = new Float64Array( [ 1, 1, 1 ] );
	var info = dposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dposv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dposv( 'lower', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dposv: identity', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	var B = new Float64Array( [ 3, 5, 7 ] );
	var info = dposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var B = new Float64Array( [ 1, 0, 0, 0, 1, 0 ] );
	var info = dposv( 'lower', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: nrhs_zero', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Float64Array( 9 );
	var B = new Float64Array( 3 );
	var info = dposv( 'lower', 3, 0, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

// ndarray validation tests

test( 'dposv: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function() {
		ndarray( 'invalid', 3, 1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 );
	}, TypeError );
});

test( 'dposv: ndarray throws RangeError for negative N', function t() {
	assert.throws( function() {
		ndarray( 'upper', -1, 1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 );
	}, RangeError );
});

test( 'dposv: ndarray throws RangeError for negative NRHS', function t() {
	assert.throws( function() {
		ndarray( 'upper', 3, -1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 );
	}, RangeError );
});
