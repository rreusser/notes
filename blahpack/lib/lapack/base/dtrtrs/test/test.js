'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrtrs = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrtrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtrtrs: upper_no_trans', function t() {
	var tc = findCase( 'upper_no_trans' );
	// A = [2 1 3; 0 4 5; 0 0 6] upper triangular, col-major
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dtrtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dtrtrs: lower_no_trans', function t() {
	var tc = findCase( 'lower_no_trans' );
	// L = [2 0 0; 1 4 0; 3 5 6] lower triangular, col-major
	var A = new Float64Array( [ 2, 1, 3, 0, 4, 5, 0, 0, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dtrtrs( 'lower', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dtrtrs: upper_trans', function t() {
	var tc = findCase( 'upper_trans' );
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dtrtrs( 'upper', 'transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dtrtrs: upper_unit_diag', function t() {
	var tc = findCase( 'upper_unit_diag' );
	// A = [1 2 3; 0 1 4; 0 0 1] unit diagonal, col-major
	var A = new Float64Array( [ 1, 0, 0, 2, 1, 0, 3, 4, 1 ] );
	var B = new Float64Array( [ 10, 5, 1 ] );
	var info = dtrtrs( 'upper', 'no-transpose', 'unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dtrtrs: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dtrtrs( 'upper', 'no-transpose', 'non-unit', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtrtrs: singular', function t() {
	var tc = findCase( 'singular' );
	// A with zero on diagonal at (1,1) (0-based)
	var A = new Float64Array( [ 2, 0, 0, 1, 0, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dtrtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dtrtrs: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	// B is 3x2 col-major
	var B = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var info = dtrtrs( 'upper', 'no-transpose', 'non-unit', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dtrtrs: lower_trans', function t() {
	var tc = findCase( 'lower_trans' );
	var A = new Float64Array( [ 2, 1, 3, 0, 4, 5, 0, 0, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var info = dtrtrs( 'lower', 'transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});


// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative NRHS', function t() {
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var B = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 3, -1, A, 1, 3, 0, B, 1, 3, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = ndarray( 'upper', 'no-transpose', 'non-unit', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
});
