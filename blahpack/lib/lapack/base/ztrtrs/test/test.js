'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrtrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrtrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'ztrtrs: upper triangular, no transpose', function t() {
	var tc = findCase( 'upper_no_trans' );
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'U', 'N', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: lower triangular, no transpose', function t() {
	var tc = findCase( 'lower_no_trans' );
	var A = new Complex128Array([
		2, 1,  1, 2,  3, 0,
		0, 0,  4, 1,  5, 2,
		0, 0,  0, 0,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'L', 'N', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: upper triangular, transpose', function t() {
	var tc = findCase( 'upper_trans' );
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'U', 'T', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: upper triangular, conjugate transpose', function t() {
	var tc = findCase( 'upper_conj_trans' );
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'U', 'C', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: upper triangular, unit diagonal', function t() {
	var tc = findCase( 'upper_unit_diag' );
	var A = new Complex128Array([
		99, 99,  0, 0,  0, 0,
		2, 1,  99, 99,  0, 0,
		3, 0,  4, 2,  99, 99
	]);
	var B = new Complex128Array([
		10, 5,  5, 3,  1, 1
	]);
	var info = ztrtrs( 'U', 'N', 'U', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info = ztrtrs( 'U', 'N', 'N', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: singular diagonal (element 2)', function t() {
	var tc = findCase( 'singular' );
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  0, 0,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'U', 'N', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: multiple right-hand sides (NRHS=2)', function t() {
	var tc = findCase( 'multi_rhs' );
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2,
		4, 1,  5, 2,  6, 3
	]);
	var info = ztrtrs( 'U', 'N', 'N', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: lower triangular, transpose', function t() {
	var tc = findCase( 'lower_trans' );
	var A = new Complex128Array([
		2, 1,  1, 2,  3, 0,
		0, 0,  4, 1,  5, 2,
		0, 0,  0, 0,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'L', 'T', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: lower triangular, conjugate transpose', function t() {
	var tc = findCase( 'lower_conj_trans' );
	var A = new Complex128Array([
		2, 1,  1, 2,  3, 0,
		0, 0,  4, 1,  5, 2,
		0, 0,  0, 0,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'L', 'C', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: singular at first diagonal element', function t() {
	var tc = findCase( 'singular_first' );
	var A = new Complex128Array([
		0, 0,  0, 0,  0, 0,
		1, 0,  2, 0,  0, 0,
		1, 0,  1, 0,  3, 0
	]);
	var B = new Complex128Array([
		1, 0,  2, 0,  3, 0
	]);
	var info = ztrtrs( 'U', 'N', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: singular at last diagonal element', function t() {
	var tc = findCase( 'singular_last' );
	var A = new Complex128Array([
		2, 0,  0, 0,  0, 0,
		1, 0,  3, 0,  0, 0,
		1, 0,  1, 0,  0, 0
	]);
	var B = new Complex128Array([
		1, 0,  2, 0,  3, 0
	]);
	var info = ztrtrs( 'U', 'N', 'N', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: NRHS=0 (no right-hand sides)', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Complex128Array([ 2, 1 ]);
	var B = new Complex128Array( 1 );
	var info = ztrtrs( 'U', 'N', 'N', 1, 0, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});
