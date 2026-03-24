'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zptts2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zptts2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zptts2: lower_n4_nrhs1 (IUPLO=0, N=4, NRHS=1)', function t() {
	var tc = findCase( 'lower_n4_nrhs1' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1, -0.3, 0.2, 0.4, -0.1 ] );
	var B = new Complex128Array( [ 2.0, 1.0, 3.0, -1.0, 1.0, 2.0, 4.0, 0.0 ] );
	var bv;

	zptts2( 0, 4, 1, d, 1, 0, e, 1, 0, B, 1, 4, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: upper_n4_nrhs1 (IUPLO=1, N=4, NRHS=1)', function t() {
	var tc = findCase( 'upper_n4_nrhs1' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1, -0.3, 0.2, 0.4, -0.1 ] );
	var B = new Complex128Array( [ 2.0, 1.0, 3.0, -1.0, 1.0, 2.0, 4.0, 0.0 ] );
	var bv;

	zptts2( 1, 4, 1, d, 1, 0, e, 1, 0, B, 1, 4, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: lower_n4_nrhs3 (IUPLO=0, N=4, NRHS=3, exercises NRHS>2 path)', function t() {
	var tc = findCase( 'lower_n4_nrhs3' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1, -0.3, 0.2, 0.4, -0.1 ] );
	var B = new Complex128Array( [
		2.0, 1.0, 3.0, -1.0, 1.0, 2.0, 4.0, 0.0,
		1.0, 0.0, 0.0, 1.0, -1.0, 0.0, 0.0, -1.0,
		5.0, 3.0, -2.0, 4.0, 3.0, -1.0, 1.0, 1.0
	] );
	var bv;

	zptts2( 0, 4, 3, d, 1, 0, e, 1, 0, B, 1, 4, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: upper_n4_nrhs3 (IUPLO=1, N=4, NRHS=3, exercises NRHS>2 path)', function t() {
	var tc = findCase( 'upper_n4_nrhs3' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1, -0.3, 0.2, 0.4, -0.1 ] );
	var B = new Complex128Array( [
		2.0, 1.0, 3.0, -1.0, 1.0, 2.0, 4.0, 0.0,
		1.0, 0.0, 0.0, 1.0, -1.0, 0.0, 0.0, -1.0,
		5.0, 3.0, -2.0, 4.0, 3.0, -1.0, 1.0, 1.0
	] );
	var bv;

	zptts2( 1, 4, 3, d, 1, 0, e, 1, 0, B, 1, 4, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: n_eq_1 (N=1, NRHS=1)', function t() {
	var tc = findCase( 'n_eq_1' );
	var d = new Float64Array( [ 3.0 ] );
	var e = new Complex128Array( 0 );
	var B = new Complex128Array( [ 9.0, 6.0 ] );
	var bv;

	zptts2( 0, 1, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: n_eq_1_multi_rhs (N=1, NRHS=2)', function t() {
	var tc = findCase( 'n_eq_1_multi_rhs' );
	var d = new Float64Array( [ 4.0 ] );
	var e = new Complex128Array( 0 );
	var B = new Complex128Array( [ 8.0, 4.0, 12.0, -8.0 ] );
	var bv;

	zptts2( 0, 1, 2, d, 1, 0, e, 1, 0, B, 1, 1, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: n_eq_0 (N=0, quick return)', function t() {
	var tc = findCase( 'n_eq_0' );
	var d = new Float64Array( [ 4.0 ] );
	var e = new Complex128Array( 0 );
	var B = new Complex128Array( [ 42.0, 7.0 ] );
	var bv;

	zptts2( 0, 0, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: lower_n4_nrhs2 (IUPLO=0, N=4, NRHS=2)', function t() {
	var tc = findCase( 'lower_n4_nrhs2' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1, -0.3, 0.2, 0.4, -0.1 ] );
	var B = new Complex128Array( [
		2.0, 1.0, 3.0, -1.0, 1.0, 2.0, 4.0, 0.0,
		1.0, 0.0, 0.0, 1.0, -1.0, 0.0, 0.0, -1.0
	] );
	var bv;

	zptts2( 0, 4, 2, d, 1, 0, e, 1, 0, B, 1, 4, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: upper_n4_nrhs2 (IUPLO=1, N=4, NRHS=2)', function t() {
	var tc = findCase( 'upper_n4_nrhs2' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1, -0.3, 0.2, 0.4, -0.1 ] );
	var B = new Complex128Array( [
		2.0, 1.0, 3.0, -1.0, 1.0, 2.0, 4.0, 0.0,
		1.0, 0.0, 0.0, 1.0, -1.0, 0.0, 0.0, -1.0
	] );
	var bv;

	zptts2( 1, 4, 2, d, 1, 0, e, 1, 0, B, 1, 4, 0 );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zptts2: returns B', function t() {
	var d = new Float64Array( [ 4.0, 3.0 ] );
	var e = new Complex128Array( [ 0.5, 0.1 ] );
	var B = new Complex128Array( [ 2.0, 1.0, 3.0, -1.0 ] );
	var result;

	result = zptts2( 0, 2, 1, d, 1, 0, e, 1, 0, B, 1, 2, 0 );
	assert.equal( result, B );
});
