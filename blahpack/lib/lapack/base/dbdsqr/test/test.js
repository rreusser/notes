

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dbdsqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dbdsqr.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Creates identity matrix of size n in column-major order.
*/
function eye( n ) {
	var A = new Float64Array( n * n );
	var i;
	for ( i = 0; i < n; i++ ) {
		A[ i + i * n ] = 1.0;
	}
	return A;
}

/**
* Extracts elements from a Float64Array into a plain array.
*/
function toArray( arr, len ) {
	var out = [];
	var i;
	for ( i = 0; i < len; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'dbdsqr: upper_4x4_values_only', function t() {
	var tc = findCase( 'upper_4x4_values_only' );
	var n = 4;
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 0, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
});

test( 'dbdsqr: upper_3x3_with_vt', function t() {
	var tc = findCase( 'upper_3x3_with_vt' );
	var n = 3;
	var d = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 0, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
});

test( 'dbdsqr: upper_3x3_with_vt_and_u', function t() {
	var tc = findCase( 'upper_3x3_with_vt_and_u' );
	var n = 3;
	var d = new Float64Array( [ 5.0, 3.0, 1.0 ] );
	var e = new Float64Array( [ 2.0, 1.0 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 3, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: lower_3x3_values_only', function t() {
	var tc = findCase( 'lower_3x3_values_only' );
	var n = 3;
	var d = new Float64Array( [ 4.0, 3.0, 2.0 ] );
	var e = new Float64Array( [ 1.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'L', n, 0, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
});

test( 'dbdsqr: lower_3x3_with_u', function t() {
	var tc = findCase( 'lower_3x3_with_u' );
	var n = 3;
	var d = new Float64Array( [ 4.0, 3.0, 2.0 ] );
	var e = new Float64Array( [ 1.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'L', n, 0, 3, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: n_1', function t() {
	var tc = findCase( 'n_1' );
	var d = new Float64Array( [ -5.0 ] );
	var e = new Float64Array( 1 );
	var work = new Float64Array( 10 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', 1, 0, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, 1 ), tc.d, 1e-14, 'd' );
});

test( 'dbdsqr: n_0', function t() {
	var tc = findCase( 'n_0' );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var work = new Float64Array( 10 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', 0, 0, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dbdsqr: upper_2x2_with_vectors', function t() {
	var tc = findCase( 'upper_2x2_with_vectors' );
	var n = 2;
	var d = new Float64Array( [ 3.0, 1.0 ] );
	var e = new Float64Array( [ 2.0 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 2 );
	var U = eye( 2 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 2, 2, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: n_1_neg_with_vt', function t() {
	var tc = findCase( 'n_1_neg_with_vt' );
	var d = new Float64Array( [ -3.0 ] );
	var e = new Float64Array( 1 );
	var work = new Float64Array( 10 );
	var VT = new Float64Array( [ 1.0, 3.0 ] );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	// ncvt=2, LDVT=1 => strideVT2=1 for row access
	var info = dbdsqr( 'U', 1, 2, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, 1 ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, 2 ), tc.vt, 1e-14, 'vt' );
});

test( 'dbdsqr: upper_3x3_with_c', function t() {
	var tc = findCase( 'upper_3x3_with_c' );
	var n = 3;
	var ncc = 2;
	var d = new Float64Array( [ 4.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1.0, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	// C is 3x2 column-major: C(1,:)=[1,2], C(2,:)=[0.5,0.25], C(3,:)=[1.5,2.5]
	var C = new Float64Array( [ 1.0, 0.5, 1.5, 2.0, 0.25, 2.5 ] );
	var info = dbdsqr( 'U', n, 0, 0, ncc, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, n, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( C, n * ncc ), tc.c, 1e-14, 'c' );
});

test( 'dbdsqr: upper_4x4_idir2', function t() {
	var tc = findCase( 'upper_4x4_idir2' );
	var n = 4;
	var d = new Float64Array( [ 0.5, 1.0, 2.0, 4.0 ] );
	var e = new Float64Array( [ 0.1, 0.1, 0.1 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 0, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
});

test( 'dbdsqr: upper_3x3_zero_shift', function t() {
	var tc = findCase( 'upper_3x3_zero_shift' );
	var n = 3;
	var d = new Float64Array( [ 1.0, 1e-15, 1.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 3, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: lower_3x3_with_c', function t() {
	var tc = findCase( 'lower_3x3_with_c' );
	var n = 3;
	var ncc = 2;
	var d = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
	var info = dbdsqr( 'L', n, 0, 0, ncc, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, n, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( C, n * ncc ), tc.c, 1e-14, 'c' );
});

test( 'dbdsqr: upper_3x3_idir2_with_vectors', function t() {
	var tc = findCase( 'upper_3x3_idir2_with_vectors' );
	var n = 3;
	var d = new Float64Array( [ 0.1, 0.5, 3.0 ] );
	var e = new Float64Array( [ 0.2, 0.3 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 3, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: upper_3x3_negative_d', function t() {
	var tc = findCase( 'upper_3x3_negative_d' );
	var n = 3;
	var d = new Float64Array( [ -3.0, 2.0, -1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 0, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
});

test( 'dbdsqr: nearly_diagonal', function t() {
	var tc = findCase( 'nearly_diagonal' );
	var n = 4;
	var d = new Float64Array( [ 5.0, 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1e-16, 1e-16, 1e-16 ] );
	var work = new Float64Array( 100 );
	var VT = new Float64Array( 1 );
	var U = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 0, 0, 0, d, 1, 0, e, 1, 0, VT, 1, 1, 0, U, 1, 1, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
});

test( 'dbdsqr: lower_3x3_with_vt_and_u', function t() {
	var tc = findCase( 'lower_3x3_with_vt_and_u' );
	var n = 3;
	var d = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'L', n, 3, 3, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: lower_3x3_all_vectors', function t() {
	var tc = findCase( 'lower_3x3_all_vectors' );
	var n = 3;
	var ncc = 2;
	var d = new Float64Array( [ 4.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1.0, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( [ 1.0, 0.5, 1.5, 2.0, 0.25, 2.5 ] );
	var info = dbdsqr( 'L', n, 3, 3, ncc, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, n, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
	assertArrayClose( toArray( C, n * ncc ), tc.c, 1e-14, 'c' );
});

test( 'dbdsqr: upper_3x3_zero_d', function t() {
	var tc = findCase( 'upper_3x3_zero_d' );
	var n = 3;
	var d = new Float64Array( [ 2.0, 0.0, 3.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 3, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});

test( 'dbdsqr: upper_3x3_zero_shift_all_vecs', function t() {
	var tc = findCase( 'upper_3x3_zero_shift_all_vecs' );
	var n = 3;
	var ncc = 2;
	var d = new Float64Array( [ 1e-15, 1.0, 1.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( [ 1.0, 0.5, 1.5, 2.0, 0.25, 2.5 ] );
	var info = dbdsqr( 'U', n, 3, 3, ncc, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, n, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
	assertArrayClose( toArray( C, n * ncc ), tc.c, 1e-14, 'c' );
});

test( 'dbdsqr: upper_4x4_idir2_all_vecs', function t() {
	var tc = findCase( 'upper_4x4_idir2_all_vecs' );
	var n = 4;
	var ncc = 2;
	var d = new Float64Array( [ 0.5, 1.0, 2.0, 4.0 ] );
	var e = new Float64Array( [ 0.1, 0.1, 0.1 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 4 );
	var U = eye( 4 );
	var C = new Float64Array( [ 1.0, 0.5, 1.5, 0.25, 2.0, 0.75, 2.5, 1.0 ] );
	var info = dbdsqr( 'U', n, 4, 4, ncc, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, n, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
	assertArrayClose( toArray( C, n * ncc ), tc.c, 1e-14, 'c' );
});

test( 'dbdsqr: upper_4x4_idir1_zero_shift_all_vecs', function t() {
	var tc = findCase( 'upper_4x4_idir1_zero_shift_all_vecs' );
	var n = 4;
	var ncc = 2;
	var d = new Float64Array( [ 10.0, 1e-15, 5.0, 1.0 ] );
	var e = new Float64Array( [ 0.1, 0.1, 0.1 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 4 );
	var U = eye( 4 );
	var C = new Float64Array( [ 1.0, 0.5, 1.5, 0.25, 2.0, 0.75, 2.5, 1.0 ] );
	var info = dbdsqr( 'U', n, 4, 4, ncc, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, n, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
	assertArrayClose( toArray( C, n * ncc ), tc.c, 1e-14, 'c' );
});

test( 'dbdsqr: upper_3x3_near_zero_shift', function t() {
	var tc = findCase( 'upper_3x3_near_zero_shift' );
	var n = 3;
	var d = new Float64Array( [ 1e8, 1.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var work = new Float64Array( 100 );
	var VT = eye( 3 );
	var U = eye( 3 );
	var C = new Float64Array( 1 );
	var info = dbdsqr( 'U', n, 3, 3, 0, d, 1, 0, e, 1, 0, VT, 1, n, 0, U, 1, n, 0, C, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, n ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( VT, n * n ), tc.vt, 1e-14, 'vt' );
	assertArrayClose( toArray( U, n * n ), tc.u, 1e-14, 'u' );
});
