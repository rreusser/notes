

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
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

// A = [10 2+i 1-2i; 2-i 8 3+i; 1+2i 3-i 7] Hermitian (upper stored)
var A_UPPER = [
	10, 0, 0, 0, 0, 0,
	2, 1, 8, 0, 0, 0,
	1, -2, 3, 1, 7, 0
];

// A (lower stored)
var A_LOWER = [
	10, 0, 2, -1, 1, 2,
	0, 0, 8, 0, 3, -1,
	0, 0, 0, 0, 7, 0
];

// B = [4 1+i 0; 1-i 5 2-i; 0 2+i 6] (upper stored)
var B_UPPER = [
	4, 0, 0, 0, 0, 0,
	1, 1, 5, 0, 0, 0,
	0, 0, 2, -1, 6, 0
];

// B (lower stored)
var B_LOWER = [
	4, 0, 1, -1, 0, 0,
	0, 0, 5, 0, 2, 1,
	0, 0, 0, 0, 6, 0
];

function callZhegv( itype, jobz, uplo, N, aData, bData ) {
	var RWORK = new Float64Array( Math.max( 1, 3 * N - 2 ) );
	var WORK = new Complex128Array( Math.max( 1, 2 * N ) );
	var lwork = Math.max( 1, 2 * N );
	var A = new Complex128Array( aData );
	var B = new Complex128Array( bData );
	var w = new Float64Array( N );
	var info = zhegv( itype, jobz, uplo, N, A, 1, N, 0, B, 1, N, 0, w, 1, 0, WORK, 1, 0, lwork, RWORK, 1, 0 );
	return { info: info, w: w, A: A };
}


// TESTS //

test( 'zhegv: itype1_v_upper', function t() {
	var tc = findCase( 'itype1_v_upper' );
	var r = callZhegv( 1, 'compute', 'upper', 3, A_UPPER, B_UPPER );
	assert.equal( r.info, tc.info );
	assertArrayClose( Array.from( r.w ), tc.w, 1e-12, 'w' );
});

test( 'zhegv: itype1_v_lower', function t() {
	var tc = findCase( 'itype1_v_lower' );
	var r = callZhegv( 1, 'compute', 'lower', 3, A_LOWER, B_LOWER );
	assert.equal( r.info, tc.info );
	assertArrayClose( Array.from( r.w ), tc.w, 1e-12, 'w' );
});

test( 'zhegv: itype1_n_lower', function t() {
	var tc = findCase( 'itype1_n_lower' );
	var r = callZhegv( 1, 'none', 'lower', 3, A_LOWER, B_LOWER );
	assert.equal( r.info, tc.info );
	assertArrayClose( Array.from( r.w ), tc.w, 1e-12, 'w' );
});

test( 'zhegv: itype2_v_upper', function t() {
	var tc = findCase( 'itype2_v_upper' );
	var r = callZhegv( 2, 'compute', 'upper', 3, A_UPPER, B_UPPER );
	assert.equal( r.info, tc.info );
	assertArrayClose( Array.from( r.w ), tc.w, 1e-12, 'w' );
});

test( 'zhegv: itype3_v_lower', function t() {
	var tc = findCase( 'itype3_v_lower' );
	var r = callZhegv( 3, 'compute', 'lower', 3, A_LOWER, B_LOWER );
	assert.equal( r.info, tc.info );
	assertArrayClose( Array.from( r.w ), tc.w, 1e-12, 'w' );
});

test( 'zhegv: n_zero', function t() {
	var WORK = new Complex128Array( 1 );
	var RWORK = new Float64Array( 1 );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var w = new Float64Array( 1 );
	var info = zhegv( 1, 'compute', 'upper', 0, A, 1, 1, 0, B, 1, 1, 0, w, 1, 0, WORK, 1, 0, 1, RWORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zhegv: n_one', function t() {
	var tc = findCase( 'n_one' );
	var r = callZhegv( 1, 'compute', 'upper', 1, [ 6, 0 ], [ 2, 0 ] );
	assert.equal( r.info, tc.info );
	assertArrayClose( Array.from( r.w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( reinterpret( r.A, 0 ) ), tc.A, 1e-12, 'A' );
});

test( 'zhegv: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var r = callZhegv( 1, 'compute', 'lower', 2, [ 1, 0, 0, 0, 0, 0, 1, 0 ], [ -1, 0, 0, 0, 0, 0, 1, 0 ] );
	assert.equal( r.info, tc.info );
});
