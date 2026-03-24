

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpotrf = require( '../../zpotrf/lib/base.js' );
var zhegs2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhegs2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// B matrix (Hermitian positive definite):
// B = [4 1+i 0; 1-i 5 2-i; 0 2+i 6]
// Col-major interleaved re/im for 3x3:
var B_UPPER_DATA = [
	4, 0, 1, -1, 0, 0,   // col 1: B(1,1)=4, B(2,1)=1-i, B(3,1)=0
	1, 1, 5, 0, 2, 1,    // col 2: B(1,2)=1+i, B(2,2)=5, B(3,2)=2+i
	0, 0, 2, -1, 6, 0    // col 3: B(1,3)=0, B(2,3)=2-i, B(3,3)=6
];

// A matrix (Hermitian), upper stored:
// A = [10 2+i 1-2i; 2-i 8 3+i; 1+2i 3-i 7]
var A_UPPER_DATA = [
	10, 0, 0, 0, 0, 0,     // col 1
	2, 1, 8, 0, 0, 0,      // col 2
	1, -2, 3, 1, 7, 0      // col 3
];

// A matrix (Hermitian), lower stored:
var A_LOWER_DATA = [
	10, 0, 2, -1, 1, 2,    // col 1
	0, 0, 8, 0, 3, -1,     // col 2
	0, 0, 0, 0, 7, 0       // col 3
];

function makeB( uplo ) {
	var B = new Complex128Array( B_UPPER_DATA );
	zpotrf( uplo, 3, B, 1, 3, 0 );
	return B;
}


// TESTS //

test( 'zhegs2: itype1_upper', function t() {
	var tc = findCase( 'itype1_upper' );
	var B = makeB( 'upper' );
	var A = new Complex128Array( A_UPPER_DATA );
	var info = zhegs2( 1, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});

test( 'zhegs2: itype1_lower', function t() {
	var tc = findCase( 'itype1_lower' );
	var B = makeB( 'lower' );
	var A = new Complex128Array( A_LOWER_DATA );
	var info = zhegs2( 1, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});

test( 'zhegs2: itype2_upper', function t() {
	var tc = findCase( 'itype2_upper' );
	var B = makeB( 'upper' );
	var A = new Complex128Array( A_UPPER_DATA );
	var info = zhegs2( 2, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});

test( 'zhegs2: itype2_lower', function t() {
	var tc = findCase( 'itype2_lower' );
	var B = makeB( 'lower' );
	var A = new Complex128Array( A_LOWER_DATA );
	var info = zhegs2( 2, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});

test( 'zhegs2: itype3_upper', function t() {
	var tc = findCase( 'itype3_upper' );
	var B = makeB( 'upper' );
	var A = new Complex128Array( A_UPPER_DATA );
	var info = zhegs2( 3, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});

test( 'zhegs2: itype3_lower', function t() {
	var tc = findCase( 'itype3_lower' );
	var B = makeB( 'lower' );
	var A = new Complex128Array( A_LOWER_DATA );
	var info = zhegs2( 3, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});

test( 'zhegs2: n_zero', function t() {
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info = zhegs2( 1, 'upper', 0, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zhegs2: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Complex128Array( [ 9, 0 ] );
	var B = new Complex128Array( [ 3, 0 ] );
	var info = zhegs2( 1, 'upper', 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-13, 'A' );
});
