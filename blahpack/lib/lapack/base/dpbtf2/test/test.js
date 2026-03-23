

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var path = require( 'path' );
var dpbtf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpbtf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpbtf2: upper_tridiag_5', function t() {
	var tc = findCase( 'upper_tridiag_5' );
	// UPLO='U', N=5, KD=1, LDAB=2
	// Band storage: row 0 = superdiag, row 1 = diag
	var ab = new Float64Array([
		0.0,  2.0,   // col 0
		-1.0, 2.0,   // col 1
		-1.0, 2.0,   // col 2
		-1.0, 2.0,   // col 3
		-1.0, 2.0    // col 4
	]);
	var info = dpbtf2( 'U', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: lower_tridiag_5', function t() {
	var tc = findCase( 'lower_tridiag_5' );
	// UPLO='L', N=5, KD=1, LDAB=2
	// Band storage: row 0 = diag, row 1 = subdiag
	var ab = new Float64Array([
		2.0,  -1.0,  // col 0
		2.0,  -1.0,  // col 1
		2.0,  -1.0,  // col 2
		2.0,  -1.0,  // col 3
		2.0,   0.0   // col 4
	]);
	var info = dpbtf2( 'L', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: upper_penta_4', function t() {
	var tc = findCase( 'upper_penta_4' );
	// UPLO='U', N=4, KD=2, LDAB=3
	var ab = new Float64Array([
		0.0,  0.0,  4.0,   // col 0
		0.0, -1.0,  4.0,   // col 1
		0.5, -1.0,  4.0,   // col 2
		0.5, -1.0,  4.0    // col 3
	]);
	var info = dpbtf2( 'U', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: lower_penta_4', function t() {
	var tc = findCase( 'lower_penta_4' );
	// UPLO='L', N=4, KD=2, LDAB=3
	var ab = new Float64Array([
		4.0, -1.0,  0.5,   // col 0
		4.0, -1.0,  0.5,   // col 1
		4.0, -1.0,  0.0,   // col 2
		4.0,  0.0,  0.0    // col 3
	]);
	var info = dpbtf2( 'L', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: n_one', function t() {
	var tc = findCase( 'n_one' );
	var ab = new Float64Array([ 9.0 ]);
	var info = dpbtf2( 'U', 1, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var ab = new Float64Array([ 99.0 ]);
	var info = dpbtf2( 'L', 0, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpbtf2: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	// UPLO='L', N=2, KD=1, LDAB=2
	var ab = new Float64Array([
		1.0,  2.0,   // col 0
		1.0,  0.0    // col 1
	]);
	var info = dpbtf2( 'L', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: upper_full_3', function t() {
	var tc = findCase( 'upper_full_3' );
	// UPLO='U', N=3, KD=2, LDAB=3
	// A = [4 2 1; 2 5 3; 1 3 6]
	var ab = new Float64Array([
		0.0, 0.0, 4.0,   // col 0
		0.0, 2.0, 5.0,   // col 1
		1.0, 3.0, 6.0    // col 2
	]);
	var info = dpbtf2( 'U', 3, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: not_posdef upper', function t() {
	// UPLO='U', N=2, KD=1, LDAB=2
	// Not positive definite matrix
	var ab = new Float64Array([
		0.0, 1.0,   // col 0: diag=1
		2.0, 1.0    // col 1: superdiag=2, diag=1
	]);
	var info = dpbtf2( 'U', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, 2 );
});
