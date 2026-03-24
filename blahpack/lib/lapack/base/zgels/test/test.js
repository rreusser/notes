

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgels = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgels.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Create a Complex128Array from interleaved re/im pairs.
*/
function c128( arr ) {
	var out = new Complex128Array( arr.length / 2 );
	var v = reinterpret( out, 0 );
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		v[ i ] = arr[ i ];
	}
	return out;
}

/**
* Extract n doubles from the Float64 view of a Complex128Array at a given complex offset.
*/
function extractDoubles( C128, complexOffset, nDoubles ) {
	var v = reinterpret( C128, 0 );
	var out = [];
	var i;
	for ( i = 0; i < nDoubles; i++ ) {
		out.push( v[ (complexOffset * 2) + i ] );
	}
	return out;
}


// TESTS //

test( 'zgels: overdetermined_4x2, TRANS=N', function t() {
	var tc = findCase( 'overdetermined_4x2' );
	// A = [1+i 1+2i; 2+i 2+i; 3 3+i; 1+3i 1] col-major (4x2, LDA=4)
	var A = c128( [ 1,1, 2,1, 3,0, 1,3, 1,2, 2,1, 3,1, 1,0 ] );
	// b = [1+i; 2; 3+i; 1] (LDB=4, NRHS=1)
	var B = c128( [ 1,1, 2,0, 3,1, 1,0 ] );
	var info = zgels( 'no-transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	// x occupies first 2 complex elements = 4 doubles
	assertArrayClose( extractDoubles( B, 0, 4 ), tc.x, 1e-12, 'x' );
});

test( 'zgels: underdetermined_2x4, TRANS=N', function t() {
	var tc = findCase( 'underdetermined_2x4' );
	// A = [1+i 0; 2 2+i; 3+i 1; 1 3+i] col-major (2x4, LDA=2)
	var A = c128( [ 1,1, 0,0, 2,0, 2,1, 3,1, 1,0, 1,0, 3,1 ] );
	// b = [10+2i; 5+i] but B is 4x1 (LDB=4)
	var B = c128( [ 10,2, 5,1, 0,0, 0,0 ] );
	var info = zgels( 'no-transpose', 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	// x occupies first 4 complex elements = 8 doubles
	assertArrayClose( extractDoubles( B, 0, 8 ), tc.x, 1e-12, 'x' );
});

test( 'zgels: square_3x3, TRANS=N', function t() {
	var tc = findCase( 'square_3x3' );
	// A = [5+i i 0; i 5+i i; 0 i 5+i] col-major
	var A = c128( [ 5,1, 0,1, 0,0, 0,1, 5,1, 0,1, 0,0, 0,1, 5,1 ] );
	var B = c128( [ 5,2, 5,3, 5,2 ] );
	var info = zgels( 'no-transpose', 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	assertArrayClose( extractDoubles( B, 0, 6 ), tc.x, 1e-12, 'x' );
});

test( 'zgels: conjtrans_mge_n_minnorm, TRANS=C, M>=N', function t() {
	var tc = findCase( 'conjtrans_mge_n_minnorm' );
	// A = [2+i 1; 1 2+i; 1+i 1+i; 0 1] col-major (4x2, LDA=4)
	var A = c128( [ 2,1, 1,0, 1,1, 0,0, 1,0, 2,1, 1,1, 1,0 ] );
	// b = [3+i; 2] but B is 4x1 (LDB=4)
	var B = c128( [ 3,1, 2,0, 0,0, 0,0 ] );
	var info = zgels( 'conjugate-transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	// x has 4 complex elements = 8 doubles
	assertArrayClose( extractDoubles( B, 0, 8 ), tc.x, 1e-12, 'x' );
});

test( 'zgels: conjtrans_mlt_n_ls, TRANS=C, M<N', function t() {
	var tc = findCase( 'conjtrans_mlt_n_ls' );
	// A = [1+i 1; 2 2+i; 3 1+i] col-major (2x3, LDA=2)
	var A = c128( [ 1,1, 1,0, 2,0, 2,1, 3,0, 1,1 ] );
	// b = [1; 2+i; 3] but B is 3x1 (LDB=3)
	var B = c128( [ 1,0, 2,1, 3,0 ] );
	var info = zgels( 'conjugate-transpose', 2, 3, 1, A, 1, 2, 0, B, 1, 3, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	// x has 2 complex elements = 4 doubles
	assertArrayClose( extractDoubles( B, 0, 4 ), tc.x, 1e-12, 'x' );
});

test( 'zgels: multi_rhs_overdetermined, TRANS=N', function t() {
	var tc = findCase( 'multi_rhs_overdetermined' );
	// A = [2 i 1 1+i; 1 2 1+i i] col-major (4x2, LDA=4)
	var A = c128( [ 2,0, 0,1, 1,0, 1,1, 1,0, 2,0, 1,1, 0,1 ] );
	// B has 2 columns (LDB=4, NRHS=2)
	var B = c128( [
		3,0, 2,1, 2,0, 1,1,
		5,1, 4,0, 3,1, 2,0
	] );
	var info = zgels( 'no-transpose', 4, 2, 2, A, 1, 4, 0, B, 1, 4, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	// x1: first 2 complex elements of column 1
	assertArrayClose( extractDoubles( B, 0, 4 ), tc.x1, 1e-12, 'x1' );
	// x2: first 2 complex elements of column 2 (offset by LDB=4)
	assertArrayClose( extractDoubles( B, 4, 4 ), tc.x2, 1e-12, 'x2' );
});

test( 'zgels: n_zero quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = c128( [ 1,0 ] );
	var B = c128( [ 1,0, 0,0, 0,0 ] );
	var info = zgels( 'no-transpose', 3, 0, 1, A, 1, 3, 0, B, 1, 3, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
});

test( 'zgels: m_zero quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = c128( [ 0,0 ] );
	var B = c128( [ 0,0, 0,0, 0,0 ] );
	var info = zgels( 'no-transpose', 0, 3, 1, A, 1, 1, 0, B, 1, 3, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
});

test( 'zgels: nrhs_zero quick return', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = c128( [ 1,0, 0,0, 0,0, 1,0 ] );
	var B = c128( [ 0,0, 0,0 ] );
	var info = zgels( 'no-transpose', 2, 2, 0, A, 1, 2, 0, B, 1, 2, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
});

test( 'zgels: overdetermined_6x3', function t() {
	var tc = findCase( 'overdetermined_6x3' );
	// A col-major (6x3, LDA=6), diag-dominant complex matrix
	var A = c128( [
		10,1, 1,0, 1,0, 0,1, 1,0, 0,0,
		1,0, 10,1, 1,0, 1,0, 0,1, 0,0,
		1,0, 1,0, 10,1, 1,0, 1,0, 0,1
	] );
	var B = c128( [ 15,1, 24,2, 33,3, 5,1, 5,2, 0,3 ] );
	var info = zgels( 'no-transpose', 6, 3, 1, A, 1, 6, 0, B, 1, 6, 0, null, 1, 0, -1 );
	assert.equal( info, tc.info );
	assertArrayClose( extractDoubles( B, 0, 6 ), tc.x, 1e-10, 'x' );
});
