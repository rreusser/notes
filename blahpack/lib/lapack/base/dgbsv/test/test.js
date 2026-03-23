'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var path = require( 'path' );
var dgbsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgbsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Creates a band matrix in column-major flat array.
*
* @param {number} ldab - leading dimension (2*KL+KU+1)
* @param {number} n - number of columns
* @param {Array} entries - array of [row, col, value] (0-based row/col in band storage)
* @returns {Float64Array} flat band matrix
*/
function bandMatrix( ldab, n, entries ) {
	var AB = new Float64Array( ldab * n );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		AB[ entries[i][0] + entries[i][1] * ldab ] = entries[i][2];
	}
	return AB;
}


// TESTS //

test( 'dgbsv: 4x4 tridiagonal, single RHS', function t() {
	var tc = findCase( 'tridiag_4x4_1rhs' );
	var IPIV;
	var info;
	var AB;
	var B;

	// N=4, KL=1, KU=1, LDAB=2*1+1+1=4
	// Band storage (0-indexed rows): row0=fill, row1=superdiag, row2=diag, row3=subdiag
	AB = bandMatrix( 4, 4, [
		// col 0
		[ 2, 0, 4.0 ], [ 3, 0, -1.0 ],
		// col 1
		[ 1, 1, -1.0 ], [ 2, 1, 4.0 ], [ 3, 1, -1.0 ],
		// col 2
		[ 1, 2, -1.0 ], [ 2, 2, 4.0 ], [ 3, 2, -1.0 ],
		// col 3
		[ 1, 3, -1.0 ], [ 2, 3, 4.0 ]
	]);
	B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	IPIV = new Int32Array( 4 );

	info = dgbsv( 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: 4x4 tridiagonal, 2 RHS', function t() {
	var tc = findCase( 'tridiag_4x4_2rhs' );
	var IPIV;
	var info;
	var AB;
	var B;

	AB = bandMatrix( 4, 4, [
		[ 2, 0, 4.0 ], [ 3, 0, -1.0 ],
		[ 1, 1, -1.0 ], [ 2, 1, 4.0 ], [ 3, 1, -1.0 ],
		[ 1, 2, -1.0 ], [ 2, 2, 4.0 ], [ 3, 2, -1.0 ],
		[ 1, 3, -1.0 ], [ 2, 3, 4.0 ]
	]);
	// B is N-by-NRHS column-major with LDB=4 (tight packing)
	B = new Float64Array( 8 );
	B[ 0 ] = 1.0; B[ 1 ] = 2.0; B[ 2 ] = 3.0; B[ 3 ] = 4.0; // RHS 1
	B[ 4 ] = 4.0; B[ 5 ] = 3.0; B[ 6 ] = 2.0; B[ 7 ] = 1.0; // RHS 2
	IPIV = new Int32Array( 4 );

	info = dgbsv( 4, 1, 1, 2, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: 5x5 pentadiagonal, single RHS', function t() {
	var tc = findCase( 'pentadiag_5x5_1rhs' );
	var IPIV;
	var info;
	var AB;
	var B;

	// N=5, KL=2, KU=2, LDAB=2*2+2+1=7
	AB = bandMatrix( 7, 5, [
		// col 0: diag at row KL+KU=4, subdiags at row 5,6
		[ 4, 0, 6.0 ], [ 5, 0, -2.0 ], [ 6, 0, 1.0 ],
		// col 1: superdiag at row 3, diag at row 4, sub at 5,6
		[ 3, 1, -2.0 ], [ 4, 1, 6.0 ], [ 5, 1, -2.0 ], [ 6, 1, 1.0 ],
		// col 2: super at row 2,3, diag at 4, sub at 5,6
		[ 2, 2, 1.0 ], [ 3, 2, -2.0 ], [ 4, 2, 6.0 ], [ 5, 2, -2.0 ], [ 6, 2, 1.0 ],
		// col 3
		[ 2, 3, 1.0 ], [ 3, 3, -2.0 ], [ 4, 3, 6.0 ], [ 5, 3, -2.0 ],
		// col 4
		[ 2, 4, 1.0 ], [ 3, 4, -2.0 ], [ 4, 4, 6.0 ]
	]);
	B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	IPIV = new Int32Array( 5 );

	info = dgbsv( 5, 2, 2, 1, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 5, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var IPIV;
	var info;
	var AB;
	var B;

	AB = new Float64Array( 4 );
	B = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );

	info = dgbsv( 0, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgbsv: NRHS=0 still factorizes AB', function t() {
	var tc = findCase( 'nrhs_zero' );
	var IPIV;
	var info;
	var AB;

	// Even with nrhs=0, dgbsv calls dgbtrf (factorization still happens)
	AB = bandMatrix( 4, 4, [
		[ 2, 0, 4.0 ], [ 3, 0, -1.0 ],
		[ 1, 1, -1.0 ], [ 2, 1, 4.0 ], [ 3, 1, -1.0 ],
		[ 1, 2, -1.0 ], [ 2, 2, 4.0 ], [ 3, 2, -1.0 ],
		[ 1, 3, -1.0 ], [ 2, 3, 4.0 ]
	]);
	IPIV = new Int32Array( 4 );

	info = dgbsv( 4, 1, 1, 0, AB, 1, 4, 0, IPIV, 1, 0, new Float64Array( 1 ), 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	// Verify that IPIV was filled (factorization happened)
	// Fortran IPIV is 1-based, JS is 0-based
	var i;
	var anyNonZero = false;
	for ( i = 0; i < 4; i++ ) {
		if ( IPIV[ i ] !== 0 ) {
			anyNonZero = true;
		}
	}
	// With this tridiagonal matrix, no pivoting occurs, so IPIV = [0,1,2,3] (0-based)
	assert.equal( IPIV[ 0 ], 0, 'IPIV[0]' );
	assert.equal( IPIV[ 1 ], 1, 'IPIV[1]' );
	assert.equal( IPIV[ 2 ], 2, 'IPIV[2]' );
	assert.equal( IPIV[ 3 ], 3, 'IPIV[3]' );
});

test( 'dgbsv: singular matrix returns info > 0', function t() {
	var tc = findCase( 'singular' );
	var IPIV;
	var info;
	var AB;
	var B;

	// 2x2 matrix: A = [1 0; 0 0], KL=1, KU=1, LDAB=4
	AB = bandMatrix( 4, 2, [
		[ 2, 0, 1.0 ]  // diag(0) = 1, diag(1) = 0 (default)
	]);
	B = new Float64Array( [ 1.0, 2.0 ] );
	IPIV = new Int32Array( 2 );

	info = dgbsv( 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.equal( info, tc.info, 'info (singular at position 2)' );
});

test( 'dgbsv: pivoting 2x2', function t() {
	var tc = findCase( 'pivot_2x2' );
	var IPIV;
	var info;
	var AB;
	var B;

	// A = [1 2; 3 4], KL=1, KU=1, LDAB=4
	AB = bandMatrix( 4, 2, [
		[ 2, 0, 1.0 ], [ 3, 0, 3.0 ],  // col 0: diag=1, sub=3
		[ 1, 1, 2.0 ], [ 2, 1, 4.0 ]    // col 1: super=2, diag=4
	]);
	B = new Float64Array( [ 5.0, 11.0 ] );
	IPIV = new Int32Array( 2 );

	info = dgbsv( 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: 5x5 pentadiagonal, 3 RHS', function t() {
	var tc = findCase( 'pentadiag_5x5_3rhs' );
	var IPIV;
	var info;
	var AB;
	var B;

	// N=5, KL=2, KU=2, LDAB=7, NRHS=3
	AB = bandMatrix( 7, 5, [
		[ 4, 0, 6.0 ], [ 5, 0, -2.0 ], [ 6, 0, 1.0 ],
		[ 3, 1, -2.0 ], [ 4, 1, 6.0 ], [ 5, 1, -2.0 ], [ 6, 1, 1.0 ],
		[ 2, 2, 1.0 ], [ 3, 2, -2.0 ], [ 4, 2, 6.0 ], [ 5, 2, -2.0 ], [ 6, 2, 1.0 ],
		[ 2, 3, 1.0 ], [ 3, 3, -2.0 ], [ 4, 3, 6.0 ], [ 5, 3, -2.0 ],
		[ 2, 4, 1.0 ], [ 3, 4, -2.0 ], [ 4, 4, 6.0 ]
	]);
	// B: 5x3, column-major with LDB=5
	B = new Float64Array( 15 );
	// RHS 1: e1
	B[ 0 ] = 1.0;
	// RHS 2: e3
	B[ 5 + 2 ] = 1.0;
	// RHS 3: [1,2,3,4,5]
	B[ 10 ] = 1.0; B[ 11 ] = 2.0; B[ 12 ] = 3.0; B[ 13 ] = 4.0; B[ 14 ] = 5.0;
	IPIV = new Int32Array( 5 );

	info = dgbsv( 5, 2, 2, 3, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 5, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});
