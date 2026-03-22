

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgesv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgesv.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Computes matrix-matrix product C = A*B (col-major, N x N times N x NRHS).
*/
function matmat( A, B, N, nrhs ) {
	var C = new Float64Array( N * nrhs );
	var i;
	var j;
	var k;
	for ( j = 0; j < nrhs; j++ ) {
		for ( i = 0; i < N; i++ ) {
			for ( k = 0; k < N; k++ ) {
				C[ i + j * N ] += A[ i + k * N ] * B[ k + j * N ];
			}
		}
	}
	return C;
}


// TESTS //

test( 'dgesv: solve_3x3', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'solve_3x3' );

	// A = [2 1 1; 4 3 3; 8 7 9] col-major
	Aorig = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );

	// b = [4; 10; 24], solution x = [1; 1; 1]
	Borig = new Float64Array( [ 4.0, 10.0, 24.0 ] );
	B = new Float64Array( Borig );

	info = dgesv( 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );

	// Verify A_orig * x = b_orig
	AB = matmat( Aorig, B, 3, 1 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-14, 'A*x=b' );
});

test( 'dgesv: singular', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'singular' );

	// Singular 3x3: [1 2 3; 2 4 6; 3 6 9] col-major
	A = new Float64Array( [ 1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 3.0, 6.0, 9.0 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1.0, 2.0, 3.0 ] );

	info = dgesv( 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	// Singular matrix: info > 0
	assert.ok( info > 0, 'info > 0 for singular matrix' );
});

test( 'dgesv: n_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_zero' );

	A = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( 1 );

	info = dgesv( 0, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgesv: nrhs_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'nrhs_zero' );

	A = new Float64Array( [ 5.0 ] );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( 1 );

	info = dgesv( 1, 0, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgesv: multi_rhs', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'multi_rhs' );

	// A = [1 2; 3 4] col-major
	Aorig = new Float64Array( [ 1.0, 3.0, 2.0, 4.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 2 );

	// B = [5 11; 6 12] col-major (2x2)
	Borig = new Float64Array( [ 5.0, 6.0, 11.0, 12.0 ] );
	B = new Float64Array( Borig );

	info = dgesv( 2, 2, A, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );

	// Verify A_orig * X = B_orig
	AB = matmat( Aorig, B, 2, 2 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-14, 'A*X=B' );
});

test( 'dgesv: 1x1', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( '1x1' );

	// 5x = 10 => x = 2
	A = new Float64Array( [ 5.0 ] );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( [ 10.0 ] );

	info = dgesv( 1, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgesv: 4x4', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( '4x4' );

	// A = [4 1 2 3; 1 5 1 2; 2 1 6 1; 3 2 1 7] col-major
	Aorig = new Float64Array( [
		4.0, 1.0, 2.0, 3.0,
		1.0, 5.0, 1.0, 2.0,
		2.0, 1.0, 6.0, 1.0,
		3.0, 2.0, 1.0, 7.0
	] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 4 );

	// b = A * [1;2;3;4] = [24;22;26;38]
	Borig = new Float64Array( [ 24.0, 22.0, 26.0, 38.0 ] );
	B = new Float64Array( Borig );

	info = dgesv( 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );

	// Verify A_orig * x = b_orig
	AB = matmat( Aorig, B, 4, 1 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-14, 'A*x=b' );
});
