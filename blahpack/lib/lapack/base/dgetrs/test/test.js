

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgetrf = require( './../../dgetrf/lib/base.js' );
var dgetrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgetrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Factorizes matrix A in place and returns info.
*/
function factorize( N, A, IPIV ) {
	return dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
}

/**
* Computes matrix-vector product y = A*x (col-major, N x N).
*/
function matvec( A, x, N ) {
	var y = new Float64Array( N );
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			y[ i ] += A[ i + j * N ] * x[ j ];
		}
	}
	return y;
}

/**
* Computes matrix-vector product y = A^T * x (col-major, N x N).
*/
function matvecT( A, x, N ) {
	var y = new Float64Array( N );
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			y[ i ] += A[ j + i * N ] * x[ j ];
		}
	}
	return y;
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

test( 'dgetrs: solve_3x3', function t() {
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var Ax;
	var A;
	var B;

	tc = findCase( 'solve_3x3' );

	// A = [2 1 1; 4 3 3; 8 7 9] col-major
	Aorig = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1.0, 1.0, 1.0 ] );

	factorize( 3, A, IPIV );
	info = dgetrs( 'N', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );

	// Verify A*x = b: multiply original A by solution x (now in B)
	Ax = matvec( Aorig, B, 3 );
	assertArrayClose( Array.from( Ax ), [ 1.0, 1.0, 1.0 ], 1e-14, 'A*x=b' );
});

test( 'dgetrs: solve_3x3_trans', function t() {
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var ATx;
	var A;
	var B;

	tc = findCase( 'solve_3x3_trans' );

	// Same matrix, transpose solve
	Aorig = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1.0, 1.0, 1.0 ] );

	factorize( 3, A, IPIV );
	info = dgetrs( 'T', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );

	// Verify A^T * x = b
	ATx = matvecT( Aorig, B, 3 );
	assertArrayClose( Array.from( ATx ), [ 1.0, 1.0, 1.0 ], 1e-14, 'A^T*x=b' );
});

test( 'dgetrs: multi_rhs', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'multi_rhs' );

	// A = [2 1 1; 4 3 3; 8 7 9] col-major
	Aorig = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );

	// B is 3x2 col-major: b1 = [1;0;0], b2 = [0;1;0]
	Borig = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
	B = new Float64Array( Borig );

	factorize( 3, A, IPIV );
	info = dgetrs( 'N', 3, 2, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );

	// Verify A*X = B_original
	AB = matmat( Aorig, B, 3, 2 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-14, 'A*X=B' );
});

test( 'dgetrs: n_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_zero' );

	A = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( 1 );

	info = dgetrs( 'N', 0, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgetrs: nrhs_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'nrhs_zero' );

	A = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( 3 );

	info = dgetrs( 'N', 3, 0, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgetrs: 1x1', function t() {
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

	factorize( 1, A, IPIV );
	info = dgetrs( 'N', 1, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgetrs: identity', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'identity' );

	// 3x3 identity matrix, col-major
	A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 3.0, 5.0, 7.0 ] );

	factorize( 3, A, IPIV );
	info = dgetrs( 'N', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgetrs: lowercase trans argument', function t() {
	var Aorig;
	var IPIV;
	var info;
	var Ax;
	var A;
	var B;

	// Same as solve_3x3 but with lowercase 'n'
	Aorig = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1.0, 1.0, 1.0 ] );

	factorize( 3, A, IPIV );
	info = dgetrs( 'n', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, 0, 'info' );
	Ax = matvec( Aorig, B, 3 );
	assertArrayClose( Array.from( Ax ), [ 1.0, 1.0, 1.0 ], 1e-14, 'A*x=b' );
});
