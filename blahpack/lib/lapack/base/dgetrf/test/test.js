/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgetrf.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts 1-based Fortran IPIV to 0-based JS IPIV for comparison.
*/
function ipivTo0Based( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] - 1 );
	}
	return out;
}

/**
* Verifies P_L_U = A_original for a factored M x N matrix.
*
* @param {Float64Array} Aorig - original matrix (col-major, M rows, N cols, LDA=M)
* @param {Float64Array} ALU - factored matrix from dgetrf (col-major)
* @param {Int32Array} IPIV - 0-based pivot indices from dgetrf
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {number} tol - tolerance
* @param {string} msg - error message prefix
*/
function assertFactorizationCorrect( Aorig, ALU, IPIV, M, N, tol, msg ) {
	var result;
	var minMN = Math.min( M, N );
	var sum;
	var tmp;
	var LU;
	var i;
	var j;
	var k;

	// Compute L*U (M x N)
	LU = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			sum = 0.0;
			for ( k = 0; k < minMN; k++ ) {
				// L(i,k): if i===k, 1.0; if i>k, ALU[i+k*M]; else 0
				// U(k,j): if k<=j, ALU[k+j*M]; else 0
				var Lik;
				var Ukj;
				if ( i === k ) {
					Lik = 1.0;
				} else if ( i > k ) {
					Lik = ALU[ i + k * M ];
				} else {
					Lik = 0.0;
				}
				if ( k <= j ) {
					Ukj = ALU[ k + j * M ];
				} else {
					Ukj = 0.0;
				}
				sum += Lik * Ukj;
			}
			LU[ i + j * M ] = sum;
		}
	}

	// Apply P^T (undo row interchanges in reverse) to get P*L*U
	result = new Float64Array( LU );
	for ( i = minMN - 1; i >= 0; i-- ) {
		if ( IPIV[ i ] !== i ) {
			for ( j = 0; j < N; j++ ) {
				tmp = result[ i + j * M ];
				result[ i + j * M ] = result[ IPIV[ i ] + j * M ];
				result[ IPIV[ i ] + j * M ] = tmp;
			}
		}
	}

	// Compare
	for ( i = 0; i < M * N; i++ ) {
		assertClose( result[ i ], Aorig[ i ], tol, msg + ' PLU[' + i + ']' );
	}
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'dgetrf: 3x3', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( '3x3' );
	A = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	IPIV = new Int32Array( 3 );
	info = dgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( A ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( toArray( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf: 5x4 tall matrix', function t() {
	var Aorig;
	var IPIV;
	var info;
	var A;

	Aorig = new Float64Array([
		1.0,
		6.0,
		11.0,
		16.0,
		21.0,
		2.0,
		7.0,
		12.0,
		17.0,
		22.0,
		3.0,
		8.0,
		13.0,
		18.0,
		23.0,
		4.0,
		9.0,
		14.0,
		19.0,
		24.0
	]);
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 4 );
	info = dgetrf( 5, 4, A, 1, 5, 0, IPIV, 1, 0 );
	assert.ok( info > 0, 'info > 0 for rank-deficient matrix' );
	assertFactorizationCorrect( Aorig, A, IPIV, 5, 4, 1e-13, '5x4' );
});

test( 'dgetrf: singular', function t() {
	var Aorig;
	var IPIV;
	var info;
	var A;

	Aorig = new Float64Array( [ 1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 3.0, 6.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );
	info = dgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	assert.ok( info > 0, 'info > 0 for singular matrix' );
	assertFactorizationCorrect( Aorig, A, IPIV, 3, 3, 1e-14, 'singular' );
});

test( 'dgetrf: n_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( 'n_zero' );
	A = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	info = dgetrf( 3, 0, A, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dgetrf: m_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( 'm_zero' );
	A = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	info = dgetrf( 0, 3, A, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dgetrf: 1x1', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( '1x1' );
	A = new Float64Array( [ 7.0 ] );
	IPIV = new Int32Array( 1 );
	info = dgetrf( 1, 1, A, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( A ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( toArray( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf: 4x4', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( '4x4' );
	A = new Float64Array([
		2.0,
		1.0,
		3.0,
		4.0,
		1.0,
		3.0,
		2.0,
		1.0,
		3.0,
		2.0,
		1.0,
		2.0,
		1.0,
		2.0,
		4.0,
		3.0
	]);
	IPIV = new Int32Array( 4 );
	info = dgetrf( 4, 4, A, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( A ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( toArray( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf: non-unit stride with offset', function t() {
	var subIPIV;
	var subOrig;
	var Aorig;
	var IPIV;
	var info;
	var subA;
	var A;

	Aorig = new Float64Array( [ 0.0, 0.0, 0.0, 4.0, 3.0, 6.0, 8.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( [ 0, 0, 0, 0 ] );
	info = dgetrf( 2, 2, A, 1, 2, 3, IPIV, 1, 1 );
	assert.equal( info, 0, 'info' );
	subA = new Float64Array( [ A[ 3 ], A[ 4 ], A[ 5 ], A[ 6 ] ] );
	subIPIV = new Int32Array( [ IPIV[ 1 ], IPIV[ 2 ] ] );
	subOrig = new Float64Array( [ 4.0, 3.0, 6.0, 8.0 ] );
	assertFactorizationCorrect( subOrig, subA, subIPIV, 2, 2, 1e-14, 'offset' );
});

test( 'dgetrf: 70x70 blocked path (NB=64, min(M,N) > NB)', function t() {
	var Aorig;
	var IPIV;
	var info;
	var seed;
	var N;
	var A;
	var i;
	var j;

	N = 70;
	Aorig = new Float64Array( N * N );
	seed = 12345;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * N ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < N; i++ ) {
		Aorig[ i + i * N ] += 100.0;
	}
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( N );
	info = dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, '70x70 info should be 0 for non-singular matrix' );
	assertFactorizationCorrect( Aorig, A, IPIV, N, N, 1e-10, '70x70 blocked' );
});

test( 'dgetrf: 80x70 tall blocked path', function t() {
	var Aorig;
	var minMN;
	var IPIV;
	var info;
	var seed;
	var M;
	var N;
	var A;
	var i;
	var j;

	M = 80;
	N = 70;
	minMN = Math.min( M, N );
	Aorig = new Float64Array( M * N );
	seed = 67890;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * M ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < minMN; i++ ) {
		Aorig[ i + i * M ] += 100.0;
	}
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( minMN );
	info = dgetrf( M, N, A, 1, M, 0, IPIV, 1, 0 );
	assert.equal( info, 0, '80x70 info should be 0' );
	assertFactorizationCorrect( Aorig, A, IPIV, M, N, 1e-10, '80x70 blocked' );
});

test( 'dgetrf: 70x80 wide blocked path', function t() {
	var Aorig;
	var minMN;
	var IPIV;
	var info;
	var seed;
	var M;
	var N;
	var A;
	var i;
	var j;

	M = 70;
	N = 80;
	minMN = Math.min( M, N );
	Aorig = new Float64Array( M * N );
	seed = 11111;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * M ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < minMN; i++ ) {
		Aorig[ i + i * M ] += 100.0;
	}
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( minMN );
	info = dgetrf( M, N, A, 1, M, 0, IPIV, 1, 0 );
	assert.equal( info, 0, '70x80 info should be 0' );
	assertFactorizationCorrect( Aorig, A, IPIV, M, N, 1e-10, '70x80 blocked' );
});

test( 'dgetrf: 70x70 singular matrix in blocked path (iinfo > 0 branch)', function t() { // eslint-disable-line max-len
	var Aorig;
	var IPIV;
	var info;
	var seed;
	var N;
	var A;
	var i;
	var j;

	N = 70;
	Aorig = new Float64Array( N * N );
	seed = 99999;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * N ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < N; i++ ) {
		Aorig[ i + i * N ] += 100.0;
	}
	for ( j = 0; j < N; j++ ) {
		Aorig[ 64 + j * N ] = Aorig[ 0 + j * N ];
	}
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( N );
	info = dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.ok( info > 0, '70x70 singular: info > 0 (got ' + info + ')' );
	assertFactorizationCorrect( Aorig, A, IPIV, N, N, 1e-8, '70x70 singular blocked' ); // eslint-disable-line max-len
});
