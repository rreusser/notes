/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zhpsv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
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
* @param {Array} actual - actual array
* @param {Array} expected - expected array
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
* Converts a typed array to a plain Array.
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

/**
* Build a Complex128Array from a flat array of interleaved doubles.
*
* @private
* @param {Array} flatDoubles - interleaved real/imag values
* @param {integer} nc - number of complex elements
* @returns {Complex128Array} complex array
*/
function buildComplex( flatDoubles, nc ) {
	var out = new Complex128Array( nc );
	var ov = reinterpret( out, 0 );
	var i;
	for ( i = 0; i < 2 * nc; i++ ) {
		ov[ i ] = flatDoubles[ i ];
	}
	return out;
}

/**
* Compute b = A*X where A is N-by-N Hermitian packed, X is N-by-nrhs.
*
* Unpacks A to full dense and multiplies. All arrays are Float64 interleaved.
*
* @private
* @param {Array} APdata - packed Hermitian matrix as flat doubles
* @param {Float64Array} Xv - solution X (2*N*nrhs doubles, col-major stride N)
* @param {integer} N - matrix order
* @param {integer} nrhs - number of right-hand sides
* @param {string} uplo - `upper` or `lower`
* @returns {Float64Array} result b (2*N*nrhs doubles)
*/
function hermPackedMatMul( APdata, Xv, N, nrhs, uplo ) {
	var Afull;
	var are;
	var aim;
	var xre;
	var xim;
	var kk;
	var rh;
	var b;
	var i;
	var j;

	// Unpack to full dense
	Afull = new Float64Array( 2 * N * N );
	kk = 0;
	if ( uplo === 'upper' ) {
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i <= j; i++ ) {
				are = APdata[ 2 * kk ];
				aim = APdata[ ( 2 * kk ) + 1 ];
				Afull[ 2 * ( i + ( j * N ) ) ] = are;
				Afull[ ( 2 * ( i + ( j * N ) ) ) + 1 ] = aim;
				Afull[ 2 * ( j + ( i * N ) ) ] = are;
				Afull[ ( 2 * ( j + ( i * N ) ) ) + 1 ] = -aim;
				kk += 1;
			}
		}
	} else {
		for ( j = 0; j < N; j++ ) {
			for ( i = j; i < N; i++ ) {
				are = APdata[ 2 * kk ];
				aim = APdata[ ( 2 * kk ) + 1 ];
				Afull[ 2 * ( i + ( j * N ) ) ] = are;
				Afull[ ( 2 * ( i + ( j * N ) ) ) + 1 ] = aim;
				Afull[ 2 * ( j + ( i * N ) ) ] = are;
				Afull[ ( 2 * ( j + ( i * N ) ) ) + 1 ] = -aim;
				kk += 1;
			}
		}
	}

	// Multiply b = A * X
	b = new Float64Array( 2 * N * nrhs );
	for ( rh = 0; rh < nrhs; rh++ ) {
		for ( i = 0; i < N; i++ ) {
			for ( j = 0; j < N; j++ ) {
				are = Afull[ 2 * ( i + ( j * N ) ) ];
				aim = Afull[ ( 2 * ( i + ( j * N ) ) ) + 1 ];
				xre = Xv[ 2 * ( j + ( rh * N ) ) ];
				xim = Xv[ ( 2 * ( j + ( rh * N ) ) ) + 1 ];
				b[ 2 * ( i + ( rh * N ) ) ] += ( are * xre ) - ( aim * xim );
				b[ ( 2 * ( i + ( rh * N ) ) ) + 1 ] += ( are * xim ) + ( aim * xre ); // eslint-disable-line max-len
			}
		}
	}
	return b;
}

/**
* Run a fixture-based zhpsv test verifying A*x = b round-trip.
*
* @private
* @param {string} name - fixture test case name
* @param {string} uplo - `upper` or `lower`
*/
function runFixtureTest( name, uplo ) {
	var nrhs;
	var info;
	var IPIV;
	var Ax;
	var Bv;
	var bV;
	var nn;
	var tc;
	var AP;
	var N;
	var B;

	tc = findCase( name );
	N = tc.n;
	nrhs = tc.nrhs;
	nn = ( N * ( N + 1 ) ) / 2;

	// Build input AP from fixture (packed Hermitian, nn complex elements)
	AP = buildComplex( tc.AP, nn );

	// Build input B (N*nrhs complex elements)
	B = buildComplex( tc.b, N * nrhs );
	bV = new Float64Array( tc.b );

	// Allocate IPIV
	IPIV = new Int32Array( N );

	// Call zhpsv: column-major B with strideB1=1, strideB2=N
	info = zhpsv( uplo, N, nrhs, AP, 1, 0, IPIV, 1, 0, B, 1, N, 0 );

	// Check info
	assert.strictEqual( info, tc.info, name + ': info' );

	// Verify round-trip: A * x should equal original b
	Bv = reinterpret( B, 0 );
	Ax = hermPackedMatMul( tc.AP, Bv, N, nrhs, uplo );
	assertArrayClose( toArray( Ax ), toArray( bV ), 1e-10, name + ' A*x=b' );
}


// TESTS //

test( 'zhpsv: main export is a function', function t() {
	assert.strictEqual( typeof zhpsv, 'function' );
});

test( 'zhpsv: upper 3x3, 1 RHS', function t() {
	runFixtureTest( 'upper_3x3_1rhs', 'upper' );
});

test( 'zhpsv: lower 3x3, 2 RHS', function t() {
	runFixtureTest( 'lower_3x3_2rhs', 'lower' );
});

test( 'zhpsv: N=1', function t() {
	runFixtureTest( 'n1', 'upper' );
});

test( 'zhpsv: N=0 quick return', function t() {
	var IPIV;
	var info;
	var AP;
	var B;

	AP = new Complex128Array( 1 );
	B = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	info = zhpsv( 'upper', 0, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'zhpsv: NRHS=0 quick return', function t() {
	var IPIV;
	var info;
	var AP;
	var B;

	AP = new Complex128Array( 1 );
	B = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	info = zhpsv( 'upper', 1, 0, AP, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'zhpsv: singular matrix returns info > 0', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var nn;
	var N;
	var B;

	tc = findCase( 'singular' );
	N = tc.n;
	nn = ( N * ( N + 1 ) ) / 2;
	AP = buildComplex( tc.AP, nn );
	B = buildComplex( tc.b, N * tc.nrhs );
	IPIV = new Int32Array( N );
	info = zhpsv( 'upper', N, tc.nrhs, AP, 1, 0, IPIV, 1, 0, B, 1, N, 0 );
	assert.strictEqual( info, tc.info );
	assert.ok( info > 0, 'singular matrix should return info > 0' );
});

test( 'zhpsv: upper 4x4 with 2x2 pivots', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var nn;
	var N;
	var B;

	tc = findCase( 'upper_4x4_pivot' );
	N = tc.n;
	nn = ( N * ( N + 1 ) ) / 2;
	AP = buildComplex( tc.AP, nn );
	B = buildComplex( tc.b, N * tc.nrhs );
	IPIV = new Int32Array( N );
	info = zhpsv( 'upper', N, tc.nrhs, AP, 1, 0, IPIV, 1, 0, B, 1, N, 0 );
	assert.strictEqual( info, 0 );
});

test( 'zhpsv: lower 4x4 with 2x2 pivots, 2 RHS', function t() {
	runFixtureTest( 'lower_4x4_pivot', 'lower' );
});
