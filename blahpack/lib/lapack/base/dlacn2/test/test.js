'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlacn2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlacn2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Matrix-vector multiply: y = A*x or y = A^T * x, column-major.
*/
function matvec( trans, n, A, x ) {
	var y = new Float64Array( n );
	var i;
	var j;
	for ( i = 0; i < n; i++ ) {
		for ( j = 0; j < n; j++ ) {
			if ( trans === 'N' ) {
				y[ i ] += A[ j * n + i ] * x[ j ];
			} else {
				y[ i ] += A[ i * n + j ] * x[ j ];
			}
		}
	}
	return y;
}

/**
* Run dlacn2 reverse-communication loop to estimate 1-norm of matrix A.
*/
function estimateNorm( n, A ) {
	var v = new Float64Array( n );
	var x = new Float64Array( n );
	var ISGN = new Int32Array( n );
	var EST = new Float64Array( 1 );
	var KASE = new Int32Array( 1 );
	var ISAVE = new Int32Array( 3 );
	var iter = 0;
	var yy;

	KASE[ 0 ] = 0;
	EST[ 0 ] = 0.0;

	while ( true ) { // eslint-disable-line no-constant-condition
		dlacn2( n, v, 1, 0, x, 1, 0, ISGN, 1, 0, EST, KASE, ISAVE, 1, 0 );
		if ( KASE[ 0 ] === 0 ) {
			break;
		}
		iter += 1;
		if ( iter > 20 ) {
			break;
		}
		if ( KASE[ 0 ] === 1 ) {
			yy = matvec( 'N', n, A, x );
			x.set( yy );
		} else {
			yy = matvec( 'T', n, A, x );
			x.set( yy );
		}
	}
	return { est: EST[ 0 ], iterations: iter };
}


// TESTS //

test( 'dlacn2: main export is a function', function t() {
	assert.strictEqual( typeof dlacn2, 'function' );
});

test( 'dlacn2: 3x3 identity matrix (1-norm = 1)', function t() {
	var tc = findCase( 'identity_3x3' );
	var n = 3;
	var A = new Float64Array( [
		1, 0, 0,
		0, 1, 0,
		0, 0, 1
	] );
	var result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 4x4 upper triangular', function t() {
	var tc = findCase( 'upper_tri_4x4' );
	var n = 4;
	// A = [[1, 2, 3, 4], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
	// Column-major layout
	var A = new Float64Array( [
		1, 0, 0, 0,
		2, 1, 0, 0,
		3, 0, 1, 0,
		4, 0, 0, 1
	] );
	var result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 1x1 matrix', function t() {
	var tc = findCase( '1x1' );
	var n = 1;
	var A = new Float64Array( [ 7 ] );
	var result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 5x5 diagonal matrix (1-norm = max|diag| = 5)', function t() {
	var tc = findCase( 'diag_5x5' );
	var n = 5;
	var A = new Float64Array( n * n );
	A[ 0 * n + 0 ] = 2.0;
	A[ 1 * n + 1 ] = -5.0;
	A[ 2 * n + 2 ] = 3.0;
	A[ 3 * n + 3 ] = 1.0;
	A[ 4 * n + 4 ] = 4.0;
	var result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 3x3 dense matrix (1-norm = 18)', function t() {
	var tc = findCase( 'dense_3x3' );
	var n = 3;
	// A = [[1, -2, 3], [4, 5, -6], [-7, 8, 9]], column-major
	var A = new Float64Array( [
		1, 4, -7,
		-2, 5, 8,
		3, -6, 9
	] );
	var result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});
