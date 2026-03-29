/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlacn2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlacn2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

/**
* Matrix-vector multiply: y = A_x or y = A^T _ x, column-major.
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
	var ISAVE = new Int32Array( 3 );
	var ISGN = new Int32Array( n );
	var KASE = new Int32Array( 1 );
	var iter = 0;
	var EST = new Float64Array( 1 );
	var yy;
	var v = new Float64Array( n );
	var x = new Float64Array( n );

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
	return {
		'est': EST[ 0 ],
		'iterations': iter
	};
}


// TESTS //

test( 'dlacn2: main export is a function', function t() {
	assert.strictEqual( typeof dlacn2, 'function' );
});

test( 'dlacn2: 3x3 identity matrix (1-norm = 1)', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = findCase( 'identity_3x3' );
	n = 3;
	A = new Float64Array([
		1,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		1
	]);
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 4x4 upper triangular', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = findCase( 'upper_tri_4x4' );
	n = 4;
	A = new Float64Array([
		1,
		0,
		0,
		0,
		2,
		1,
		0,
		0,
		3,
		0,
		1,
		0,
		4,
		0,
		0,
		1
	]);
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 1x1 matrix', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = findCase( '1x1' );
	n = 1;
	A = new Float64Array( [ 7 ] );
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 5x5 diagonal matrix (1-norm = max|diag| = 5)', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = findCase( 'diag_5x5' );
	n = 5;
	A = new Float64Array( n * n );
	A[ 0 * n + 0 ] = 2.0;
	A[ 1 * n + 1 ] = -5.0;
	A[ 2 * n + 2 ] = 3.0;
	A[ 3 * n + 3 ] = 1.0;
	A[ 4 * n + 4 ] = 4.0;
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacn2: 3x3 dense matrix (1-norm = 18)', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = findCase( 'dense_3x3' );
	n = 3;
	A = new Float64Array([
		1,
		4,
		-7,
		-2,
		5,
		8,
		3,
		-6,
		9
	]);
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});
