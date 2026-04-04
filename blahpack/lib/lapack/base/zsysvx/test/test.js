'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsysvx = require( './../lib/base.js' );

// FIXTURES //

var upper_4x4_1rhs = require( './fixtures/upper_4x4_1rhs.json' );
var lower_4x4_2rhs = require( './fixtures/lower_4x4_2rhs.json' );

// FUNCTIONS //

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

function toF64( cArr, n ) {
	return Array.prototype.slice.call( reinterpret( cArr, 0 ), 0, n );
}

// TESTS //

test( 'zsysvx: upper 4x4, 1 RHS, not-factored', function t() {
	var tc = upper_4x4_1rhs;
	var n = 4;
	var nrhs = 1;
	var A = new Complex128Array( new Float64Array( tc.A ) );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var B = new Complex128Array( new Float64Array( tc.B ) );
	var X = new Complex128Array( n * nrhs );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var WORK = new Complex128Array( 2 * n );
	var RWORK = new Float64Array( n );

	var info = zsysvx(
		'not-factored', 'upper', n, nrhs,
		A, 1, n, 0,
		AF, 1, n, 0,
		IPIV, 1, 0,
		B, 1, n, 0,
		X, 1, n, 0,
		rcond,
		FERR, 1, 0,
		BERR, 1, 0,
		WORK, 1, 0, 2 * n,
		RWORK, 1, 0
	);
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( X, 2 * n ), tc.X, 1e-12, 'X' );
	assert.ok( rcond[ 0 ] > 0.0, 'rcond > 0' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-10, 'rcond' );
});

test( 'zsysvx: lower 4x4, 2 RHS, not-factored', function t() {
	var tc = lower_4x4_2rhs;
	var n = 4;
	var nrhs = 2;
	var A = new Complex128Array( new Float64Array( tc.A ) );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var B = new Complex128Array( new Float64Array( tc.B ) );
	var X = new Complex128Array( n * nrhs );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var WORK = new Complex128Array( 2 * n );
	var RWORK = new Float64Array( n );

	var info = zsysvx(
		'not-factored', 'lower', n, nrhs,
		A, 1, n, 0,
		AF, 1, n, 0,
		IPIV, 1, 0,
		B, 1, n, 0,
		X, 1, n, 0,
		rcond,
		FERR, 1, 0,
		BERR, 1, 0,
		WORK, 1, 0, 2 * n,
		RWORK, 1, 0
	);
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( X, 2 * n * nrhs ), tc.X, 1e-12, 'X' );
	assert.ok( rcond[ 0 ] > 0.0, 'rcond > 0' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-10, 'rcond' );
});

test( 'zsysvx: N=0 quick return', function t() {
	var A = new Complex128Array( 1 );
	var AF = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	var B = new Complex128Array( 1 );
	var X = new Complex128Array( 1 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Complex128Array( 1 );
	var RWORK = new Float64Array( 1 );

	var info = zsysvx(
		'not-factored', 'upper', 0, 1,
		A, 1, 1, 0,
		AF, 1, 1, 0,
		IPIV, 1, 0,
		B, 1, 1, 0,
		X, 1, 1, 0,
		rcond,
		FERR, 1, 0,
		BERR, 1, 0,
		WORK, 1, 0, 1,
		RWORK, 1, 0
	);
	assert.strictEqual( info, 0, 'info' );
});
