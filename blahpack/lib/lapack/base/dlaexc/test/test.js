'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaexc = require( './../lib/base.js' );

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

test( 'dlaexc: swap two 1x1 blocks', function t() {
	var N = 3;
	// Upper triangular: diag = [4, 2, 1]
	var T = new Float64Array( N * N );
	T[ 0 + 0*N ] = 4.0; T[ 0 + 1*N ] = 1.0; T[ 0 + 2*N ] = 0.5;
	T[ 1 + 1*N ] = 2.0; T[ 1 + 2*N ] = 0.8;
	T[ 2 + 2*N ] = 1.0;

	var Q = new Float64Array( N * N );
	for ( var i = 0; i < N; i++ ) Q[ i + i*N ] = 1.0;

	var WORK = new Float64Array( N );

	// Swap blocks at j1=1 (1-based), n1=1, n2=1 -> swap T(1,1)=4 with T(2,2)=2
	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 1, 1, 1, WORK, 1, 0 );

	assert.strictEqual( info, 0, 'info' );
	// After swap, diagonal should be [2, 4, 1]
	assertClose( T[ 0 ], 2.0, 1e-12, 'T(1,1)' );
	assertClose( T[ 1 + 1*N ], 4.0, 1e-12, 'T(2,2)' );
	assertClose( T[ 2 + 2*N ], 1.0, 1e-12, 'T(3,3)' );
});

test( 'dlaexc: quick return N=0', function t() {
	var info = dlaexc( true, 0, new Float64Array(0), 1, 0, 0, new Float64Array(0), 1, 0, 0, 1, 1, 1, new Float64Array(0), 1, 0 );
	assert.strictEqual( info, 0, 'info' );
});
