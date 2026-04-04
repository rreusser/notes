'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgttrf = require( './../../zgttrf/lib/base.js' );
var zgtcon = require( './../lib/base.js' );

// FIXTURES //

var tridiag_1norm = require( './fixtures/tridiag_1norm.json' );
var tridiag_inorm = require( './fixtures/tridiag_inorm.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var anorm_zero = require( './fixtures/anorm_zero.json' );
var complex_4x4_1norm = require( './fixtures/complex_4x4_1norm.json' );

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function condTridiag( norm, n, dlArr, dArr, duArr, anorm ) {
	var dl = new Complex128Array( dlArr.length / 2 );
	var d = new Complex128Array( dArr.length / 2 );
	var du = new Complex128Array( duArr.length / 2 );
	var du2 = new Complex128Array( Math.max( n - 2, 0 ) );
	var WORK = new Complex128Array( 2 * n );
	var ipiv = new Int32Array( n );
	var rcond = new Float64Array( 1 );
	var dlv = reinterpret( dl, 0 );
	var dv = reinterpret( d, 0 );
	var duv = reinterpret( du, 0 );
	var info;
	var i;

	for ( i = 0; i < dlArr.length; i++ ) { dlv[ i ] = dlArr[ i ]; }
	for ( i = 0; i < dArr.length; i++ ) { dv[ i ] = dArr[ i ]; }
	for ( i = 0; i < duArr.length; i++ ) { duv[ i ] = duArr[ i ]; }

	info = zgttrf( n, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'zgttrf info' );

	info = zgtcon( norm, n, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0, anorm, rcond, WORK, 1, 0 );
	assert.equal( info, 0, 'zgtcon info' );

	return rcond[ 0 ];
}

test( 'zgtcon: tridiag 1-norm (5x5)', function t() {
	var tc = tridiag_1norm;
	var rcond = condTridiag( 'one-norm', 5,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 4, 1, 4, 1, 4, 1, 4, 1, 4, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		tc.anorm
	);
	assertClose( rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgtcon: tridiag infinity-norm (5x5)', function t() {
	var tc = tridiag_inorm;
	var rcond = condTridiag( 'infinity-norm', 5,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 4, 1, 4, 1, 4, 1, 4, 1, 4, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		tc.anorm
	);
	assertClose( rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgtcon: N=0 (rcond=1)', function t() {
	var tc = n_zero;
	var WORK = new Complex128Array( 0 );
	var rcond = new Float64Array( 1 );
	var info = zgtcon( 'one-norm', 0, new Complex128Array(0), 1, 0, new Complex128Array(0), 1, 0,
		new Complex128Array(0), 1, 0, new Complex128Array(0), 1, 0, new Int32Array(0), 1, 0,
		0.0, rcond, WORK, 1, 0 );
	assert.equal( info, 0 );
	assert.equal( rcond[ 0 ], 1.0 );
});

test( 'zgtcon: N=1', function t() {
	var tc = n_one;
	var rcond = condTridiag( 'one-norm', 1, [], [ 3, 1 ], [], 4.0 );
	assertClose( rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgtcon: anorm=0 (rcond=0)', function t() {
	var tc = anorm_zero;
	var rcond = condTridiag( 'one-norm', 5,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 4, 1, 4, 1, 4, 1, 4, 1, 4, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		0.0
	);
	assert.equal( rcond, 0.0 );
});

test( 'zgtcon: complex 4x4 1-norm', function t() {
	var tc = complex_4x4_1norm;
	var rcond = condTridiag( 'one-norm', 4,
		[ 2, 1, 1, 3, 0.5, 0.5 ],
		[ 5, 2, 6, 1, 7, 3, 4, 2 ],
		[ 1, 0.5, 2, 1, 1.5, 0.5 ],
		tc.anorm
	);
	assertClose( rcond, tc.rcond, 1e-10, 'rcond' );
});
