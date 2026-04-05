'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansy = require( './../lib/base.js' );

// FIXTURES //

var max_upper = require( './fixtures/max_upper.json' );
var one_upper = require( './fixtures/one_upper.json' );
var inf_upper = require( './fixtures/inf_upper.json' );
var fro_upper = require( './fixtures/fro_upper.json' );
var one_lower = require( './fixtures/one_lower.json' );
var fro_lower = require( './fixtures/fro_lower.json' );

// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

// Upper triangle of 4x4 symmetric matrix, stored column-major:
// A(1,1)=(2,1) A(1,2)=(1,2) A(1,3)=(3,-1) A(1,4)=(0.5,0.5)
//              A(2,2)=(5,-1) A(2,3)=(2,1)  A(2,4)=(1,-2)
//                            A(3,3)=(4,2)  A(3,4)=(3,0)
//                                          A(4,4)=(6,-3)
function makeUpperA() {
	// Column-major 4x4 complex, interleaved re/im
	return new Complex128Array( new Float64Array( [
		2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 2.0, 5.0, -1.0, 0.0, 0.0, 0.0, 0.0,
		3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.0, 0.0,
		0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0
	] ) );
}

// Lower triangle version (same matrix, lower stored)
function makeLowerA() {
	return new Complex128Array( new Float64Array( [
		2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5,
		0.0, 0.0, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0,
		0.0, 0.0, 0.0, 0.0, 4.0, 2.0, 3.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 6.0, -3.0
	] ) );
}

// TESTS //

test( 'zlansy: max norm, upper', function t() {
	var tc = max_upper;
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'max', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: one norm, upper', function t() {
	var tc = one_upper;
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'one-norm', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: inf norm, upper', function t() {
	var tc = inf_upper;
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'inf-norm', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: frobenius norm, upper', function t() {
	var tc = fro_upper;
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'frobenius', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: one norm, lower', function t() {
	var tc = one_lower;
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'one-norm', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: frobenius norm, lower', function t() {
	var tc = fro_lower;
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'frobenius', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: max norm, lower', function t() {
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'max', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	// Same symmetric matrix, max norm should be same as upper
	var tc = max_upper;
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: inf norm, lower', function t() {
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'inf-norm', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	// For symmetric matrix, one-norm == inf-norm
	var tc = one_lower;
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: N=0', function t() {
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'max', 'upper', 0, A, 1, 4, 0, WORK, 1, 0 );
	assert.strictEqual( result, 0.0, 'N=0 returns 0' );
});
