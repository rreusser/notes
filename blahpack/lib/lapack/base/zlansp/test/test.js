/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansp = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlansp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) {
		return t.name === name;
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


// TESTS //

// 3x3 complex symmetric matrix:
//   A = [ (2,1)   (1,2)   (3,-1) ]
//       [ (1,2)   (5,-1)  (2,1)  ]
//       [ (3,-1)  (2,1)   (4,2)  ]

// Upper packed (col-major): (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2)
// Lower packed (col-major): (2,1),(1,2),(3,-1),(5,-1),(2,1),(4,2)

test( 'zlansp is a function', function t() {
	assert.strictEqual( typeof zlansp, 'function' );
});

test( 'zlansp: zlansp_3x3_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_max_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'max', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assert.strictEqual( typeof result, 'number', 'returns a number' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_one_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'one-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_inf_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'inf-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_frob_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'frobenius', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_max_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'max', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_one_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'one-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_inf_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'inf-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_3x3_frob_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'frobenius', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 complex symmetric matrix:

//   A = [ (2,1)      (1,2)    (3,-1)   (0.5,0.5) ]

//       [ (1,2)      (5,-1)   (2,1)    (1,-2)    ]

//       [ (3,-1)     (2,1)    (4,2)    (3,0)     ]

//       [ (0.5,0.5)  (1,-2)   (3,0)    (6,-3)    ]

// Upper packed: (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2),(0.5,0.5),(1,-2),(3,0),(6,-3) // eslint-disable-line max-len

// Lower packed: (2,1),(1,2),(3,-1),(0.5,0.5),(5,-1),(2,1),(1,-2),(4,2),(3,0),(6,-3) // eslint-disable-line max-len

test( 'zlansp: zlansp_4x4_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_max_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'max', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_one_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'one-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_inf_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'inf-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_frob_U' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'frobenius', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_max_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'max', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_one_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'one-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_inf_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'inf-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_4x4_frob_L' );
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'frobenius', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=0 quick return
test( 'zlansp: zlansp_n0', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_n0' );
	ap = new Complex128Array( 0 );
	work = new Float64Array( 0 );
	result = zlansp( 'max', 'upper', 0, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1 matrix, single element = (3.0, 4.0) => |z| = 5.0
test( 'zlansp: zlansp_1x1_max', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_1x1_max' );
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'max', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_1x1_one', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_1x1_one' );
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'one-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_1x1_inf', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_1x1_inf' );
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'inf-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_1x1_frob', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'zlansp_1x1_frob' );
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'frobenius', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
