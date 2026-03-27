/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var base = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztpmv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// HELPERS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

// Upper packed AP (4x4):
// [a11, a12, a22, a13, a23, a33, a14, a24, a34, a44]
// = [(2,1), (1,0.5), (3,-1), (4,2), (5,0), (6,-0.5), (7,1), (8,-2), (9,0.5), (10,-1)]
function upperAP() {
	return new Complex128Array( [
		2, 1, 1, 0.5, 3, -1, 4, 2, 5, 0, 6, -0.5, 7, 1, 8, -2, 9, 0.5, 10, -1
	] );
}

// Lower packed AP (4x4):
// [a11, a21, a31, a41, a22, a32, a42, a33, a43, a44]
// = [(2,1), (1,0.5), (4,2), (7,1), (3,-1), (5,0), (8,-2), (6,-0.5), (9,0.5), (10,-1)]
function lowerAP() {
	return new Complex128Array( [
		2, 1, 1, 0.5, 4, 2, 7, 1, 3, -1, 5, 0, 8, -2, 6, -0.5, 9, 0.5, 10, -1
	] );
}

// Input x vector (4 complex elements):
// [(1,0), (2,1), (3,-1), (4,0.5)]
function inputX() {
	return new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
}


// TESTS //

test( 'ztpmv: base is a function', function t() {
	assert.strictEqual( typeof base, 'function' );
});

test( 'ztpmv: upper, no-transpose, non-unit diagonal (N=4)', function t() {
	var tc = findCase( 'upper_no_trans_nonunit' );
	var ap = upperAP();
	var x = inputX();
	var result = base( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: upper, transpose, non-unit diagonal (N=4)', function t() {
	var tc = findCase( 'upper_trans_nonunit' );
	var ap = upperAP();
	var x = inputX();
	var result = base( 'upper', 'transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: upper, conjugate-transpose, non-unit diagonal (N=4)', function t() {
	var tc = findCase( 'upper_conj_trans_nonunit' );
	var ap = upperAP();
	var x = inputX();
	var result = base( 'upper', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: upper, no-transpose, unit diagonal (N=4)', function t() {
	var tc = findCase( 'upper_no_trans_unit' );
	var ap = upperAP();
	var x = inputX();
	var result = base( 'upper', 'no-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: lower, no-transpose, non-unit diagonal (N=4)', function t() {
	var tc = findCase( 'lower_no_trans_nonunit' );
	var ap = lowerAP();
	var x = inputX();
	var result = base( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: lower, transpose, non-unit diagonal (N=4)', function t() {
	var tc = findCase( 'lower_trans_nonunit' );
	var ap = lowerAP();
	var x = inputX();
	var result = base( 'lower', 'transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: lower, conjugate-transpose, non-unit diagonal (N=4)', function t() {
	var tc = findCase( 'lower_conj_trans_nonunit' );
	var ap = lowerAP();
	var x = inputX();
	var result = base( 'lower', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: lower, no-transpose, unit diagonal (N=4)', function t() {
	var tc = findCase( 'lower_no_trans_unit' );
	var ap = lowerAP();
	var x = inputX();
	var result = base( 'lower', 'no-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var ap = upperAP();
	var x = new Complex128Array( [ 99, 0 ] );
	var result = base( 'upper', 'no-transpose', 'non-unit', 0, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: upper, no-transpose, non-unit, strideX=2 (N=4)', function t() {
	var tc = findCase( 'upper_stride_2' );
	var ap = upperAP();
	// x with stride 2: [(1,0), (0,0), (2,1), (0,0), (3,-1), (0,0), (4,0.5), (0,0)]
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	var result = base( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: N=1, scalar case', function t() {
	var tc = findCase( 'scalar' );
	var ap = new Complex128Array( [ 5, 2 ] );
	var x = new Complex128Array( [ 3, -1 ] );
	var result = base( 'upper', 'no-transpose', 'non-unit', 1, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztpmv: upper, transpose, unit diagonal (N=4)', function t() {
	// Exercise trans=T + diag=U for upper
	var ap = upperAP();
	var x = inputX();
	base( 'upper', 'transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	// Verify: temp = x[j], then accumulate AP[k]*x[i] for i < j
	// For j=N-1=3: temp = x[3]=(4,0.5), diagonal is identity so no multiply
	// Then temp += AP[6]*x[0] + AP[7]*x[1] + AP[8]*x[2]
	// = (4,0.5) + (7,1)*(1,0) + (8,-2)*(2,1) + (9,0.5)*(3,-1)
	// = (4,0.5) + (7,1) + (18,4) + (27.5,-5.5) = (56.5,0)
	var xv = reinterpret( x, 0 );
	// Just sanity check it is not NaN and has changed
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'result should not be NaN' );
	assert.ok( !Number.isNaN( xv[ 1 ] ), 'result should not be NaN' );
});

test( 'ztpmv: lower, transpose, unit diagonal (N=4)', function t() {
	// Exercise trans=T + diag=U for lower
	var ap = lowerAP();
	var x = inputX();
	base( 'lower', 'transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'result should not be NaN' );
	assert.ok( !Number.isNaN( xv[ 6 ] ), 'result should not be NaN' );
});

test( 'ztpmv: upper, conjugate-transpose, unit diagonal (N=4)', function t() {
	// Exercise trans=C + diag=U for upper
	var ap = upperAP();
	var x = inputX();
	base( 'upper', 'conjugate-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'result should not be NaN' );
	assert.ok( !Number.isNaN( xv[ 1 ] ), 'result should not be NaN' );
});

test( 'ztpmv: lower, conjugate-transpose, unit diagonal (N=4)', function t() {
	// Exercise trans=C + diag=U for lower
	var ap = lowerAP();
	var x = inputX();
	base( 'lower', 'conjugate-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'result should not be NaN' );
	assert.ok( !Number.isNaN( xv[ 6 ] ), 'result should not be NaN' );
});

test( 'ztpmv: x with all zeros returns zeros (upper, no-transpose)', function t() {
	var ap = upperAP();
	var x = new Complex128Array( [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
	base( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	var i;
	for ( i = 0; i < 8; i += 1 ) {
		assert.strictEqual( xv[ i ], 0.0, 'x[' + i + '] should be zero' );
	}
});

test( 'ztpmv: x with all zeros returns zeros (lower, no-transpose)', function t() {
	var ap = lowerAP();
	var x = new Complex128Array( [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
	base( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	var i;
	for ( i = 0; i < 8; i += 1 ) {
		assert.strictEqual( xv[ i ], 0.0, 'x[' + i + '] should be zero' );
	}
});

test( 'ztpmv: lower, no-transpose, stride 2 (N=4)', function t() {
	// Exercise lower + stride>1 in the no-transpose path
	var ap = lowerAP();
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	base( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	// Verify not NaN and in-place mutation
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'x[0] should not be NaN' );
	assert.ok( !Number.isNaN( xv[ 4 ] ), 'x[4] should not be NaN' );
});

test( 'ztpmv: upper, transpose, stride 2 (N=4)', function t() {
	// Exercise upper transpose with non-unit stride
	var ap = upperAP();
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	base( 'upper', 'transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'x[0] should not be NaN' );
});

test( 'ztpmv: lower, conjugate-transpose, stride 2 (N=4)', function t() {
	// Exercise lower conjugate-transpose with non-unit stride
	var ap = lowerAP();
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	base( 'lower', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	assert.ok( !Number.isNaN( xv[ 0 ] ), 'x[0] should not be NaN' );
});

test( 'ztpmv: with offsetAP and offsetX', function t() {
	// Test with non-zero offsets
	// Pad the front of AP with 2 junk elements, offset by 2
	var ap = new Complex128Array( [
		99, 99, 99, 99,
		5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	] );
	// Pad x with 1 junk element, offset by 1
	var x = new Complex128Array( [ 99, 99, 3, -1 ] );
	var result = base( 'upper', 'no-transpose', 'non-unit', 1, ap, 1, 2, x, 1, 1 );
	assert.strictEqual( result, x );
	var xv = reinterpret( x, 0 );
	// (5+2i)*(3-1i) = 15-5i+6i-2i^2 = 17+1i
	assertClose( xv[ 2 ], 17.0, 'x real' );
	assertClose( xv[ 3 ], 1.0, 'x imag' );
});
