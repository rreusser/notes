/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrsyl = require( './../lib/ndarray.js' );

// FIXTURES //

var nn_basic_2x2 = require( './fixtures/nn_basic_2x2.json' );
var nn_isgn__1 = require( './fixtures/nn_isgn_-1.json' );
var tn_basic = require( './fixtures/tn_basic.json' );
var tt_basic = require( './fixtures/tt_basic.json' );
var nt_basic = require( './fixtures/nt_basic.json' );
var m_0 = require( './fixtures/m_0.json' );
var n_0 = require( './fixtures/n_0.json' );
var nn_3x3_quasi_tri = require( './fixtures/nn_3x3_quasi-tri.json' );
var m_1_n_1 = require( './fixtures/m_1_n_1.json' );
var tn_3x3_quasi_tri = require( './fixtures/tn_3x3_quasi-tri.json' );
var tt_3x3_quasi_tri = require( './fixtures/tt_3x3_quasi-tri.json' );
var nt_3x3_quasi_tri = require( './fixtures/nt_3x3_quasi-tri.json' );
var nn_4x4_quasi_tri_both = require( './fixtures/nn_4x4_quasi-tri_both.json' );
var tn_4x4_quasi_tri_both = require( './fixtures/tn_4x4_quasi-tri_both.json' );
var tt_4x4_quasi_tri_both = require( './fixtures/tt_4x4_quasi-tri_both.json' );
var nt_4x4_quasi_tri_both = require( './fixtures/nt_4x4_quasi-tri_both.json' );
var nn_4x4_quasi_tri_both_isgn__1 = require( './fixtures/nn_4x4_quasi-tri_both_isgn_-1.json' );
var tn_4x4_quasi_tri_both_isgn__1 = require( './fixtures/tn_4x4_quasi-tri_both_isgn_-1.json' );
var tt_4x4_quasi_tri_both_isgn__1 = require( './fixtures/tt_4x4_quasi-tri_both_isgn_-1.json' );
var nt_4x4_quasi_tri_both_isgn__1 = require( './fixtures/nt_4x4_quasi-tri_both_isgn_-1.json' );

// FUNCTIONS //

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
* Pack an MxN submatrix from a column-major LDA array into a dense M*N column-major array.
*/
function packMatrix( C, LDA, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( C[ i + j * LDA ] );
		}
	}
	return out;
}

/**
* Build the 4x4 test matrix A with 2x2 blocks at (1,1)-(2,2) and (3,3)-(4,4).
* A(1,1)=1.0, A(1,2)=0.5, A(2,1)=-0.5, A(2,2)=1.0
* A(3,3)=3.0, A(3,4)=0.4, A(4,3)=-0.6, A(4,4)=3.0
* Upper part: A(1,3)=0.3, A(1,4)=0.1, A(2,3)=0.2, A(2,4)=0.05
*/
function buildA4( LDA ) {
	var A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0; A[ 0 + 1*LDA ] = 0.5; A[ 0 + 2*LDA ] = 0.3; A[ 0 + 3*LDA ] = 0.1; // eslint-disable-line max-len
	A[ 1 + 0*LDA ] = -0.5; A[ 1 + 1*LDA ] = 1.0; A[ 1 + 2*LDA ] = 0.2; A[ 1 + 3*LDA ] = 0.05; // eslint-disable-line max-len
	A[ 2 + 2*LDA ] = 3.0; A[ 2 + 3*LDA ] = 0.4;
	A[ 3 + 2*LDA ] = -0.6; A[ 3 + 3*LDA ] = 3.0;
	return A;
}

/**
* Build the 4x4 test matrix B with a 2x2 block at (3,3)-(4,4).
* B(1,1)=2.0, B(1,2)=0.1, B(1,3)=0.2, B(1,4)=0.05
* B(2,2)=4.0, B(2,3)=0.3, B(2,4)=0.1
* B(3,3)=5.0, B(3,4)=0.7, B(4,3)=-0.7, B(4,4)=5.0
*/
function buildB4( LDA ) {
	var B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0; B[ 0 + 1*LDA ] = 0.1; B[ 0 + 2*LDA ] = 0.2; B[ 0 + 3*LDA ] = 0.05; // eslint-disable-line max-len
	B[ 1 + 1*LDA ] = 4.0; B[ 1 + 2*LDA ] = 0.3; B[ 1 + 3*LDA ] = 0.1;
	B[ 2 + 2*LDA ] = 5.0; B[ 2 + 3*LDA ] = 0.7;
	B[ 3 + 2*LDA ] = -0.7; B[ 3 + 3*LDA ] = 5.0;
	return B;
}

/**
* BuildC4.
*
* @private
* @param {*} LDA - LDA
* @returns {*} result
*/
function buildC4( LDA ) {
	var C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0; C[ 0 + 1*LDA ] = 2.0; C[ 0 + 2*LDA ] = 3.0; C[ 0 + 3*LDA ] = 4.0; // eslint-disable-line max-len
	C[ 1 + 0*LDA ] = 5.0; C[ 1 + 1*LDA ] = 6.0; C[ 1 + 2*LDA ] = 7.0; C[ 1 + 3*LDA ] = 8.0; // eslint-disable-line max-len
	C[ 2 + 0*LDA ] = 9.0; C[ 2 + 1*LDA ] = 10.0; C[ 2 + 2*LDA ] = 11.0; C[ 2 + 3*LDA ] = 12.0; // eslint-disable-line max-len
	C[ 3 + 0*LDA ] = 13.0; C[ 3 + 1*LDA ] = 14.0; C[ 3 + 2*LDA ] = 15.0; C[ 3 + 3*LDA ] = 16.0; // eslint-disable-line max-len
	return C;
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

test( 'dtrsyl: NN basic 2x2', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nn_basic_2x2;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0;
	B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0;
	C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0;
	C[ 1 + 1*LDA ] = 8.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: NN isgn=-1', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nn_isgn__1;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0;
	B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0;
	C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0;
	C[ 1 + 1*LDA ] = 8.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', -1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TN basic', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tn_basic;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0;
	B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0;
	C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0;
	C[ 1 + 1*LDA ] = 8.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'no-transpose', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TT basic', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tt_basic;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0;
	B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0;
	C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0;
	C[ 1 + 1*LDA ] = 8.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'transpose', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: NT basic', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nt_basic;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 2.0;
	A[ 1 + 1*LDA ] = 3.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 2.0;
	B[ 0 + 1*LDA ] = 1.0;
	B[ 1 + 1*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 5.0;
	C[ 0 + 1*LDA ] = 6.0;
	C[ 1 + 0*LDA ] = 7.0;
	C[ 1 + 1*LDA ] = 8.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'transpose', 1, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: M=0', function t() {
	var scale;
	var info;
	var tc;
	var A;
	var B;
	var C;

	tc = m_0;
	A = new Float64Array( 16 );
	B = new Float64Array( 16 );
	C = new Float64Array( 16 );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', 1, 0, 2, A, 1, 4, 0, B, 1, 4, 0, C, 1, 4, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: N=0', function t() {
	var scale;
	var info;
	var tc;
	var A;
	var B;
	var C;

	tc = n_0;
	A = new Float64Array( 16 );
	B = new Float64Array( 16 );
	C = new Float64Array( 16 );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', 1, 2, 0, A, 1, 4, 0, B, 1, 4, 0, C, 1, 4, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: NN 3x3 quasi-tri', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nn_3x3_quasi_tri;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 0.5;
	A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0;
	A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5;
	A[ 2 + 2*LDA ] = 2.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0;
	B[ 0 + 1*LDA ] = 0.2;
	B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0;
	B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3;
	B[ 2 + 2*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0;
	C[ 0 + 1*LDA ] = 2.0;
	C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0;
	C[ 1 + 1*LDA ] = 5.0;
	C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0;
	C[ 2 + 1*LDA ] = 8.0;
	C[ 2 + 2*LDA ] = 9.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: M=1 N=1', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = m_1_n_1;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 ] = 2.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 ] = 3.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 ] = 10.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', 1, 1, 1, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TN 3x3 quasi-tri', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tn_3x3_quasi_tri;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 0.5;
	A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0;
	A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5;
	A[ 2 + 2*LDA ] = 2.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0;
	B[ 0 + 1*LDA ] = 0.2;
	B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0;
	B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3;
	B[ 2 + 2*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0;
	C[ 0 + 1*LDA ] = 2.0;
	C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0;
	C[ 1 + 1*LDA ] = 5.0;
	C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0;
	C[ 2 + 1*LDA ] = 8.0;
	C[ 2 + 2*LDA ] = 9.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'no-transpose', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: TT 3x3 quasi-tri', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tt_3x3_quasi_tri;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 0.5;
	A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0;
	A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5;
	A[ 2 + 2*LDA ] = 2.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0;
	B[ 0 + 1*LDA ] = 0.2;
	B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0;
	B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3;
	B[ 2 + 2*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0;
	C[ 0 + 1*LDA ] = 2.0;
	C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0;
	C[ 1 + 1*LDA ] = 5.0;
	C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0;
	C[ 2 + 1*LDA ] = 8.0;
	C[ 2 + 2*LDA ] = 9.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'transpose', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

test( 'dtrsyl: NT 3x3 quasi-tri', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nt_3x3_quasi_tri;
	LDA = 4;
	A = new Float64Array( LDA * LDA );
	A[ 0 + 0*LDA ] = 1.0;
	A[ 0 + 1*LDA ] = 0.5;
	A[ 0 + 2*LDA ] = 0.3;
	A[ 1 + 1*LDA ] = 2.0;
	A[ 1 + 2*LDA ] = 0.4;
	A[ 2 + 1*LDA ] = -0.5;
	A[ 2 + 2*LDA ] = 2.0;
	B = new Float64Array( LDA * LDA );
	B[ 0 + 0*LDA ] = 3.0;
	B[ 0 + 1*LDA ] = 0.2;
	B[ 0 + 2*LDA ] = 0.1;
	B[ 1 + 1*LDA ] = 4.0;
	B[ 1 + 2*LDA ] = 0.3;
	B[ 2 + 1*LDA ] = -0.3;
	B[ 2 + 2*LDA ] = 4.0;
	C = new Float64Array( LDA * LDA );
	C[ 0 + 0*LDA ] = 1.0;
	C[ 0 + 1*LDA ] = 2.0;
	C[ 0 + 2*LDA ] = 3.0;
	C[ 1 + 0*LDA ] = 4.0;
	C[ 1 + 1*LDA ] = 5.0;
	C[ 1 + 2*LDA ] = 6.0;
	C[ 2 + 0*LDA ] = 7.0;
	C[ 2 + 1*LDA ] = 8.0;
	C[ 2 + 2*LDA ] = 9.0;
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'transpose', 1, 3, 3, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( C ), tc.C, 1e-14, 'C' );
});

// 4x4 tests with 2x2 blocks in BOTH A and B

test( 'dtrsyl: NN 4x4 quasi-tri both', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nn_4x4_quasi_tri_both;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', 1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: TN 4x4 quasi-tri both', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tn_4x4_quasi_tri_both;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'no-transpose', 1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: TT 4x4 quasi-tri both', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tt_4x4_quasi_tri_both;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'transpose', 1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: NT 4x4 quasi-tri both', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nt_4x4_quasi_tri_both;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'transpose', 1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: NN 4x4 quasi-tri both isgn=-1', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nn_4x4_quasi_tri_both_isgn__1;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'no-transpose', -1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: TN 4x4 quasi-tri both isgn=-1', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tn_4x4_quasi_tri_both_isgn__1;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'no-transpose', -1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: TT 4x4 quasi-tri both isgn=-1', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = tt_4x4_quasi_tri_both_isgn__1;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'transpose', 'transpose', -1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});

test( 'dtrsyl: NT 4x4 quasi-tri both isgn=-1', function t() {
	var scale;
	var info;
	var LDA;
	var tc;
	var A;
	var B;
	var C;

	tc = nt_4x4_quasi_tri_both_isgn__1;
	LDA = 4;
	A = buildA4( LDA );
	B = buildB4( LDA );
	C = buildC4( LDA );
	scale = new Float64Array( 1 );
	info = dtrsyl( 'no-transpose', 'transpose', -1, 4, 4, A, 1, LDA, 0, B, 1, LDA, 0, C, 1, LDA, 0, scale ); // eslint-disable-line max-len
	assert.strictEqual( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( packMatrix( C, LDA, 4, 4 ), tc.C, 1e-12, 'C' );
});
