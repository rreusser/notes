/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlamrg = require( './../lib/base.js' );

// FIXTURES //

var fwd_fwd_3_3 = require( './fixtures/fwd_fwd_3_3.json' );
var fwd_bwd_3_3 = require( './fixtures/fwd_bwd_3_3.json' );
var bwd_fwd_3_2 = require( './fixtures/bwd_fwd_3_2.json' );
var bwd_bwd_2_3 = require( './fixtures/bwd_bwd_2_3.json' );
var n1_n1 = require( './fixtures/n1_n1.json' );
var equal = require( './fixtures/equal.json' );
var n4_n1 = require( './fixtures/n4_n1.json' );

// TESTS //

test( 'dlamrg: both forward, 3+3', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = fwd_fwd_3_3;
	a = new Float64Array( [ 1.0, 3.0, 5.0, 2.0, 4.0, 6.0 ] );
	idx = new Int32Array( 6 );
	dlamrg( 3, 3, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 6; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: first forward, second backward, 3+3', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = fwd_bwd_3_3;
	a = new Float64Array( [ 1.0, 3.0, 5.0, 6.0, 4.0, 2.0 ] );
	idx = new Int32Array( 6 );
	dlamrg( 3, 3, a, 1, 0, 1, -1, idx, 1, 0 );
	for ( i = 0; i < 6; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: first backward, second forward, 3+2', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = bwd_fwd_3_2;
	a = new Float64Array( [ 5.0, 3.0, 1.0, 2.0, 4.0 ] );
	idx = new Int32Array( 5 );
	dlamrg( 3, 2, a, 1, 0, -1, 1, idx, 1, 0 );
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: both backward, 2+3', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = bwd_bwd_2_3;
	a = new Float64Array( [ 4.0, 2.0, 6.0, 3.0, 1.0 ] );
	idx = new Int32Array( 5 );
	dlamrg( 2, 3, a, 1, 0, -1, -1, idx, 1, 0 );
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: n1=1, n1=1', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = n1_n1;
	a = new Float64Array( [ 3.0, 1.0 ] );
	idx = new Int32Array( 2 );
	dlamrg( 1, 1, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: equal elements', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = equal;
	a = new Float64Array( [ 2.0, 2.0, 2.0, 2.0 ] );
	idx = new Int32Array( 4 );
	dlamrg( 2, 2, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: n1=4, n2=1', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = n4_n1;
	a = new Float64Array( [ 1.0, 3.0, 5.0, 7.0, 4.0 ] );
	idx = new Int32Array( 5 );
	dlamrg( 4, 1, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});
