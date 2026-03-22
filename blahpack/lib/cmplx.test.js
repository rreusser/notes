'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var cmplx = require( './cmplx.js' );

var z = new Float64Array( 2 );

test( 'cmplx: set/real/imag', function t() {
	cmplx.set( z, 3, 4 );
	assert.equal( cmplx.real( z ), 3 );
	assert.equal( cmplx.imag( z ), 4 );
});

test( 'cmplx: add: (1+2i) + (3+4i) = (4+6i)', function t() {
	var a = new Float64Array( [ 1, 2 ] );
	var b = new Float64Array( [ 3, 4 ] );
	cmplx.add( z, a, b );
	assert.equal( z[ 0 ], 4 );
	assert.equal( z[ 1 ], 6 );
});

test( 'cmplx: sub: (5+3i) - (2+1i) = (3+2i)', function t() {
	cmplx.sub( z, new Float64Array([ 5, 3 ]), new Float64Array([ 2, 1 ]) );
	assert.equal( z[ 0 ], 3 );
	assert.equal( z[ 1 ], 2 );
});

test( 'cmplx: mul: (1+2i) * (3+4i) = (-5+10i)', function t() {
	cmplx.mul( z, new Float64Array([ 1, 2 ]), new Float64Array([ 3, 4 ]) );
	assert.equal( z[ 0 ], -5 );
	assert.equal( z[ 1 ], 10 );
});

test( 'cmplx: mul: (0+1i) * (0+1i) = (-1+0i)', function t() {
	cmplx.mul( z, new Float64Array([ 0, 1 ]), new Float64Array([ 0, 1 ]) );
	assert.equal( z[ 0 ], -1 );
	assert.equal( z[ 1 ], 0 );
});

test( 'cmplx: div: (4+2i) / (1+1i) = (3-1i)', function t() {
	cmplx.div( z, new Float64Array([ 4, 2 ]), new Float64Array([ 1, 1 ]) );
	assert.ok( Math.abs( z[ 0 ] - 3 ) < 1e-15 );
	assert.ok( Math.abs( z[ 1 ] - (-1) ) < 1e-15 );
});

test( 'cmplx: div: (1+0i) / (0+1i) = (0-1i)', function t() {
	cmplx.div( z, new Float64Array([ 1, 0 ]), new Float64Array([ 0, 1 ]) );
	assert.ok( Math.abs( z[ 0 ] ) < 1e-15 );
	assert.ok( Math.abs( z[ 1 ] - (-1) ) < 1e-15 );
});

test( 'cmplx: conj: conj(3+4i) = (3-4i)', function t() {
	cmplx.conj( z, new Float64Array([ 3, 4 ]) );
	assert.equal( z[ 0 ], 3 );
	assert.equal( z[ 1 ], -4 );
});

test( 'cmplx: neg: -(2+3i) = (-2-3i)', function t() {
	cmplx.neg( z, new Float64Array([ 2, 3 ]) );
	assert.equal( z[ 0 ], -2 );
	assert.equal( z[ 1 ], -3 );
});

test( 'cmplx: scale: 3 * (2+1i) = (6+3i)', function t() {
	cmplx.scale( z, new Float64Array([ 2, 1 ]), 3 );
	assert.equal( z[ 0 ], 6 );
	assert.equal( z[ 1 ], 3 );
});

test( 'cmplx: abs: |3+4i| = 5', function t() {
	assert.equal( cmplx.abs( new Float64Array([ 3, 4 ]) ), 5 );
});

test( 'cmplx: abs: |0+0i| = 0', function t() {
	assert.equal( cmplx.abs( new Float64Array([ 0, 0 ]) ), 0 );
});

test( 'cmplx: abs: |-5+0i| = 5', function t() {
	assert.equal( cmplx.abs( new Float64Array([ -5, 0 ]) ), 5 );
});

test( 'cmplx: abs1: |3| + |4| = 7', function t() {
	assert.equal( cmplx.abs1( new Float64Array([ 3, -4 ]) ), 7 );
});

test( 'cmplx: copy', function t() {
	var a = new Float64Array([ 7, 8 ]);
	cmplx.copy( z, a );
	assert.equal( z[ 0 ], 7 );
	assert.equal( z[ 1 ], 8 );
});

test( 'cmplx: eq and iszero', function t() {
	assert.ok( cmplx.eq( new Float64Array([ 1, 2 ]), new Float64Array([ 1, 2 ]) ) );
	assert.ok( !cmplx.eq( new Float64Array([ 1, 2 ]), new Float64Array([ 1, 3 ]) ) );
	assert.ok( cmplx.iszero( new Float64Array([ 0, 0 ]) ) );
	assert.ok( !cmplx.iszero( new Float64Array([ 0, 1 ]) ) );
});

test( 'cmplx: madd: a + s*b (real scalar multiply-add)', function t() {
	cmplx.madd( z, new Float64Array([ 1, 2 ]), 3, new Float64Array([ 4, 5 ]) );
	assert.equal( z[ 0 ], 13 );
	assert.equal( z[ 1 ], 17 );
});

test( 'cmplx: mmadd: a + b*c (complex multiply-add)', function t() {
	// (1+0i) + (1+2i)*(3+4i) = (1+0i) + (-5+10i) = (-4+10i)
	cmplx.mmadd( z, new Float64Array([ 1, 0 ]), new Float64Array([ 1, 2 ]), new Float64Array([ 3, 4 ]) );
	assert.equal( z[ 0 ], -4 );
	assert.equal( z[ 1 ], 10 );
});

test( 'cmplx: in-place: out can alias a', function t() {
	var a = new Float64Array([ 2, 3 ]);
	cmplx.scale( a, a, 2 );
	assert.equal( a[ 0 ], 4 );
	assert.equal( a[ 1 ], 6 );
});

test( 'cmplx: subarray: works with views into larger arrays', function t() {
	var arr = new Float64Array([ 0, 0, 3, 4, 1, 2 ]);
	// Element 1 is at [2,3], element 2 is at [4,5]
	cmplx.mul( arr.subarray( 0, 2 ), arr.subarray( 2, 4 ), arr.subarray( 4, 6 ) );
	// (3+4i) * (1+2i) = (-5+10i)
	assert.equal( arr[ 0 ], -5 );
	assert.equal( arr[ 1 ], 10 );
});

