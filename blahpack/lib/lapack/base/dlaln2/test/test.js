/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaln2 = require( './../lib/base.js' );


// TESTS //

test( 'dlaln2: 1x1 real system', function t() {
	// (ca*A - wr*d1) * x = scale * b
	// (2*3 - 1*1) * x = scale * 10  => 5*x = scale*10 => x=2
	var A = new Float64Array( [ 3.0 ] );
	var B = new Float64Array( [ 10.0 ] );
	var X = new Float64Array( 1 );
	var res;

	res = dlaln2( false, 1, 1, 0.0, 2.0, A, 1, 1, 0, 1.0, 1.0, B, 1, 1, 0, 1.0, 0.0, X, 1, 1, 0 );
	assert.equal( res.info, 0 );
	assert.equal( res.scale, 1.0 );
	assert.ok( Math.abs( X[ 0 ] - 2.0 ) < 1e-14, 'X[0] should be 2.0, got ' + X[ 0 ] );
});

test( 'dlaln2: 1x1 complex system', function t() {
	// (ca*A - (wr+i*wi)*d1) * (x1+i*x2) = scale * (b1+i*b2)
	// (1*2 - (1+i*1)*1) * x = scale * (1+i*0) => (1-i)*x = scale*1
	// x = 1/(1-i) = (1+i)/2 = (0.5 + 0.5i)
	var A = new Float64Array( [ 2.0 ] );
	var B = new Float64Array( [ 1.0, 0.0 ] ); // B(1,1)=1, B(1,2)=0
	var X = new Float64Array( 2 );
	var res;

	res = dlaln2( false, 1, 2, 0.0, 1.0, A, 1, 1, 0, 1.0, 1.0, B, 1, 1, 0, 1.0, 1.0, X, 1, 1, 0 );
	assert.equal( res.info, 0 );
	assert.equal( res.scale, 1.0 );
	assert.ok( Math.abs( X[ 0 ] - 0.5 ) < 1e-14, 'X[0] should be 0.5, got ' + X[ 0 ] );
	assert.ok( Math.abs( X[ 1 ] - 0.5 ) < 1e-14, 'X[1] should be 0.5, got ' + X[ 1 ] );
});

test( 'dlaln2: 2x2 real system', function t() {
	// (ca*A - wr*D) * x = scale * b
	// A = [[2, 1], [0, 3]], D = diag(1,1), ca=1, wr=0
	// => A*x = b => [[2,1],[0,3]]*x = [[7],[6]] => x=[2,2]
	var A = new Float64Array( [ 2.0, 0.0, 1.0, 3.0 ] ); // col-major
	var B = new Float64Array( [ 7.0, 6.0 ] ); // B(1,1)=7, B(2,1)=6
	var X = new Float64Array( 2 );
	var res;

	res = dlaln2( false, 2, 1, 0.0, 1.0, A, 1, 2, 0, 1.0, 1.0, B, 1, 2, 0, 0.0, 0.0, X, 1, 2, 0 );
	assert.equal( res.info, 0 );
	assert.equal( res.scale, 1.0 );
	assert.ok( Math.abs( X[ 0 ] - 2.5 ) < 1e-14, 'X[0] should be 2.5, got ' + X[ 0 ] );
	assert.ok( Math.abs( X[ 1 ] - 2.0 ) < 1e-14, 'X[1] should be 2.0, got ' + X[ 1 ] );
});

test( 'dlaln2: 2x2 real transposed system', function t() {
	// (ca*A^T - wr*D) * x = scale * b
	// A = [[2, 0], [1, 3]], ca=1, wr=0, D=I
	// A^T = [[2, 1], [0, 3]]
	// [[2,1],[0,3]]*x = [[5],[9]] => x = [1,3]
	var A = new Float64Array( [ 2.0, 1.0, 0.0, 3.0 ] ); // col-major: A(1,1)=2, A(2,1)=1, A(1,2)=0, A(2,2)=3
	var B = new Float64Array( [ 5.0, 9.0 ] );
	var X = new Float64Array( 2 );
	var res;

	res = dlaln2( true, 2, 1, 0.0, 1.0, A, 1, 2, 0, 1.0, 1.0, B, 1, 2, 0, 0.0, 0.0, X, 1, 2, 0 );
	assert.equal( res.info, 0 );
	assert.equal( res.scale, 1.0 );
	assert.ok( Math.abs( X[ 0 ] - 1.0 ) < 1e-14, 'X[0] should be 1.0, got ' + X[ 0 ] );
	assert.ok( Math.abs( X[ 1 ] - 3.0 ) < 1e-14, 'X[1] should be 3.0, got ' + X[ 1 ] );
});

test( 'dlaln2: near-singular system returns info=1', function t() {
	// ca*A - wr*D = [[0]], but smin clamped to smini
	var A = new Float64Array( [ 1.0 ] );
	var B = new Float64Array( [ 1.0 ] );
	var X = new Float64Array( 1 );
	var res;

	// ca=1, A=1, wr=1, d1=1 => coefficient = 1-1 = 0 => perturbed
	res = dlaln2( false, 1, 1, 0.0, 1.0, A, 1, 1, 0, 1.0, 1.0, B, 1, 1, 0, 1.0, 0.0, X, 1, 1, 0 );
	assert.equal( res.info, 1, 'should return info=1 for near-singular' );
});
