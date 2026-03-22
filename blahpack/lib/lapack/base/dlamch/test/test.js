'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlamch = require( './../lib' );

test( 'dlamch: main export is a function', function t() {
	assert.strictEqual( typeof dlamch, 'function' );
});

test( 'dlamch: epsilon (E)', function t() {
	var eps = dlamch( 'E' );
	assert.ok( eps > 0 );
	assert.ok( eps < 1e-15 );
	assert.equal( eps, 1.1102230246251565e-16 );
});

test( 'dlamch: safe minimum (S)', function t() {
	var sfmin = dlamch( 'S' );
	assert.ok( sfmin > 0 );
	assert.ok( sfmin < 1e-300 );
});

test( 'dlamch: base (B)', function t() {
	assert.equal( dlamch( 'B' ), 2 );
});

test( 'dlamch: precision (P)', function t() {
	var prec = dlamch( 'P' );
	assert.equal( prec, 2 * dlamch( 'E' ) );
});

test( 'dlamch: number of digits (N)', function t() {
	assert.equal( dlamch( 'N' ), 53 );
});

test( 'dlamch: rounding mode (R)', function t() {
	assert.equal( dlamch( 'R' ), 1.0 );
});

test( 'dlamch: min exponent (M)', function t() {
	assert.equal( dlamch( 'M' ), -1021 );
});

test( 'dlamch: underflow threshold (U)', function t() {
	assert.ok( dlamch( 'U' ) > 0 );
});

test( 'dlamch: max exponent (L)', function t() {
	assert.equal( dlamch( 'L' ), 1024 );
});

test( 'dlamch: overflow threshold (O)', function t() {
	assert.equal( dlamch( 'O' ), 1.7976931348623157e+308 );
});

test( 'dlamch: case-insensitive', function t() {
	assert.equal( dlamch( 'e' ), dlamch( 'E' ) );
	assert.equal( dlamch( 's' ), dlamch( 'S' ) );
});
