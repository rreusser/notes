'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlanst = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlanst.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'dlanst: N=0 returns 0', function t() {
	var d = new Float64Array( 0 );
	var e = new Float64Array( 0 );
	var result = dlanst( 'M', 0, d, 1, 0, e, 1, 0 );
	assert.strictEqual( result, 0.0 );
});

test( 'dlanst: N=1, max norm', function t() {
	var tc = findCase( 'n1_max' );
	var d = new Float64Array( [ tc.d1 ] );
	var e = new Float64Array( 0 );
	var result = dlanst( 'M', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=1, one-norm', function t() {
	var tc = findCase( 'n1_one' );
	var d = new Float64Array( [ tc.d1 ] );
	var e = new Float64Array( 0 );
	var result = dlanst( '1', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=1, infinity-norm', function t() {
	var tc = findCase( 'n1_inf' );
	var d = new Float64Array( [ tc.d1 ] );
	var e = new Float64Array( 0 );
	var result = dlanst( 'I', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=1, Frobenius norm', function t() {
	var tc = findCase( 'n1_frob' );
	var d = new Float64Array( [ tc.d1 ] );
	var e = new Float64Array( 0 );
	var result = dlanst( 'F', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, max norm', function t() {
	var tc = findCase( 'n5_max' );
	var d = new Float64Array( tc.d );
	var e = new Float64Array( tc.e );
	var result = dlanst( 'M', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, one-norm (O)', function t() {
	var tc = findCase( 'n5_one' );
	var d = new Float64Array( tc.d );
	var e = new Float64Array( tc.e );
	var result = dlanst( 'O', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, infinity-norm', function t() {
	var tc = findCase( 'n5_inf' );
	var d = new Float64Array( tc.d );
	var e = new Float64Array( tc.e );
	var result = dlanst( 'I', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, Frobenius norm', function t() {
	var tc = findCase( 'n5_frob' );
	var d = new Float64Array( tc.d );
	var e = new Float64Array( tc.e );
	var result = dlanst( 'F', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=2, one-norm (1)', function t() {
	var tc = findCase( 'n2_one' );
	var d = new Float64Array( [ tc.d1, tc.d2 ] );
	var e = new Float64Array( [ tc.e1 ] );
	var result = dlanst( '1', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=2, Frobenius norm', function t() {
	var tc = findCase( 'n2_frob' );
	var d = new Float64Array( [ tc.d1, tc.d2 ] );
	var e = new Float64Array( [ tc.e1 ] );
	var result = dlanst( 'F', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: E norm type (same as F)', function t() {
	var tc = findCase( 'n2_e_norm' );
	var d = new Float64Array( [ tc.d1, tc.d2 ] );
	var e = new Float64Array( [ tc.e1 ] );
	var result = dlanst( 'E', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, max norm, all positive', function t() {
	var tc = findCase( 'n5_max_positive' );
	var d = new Float64Array( tc.d );
	var e = new Float64Array( tc.e );
	var result = dlanst( 'M', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: max norm where off-diagonal has largest element', function t() {
	// d = [1, 2], e = [10] => max is 10 from off-diagonal
	var d = new Float64Array( [ 1.0, 2.0 ] );
	var e = new Float64Array( [ 10.0 ] );
	var result = dlanst( 'M', 2, d, 1, 0, e, 1, 0 );
	assert.strictEqual( result, 10.0 );
});

test( 'dlanst: supports stride and offset for d and e', function t() {
	// d = [2, -4, 6] with stride=2 and offset=1 in padded array
	// e = [1, -2] with stride=2 and offset=0 in padded array
	var d = new Float64Array( [ 99.0, 2.0, 99.0, -4.0, 99.0, 6.0 ] );
	var e = new Float64Array( [ 1.0, 99.0, -2.0 ] );
	var result = dlanst( 'M', 3, d, 2, 1, e, 2, 0 );
	// max of |2|, |-4|, |6|, |1|, |-2| = 6
	assert.strictEqual( result, 6.0 );
});
