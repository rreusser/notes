/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztbsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztbsv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assert.ok(Math.abs( actual[ i ] - expected[ i ] ) <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ]);
	}
}


// TESTS //

test( 'ztbsv: main export is a function', function t() {
	assert.strictEqual( typeof ztbsv, 'function' );
});

test( 'ztbsv: upper, no-transpose, non-unit (N=3, K=1)', function t() {
	var tc = findCase( 'upper_notrans_nonunit' );
	var AB = new Complex128Array( [ 0, 0, 4, 0, 2, 0, 5, 0, 3, 0, 6, 0 ] );
	var xv;
	var x = new Complex128Array( [ 10, 0, 19, 0, 18, 0 ] );
	ztbsv( 'upper', 'no-transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: lower, no-transpose, non-unit (N=3, K=1)', function t() {
	var tc = findCase( 'lower_notrans_nonunit' );
	var AB = new Complex128Array( [ 2, 0, 1, 0, 3, 0, 1, 0, 4, 0, 0, 0 ] );
	var xv;
	var x = new Complex128Array( [ 2, 0, 7, 0, 14, 0 ] );
	ztbsv( 'lower', 'no-transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: upper, transpose, non-unit (N=3, K=1)', function t() {
	var tc = findCase( 'upper_trans_nonunit' );
	var AB = new Complex128Array( [ 0, 0, 4, 0, 2, 0, 5, 0, 3, 0, 6, 0 ] );
	var xv;
	var x = new Complex128Array( [ 4, 0, 12, 0, 24, 0 ] );
	ztbsv( 'upper', 'transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: upper, conjugate-transpose, non-unit (N=3, K=1)', function t() {
	var tc = findCase( 'upper_conjtrans_nonunit' );
	var AB = new Complex128Array( [ 0, 0, 2, 0, 1, 1, 3, 0, 2, -1, 4, 0 ] );
	var xv;
	var x = new Complex128Array( [ 2, 0, 4, -1, 6, 1 ] );
	ztbsv( 'upper', 'conjugate-transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: lower, transpose, non-unit (N=3, K=1)', function t() {
	var tc = findCase( 'lower_trans_nonunit' );
	var AB = new Complex128Array( [ 2, 0, 1, 0, 3, 0, 1, 0, 4, 0, 0, 0 ] );
	var xv;
	var x = new Complex128Array( [ 4, 0, 9, 0, 12, 0 ] );
	ztbsv( 'lower', 'transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: lower, conjugate-transpose, non-unit (N=3, K=1)', function t() {
	var tc = findCase( 'lower_conjtrans_nonunit' );
	var AB = new Complex128Array( [ 2, 0, 1, 1, 3, 1, 2, -1, 4, 0, 0, 0 ] );
	var xv;
	var x = new Complex128Array( [ 3, -1, 5, 0, 4, 0 ] );
	ztbsv( 'lower', 'conjugate-transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: upper, unit diagonal (N=3, K=1)', function t() {
	var tc = findCase( 'upper_unit' );
	var AB = new Complex128Array( [ 0, 0, 99, 0, 2, 0, 99, 0, 3, 0, 99, 0 ] );
	var xv;
	var x = new Complex128Array( [ 7, 0, 7, 0, 1, 0 ] );
	ztbsv( 'upper', 'no-transpose', 'unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: N=0 quick return', function t() {
	var result = ztbsv( 'upper', 'no-transpose', 'non-unit', 0, 1, AB, 1, 1, 0, x, 1, 0 );
	var AB = new Complex128Array( 1 );
	var xv;
	var x = new Complex128Array( [ 5, 5 ] );
	assert.strictEqual( result, x );
	xv = reinterpret( x, 0 );
	assert.strictEqual( xv[ 0 ], 5 );
	assert.strictEqual( xv[ 1 ], 5 );
});

test( 'ztbsv: upper, complex entries (N=3, K=1)', function t() {
	var tc = findCase( 'upper_complex' );
	var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 2, 1, 2, -1, 4, -1 ] );
	var xv;
	var x = new Complex128Array( [ 3, 2, 5, 1, 8, -2 ] );
	ztbsv( 'upper', 'no-transpose', 'non-unit', 3, 1, AB, 1, 2, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});

test( 'ztbsv: upper, K=2, N=4', function t() {
	var tc = findCase( 'upper_k2' );
	var AB = new Complex128Array([
		0,
		0,
		0,
		0,
		4,
		0,
		0,
		0,
		2,
		0,
		4,
		0,
		1,
		0,
		2,
		0,
		4,
		0,
		1,
		0,
		2,
		0,
		4,
		0
	]);
	var xv;
	var x = new Complex128Array( [ 7, 0, 7, 0, 6, 0, 4, 0 ] );
	ztbsv( 'upper', 'no-transpose', 'non-unit', 4, 2, AB, 1, 3, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-12, 'x' );
});
