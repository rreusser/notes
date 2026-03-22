'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlarf = require( './../lib/base.js' );

// Load fixture
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1e-30 );
	assert.ok( relErr <= 1e-12, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, label ) {
	var i;
	assert.strictEqual( actual.length, expected.length, label + ' length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], label + '[' + i + ']' );
	}
}

test( 'zlarf: left side, 3x2 matrix', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_left_3x2'; });
	// v = [1+0i, 0.5+0.5i, -0.3+0.2i]
	var v = new Float64Array( [ 1.0, 0.0, 0.5, 0.5, -0.3, 0.2 ] );
	var tau = new Float64Array( [ 1.5, 0.3 ] );
	// C is 3x2 col-major: col1=[1+0i,3+1i,5+2i], col2=[2+1i,4-1i,6+0i]
	var C = new Float64Array( [
		1.0, 0.0,  3.0, 1.0,  5.0, 2.0,   // col 1
		2.0, 1.0,  4.0, -1.0, 6.0, 0.0    // col 2
	]);
	var work = new Float64Array( 20 );

	// strideC1=1 (row stride in complex elems), strideC2=3 (col stride = LDA=3)
	zlarf( 'L', 3, 2, v, 1, 0, tau, 0, C, 1, 3, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: right side, 2x3 matrix', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_right_2x3'; });
	var v = new Float64Array( [ 1.0, 0.0, 0.5, 0.5, -0.3, 0.2 ] );
	var tau = new Float64Array( [ 1.5, 0.3 ] );
	// C is 2x3 col-major
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,   // col 1
		3.0, 1.0,  4.0, -1.0,  // col 2
		5.0, 2.0,  6.0, 0.0    // col 3
	]);
	var work = new Float64Array( 20 );

	zlarf( 'R', 2, 3, v, 1, 0, tau, 0, C, 1, 2, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: tau=0 (identity)', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_tau_zero'; });
	var v = new Float64Array( [ 1.0, 0.0, 0.5, 0.5 ] );
	var tau = new Float64Array( [ 0.0, 0.0 ] );
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,
		3.0, 2.0,  4.0, 3.0
	]);
	var work = new Float64Array( 20 );

	zlarf( 'L', 2, 2, v, 1, 0, tau, 0, C, 1, 2, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: n=0', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_n_zero'; });
	var v = new Float64Array( [ 1.0, 0.0 ] );
	var tau = new Float64Array( [ 1.0, 0.0 ] );
	var C = new Float64Array( [ 1.0, 0.0 ] );
	var work = new Float64Array( 20 );

	zlarf( 'L', 1, 0, v, 1, 0, tau, 0, C, 1, 1, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: left side, 4x3 matrix', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_left_4x3'; });
	var v = new Float64Array( [
		1.0, 0.0,  0.2, 0.3,  -0.5, 0.1,  0.4, -0.6
	]);
	var tau = new Float64Array( [ 1.2, -0.4 ] );
	var C = new Float64Array( [
		1.0, 0.0,   0.0, 1.0,   2.0, -1.0,  3.0, 0.5,
		-1.0, 2.0,  0.5, 0.5,   1.5, -0.5, -2.0, 1.0,
		0.0, 0.0,   1.0, 1.0,  -0.5, 0.0,   2.0, -2.0
	]);
	var work = new Float64Array( 20 );

	zlarf( 'L', 4, 3, v, 1, 0, tau, 0, C, 1, 4, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});
