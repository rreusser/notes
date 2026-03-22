'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaev2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaev2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlaev2: identity matrix', function t() {
	var tc = findCase( 'identity' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: diagonal matrix', function t() {
	var tc = findCase( 'diagonal' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: off-diagonal only', function t() {
	var tc = findCase( 'off_diagonal' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: general case', function t() {
	var tc = findCase( 'general' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: negative diagonal', function t() {
	var tc = findCase( 'negative_diagonal' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: sm = 0', function t() {
	var tc = findCase( 'sm_zero' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: equal diagonal', function t() {
	var tc = findCase( 'equal_diagonal' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: negative off-diagonal', function t() {
	var tc = findCase( 'negative_offdiag' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: df < 0 (a < c)', function t() {
	var tc = findCase( 'df_negative' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: b=0, a < c', function t() {
	var tc = findCase( 'b_zero_a_lt_c' );
	var out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});
