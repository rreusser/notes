'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlae2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlae2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlae2: diagonal matrix (a > c)', function t() {
	var tc = findCase( 'diagonal_a_gt_c' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: diagonal matrix (c > a)', function t() {
	var tc = findCase( 'diagonal_c_gt_a' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: off-diagonal only', function t() {
	var tc = findCase( 'off_diagonal' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: equal diagonal', function t() {
	var tc = findCase( 'equal_diagonal' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: general 2x2', function t() {
	var tc = findCase( 'general' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: negative diagonal (sm < 0)', function t() {
	var tc = findCase( 'negative_diagonal' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: sm = 0 (a = -c)', function t() {
	var tc = findCase( 'sm_zero' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: identity matrix', function t() {
	var tc = findCase( 'identity' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: adf < ab path', function t() {
	var tc = findCase( 'adf_lt_ab' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});

test( 'dlae2: adf = ab path', function t() {
	var tc = findCase( 'adf_eq_ab' );
	var out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
});
