'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dnrm2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dnrm2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

test( 'dnrm2: basic [3,4] -> 5', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	assertClose( dnrm2( 2, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: n=0 -> 0', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	assert.strictEqual( dnrm2( 0, x, 1, 0 ), tc.result );
});

test( 'dnrm2: n=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 7.0 ] );
	assertClose( dnrm2( 1, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: stride=2', function t() {
	var tc = findCase( 'stride2' );
	var x = new Float64Array( [ 3.0, 999.0, 4.0 ] );
	assertClose( dnrm2( 2, x, 2, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: large values', function t() {
	var tc = findCase( 'large_values' );
	var x = new Float64Array( [ 1e+154, 1e+154 ] );
	assertClose( dnrm2( 2, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: small values', function t() {
	var tc = findCase( 'small_values' );
	var x = new Float64Array( [ 1e-160, 1e-160 ] );
	assertClose( dnrm2( 2, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: five elements', function t() {
	var tc = findCase( 'five_elements' );
	var x = new Float64Array( [ 1, 2, 3, 4, 5 ] );
	assertClose( dnrm2( 5, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: all zeros', function t() {
	var tc = findCase( 'all_zeros' );
	var x = new Float64Array( [ 0, 0, 0 ] );
	assert.strictEqual( dnrm2( 3, x, 1, 0 ), tc.result );
});
