'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dznrm2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dznrm2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

test( 'dznrm2: single complex element (3+4i) has norm 5', function t() {
	var tc = findCase( 'single' );
	var zx = new Float64Array( [ 3, 4 ] );
	assertClose( dznrm2( 1, zx, 1, 0 ), tc.result, 1e-14, 'single' );
});

test( 'dznrm2: two elements (1+0i), (0+1i) has norm sqrt(2)', function t() {
	var tc = findCase( 'two_elements' );
	var zx = new Float64Array( [ 1, 0, 0, 1 ] );
	assertClose( dznrm2( 2, zx, 1, 0 ), tc.result, 1e-14, 'two_elements' );
});

test( 'dznrm2: n=0 returns 0', function t() {
	var tc = findCase( 'n_zero' );
	assert.equal( dznrm2( 0, new Float64Array( 2 ), 1, 0 ), tc.result );
});

test( 'dznrm2: three elements', function t() {
	var tc = findCase( 'three_elements' );
	var zx = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	assertClose( dznrm2( 3, zx, 1, 0 ), tc.result, 1e-14, 'three_elements' );
});

test( 'dznrm2: stride=2', function t() {
	var tc = findCase( 'stride_2' );
	// x(1) = (1,0), x(2) = (99,99), x(3) = (0,1) — stride 2 skips x(2)
	var zx = new Float64Array( [ 1, 0, 99, 99, 0, 1 ] );
	assertClose( dznrm2( 2, zx, 2, 0 ), tc.result, 1e-14, 'stride_2' );
});
