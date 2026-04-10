/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrk = require( './../lib' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarrk.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlarrk, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlarrk.ndarray, 'function', 'has ndarray method' );
});

test( 'main: n1 matches fixture', function t() {
	var tc = findCase( 'n1' );
	var d = new Float64Array( [ 2.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = dlarrk( 1, 1, 0.0, 4.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'main: n2_iw1 matches fixture', function t() {
	var tc = findCase( 'n2_iw1' );
	var d = new Float64Array( [ 1.0, 4.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = dlarrk( 2, 1, 0.0, 5.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'main: n2_iw2 matches fixture', function t() {
	var tc = findCase( 'n2_iw2' );
	var d = new Float64Array( [ 1.0, 4.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = dlarrk( 2, 2, 0.0, 5.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'main: n5_iw1 matches fixture', function t() {
	var tc = findCase( 'n5_iw1' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = dlarrk( 5, 1, -10.0, 10.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'main: n5_iw3 matches fixture', function t() {
	var tc = findCase( 'n5_iw3' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = dlarrk( 5, 3, -10.0, 10.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'main: n5_iw5 matches fixture', function t() {
	var tc = findCase( 'n5_iw5' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = dlarrk( 5, 5, -10.0, 10.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'main: n4_neg_iw2 matches fixture (negative eigenvalue)', function t() {
	var tc = findCase( 'n4_neg_iw2' );
	var d = new Float64Array( [ -5.0, -3.0, -7.0, -1.0 ] );
	var e2 = new Float64Array( [ 0.25, 0.25, 0.25 ] );
	var r = dlarrk( 4, 2, -10.0, 0.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});
