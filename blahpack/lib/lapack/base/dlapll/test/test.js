

'use strict';

// MODULES //

var Float64Array = require( '@stdlib/array/float64' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlapll = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlapll.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlapll: parallel vectors', function t() {
	var ssmin = new Float64Array( 1 );
	var tc = findCase( 'parallel' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( [ 2.0, 4.0, 6.0, 8.0 ] );
	dlapll( 4, x, 1, 0, y, 1, 0, ssmin );
	assertClose( ssmin[ 0 ], tc.ssmin, 1e-14, 'ssmin' );
});

test( 'dlapll: orthogonal vectors', function t() {
	var ssmin = new Float64Array( 1 );
	var tc = findCase( 'orthogonal' );
	var x = new Float64Array( [ 1.0, 0.0, 0.0 ] );
	var y = new Float64Array( [ 0.0, 1.0, 0.0 ] );
	dlapll( 3, x, 1, 0, y, 1, 0, ssmin );
	assertClose( ssmin[ 0 ], tc.ssmin, 1e-14, 'ssmin' );
});

test( 'dlapll: general vectors', function t() {
	var ssmin = new Float64Array( 1 );
	var tc = findCase( 'general' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dlapll( 3, x, 1, 0, y, 1, 0, ssmin );
	assertClose( ssmin[ 0 ], tc.ssmin, 1e-14, 'ssmin' );
});

test( 'dlapll: n_equals_1', function t() {
	var ssmin = new Float64Array( 1 );
	var tc = findCase( 'n_equals_1' );
	var x = new Float64Array( [ 5.0 ] );
	var y = new Float64Array( [ 3.0 ] );
	dlapll( 1, x, 1, 0, y, 1, 0, ssmin );
	assertClose( ssmin[ 0 ], tc.ssmin, 1e-14, 'ssmin' );
});

test( 'dlapll: n_equals_2', function t() {
	var ssmin = new Float64Array( 1 );
	var tc = findCase( 'n_equals_2' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	var y = new Float64Array( [ 1.0, 2.0 ] );
	dlapll( 2, x, 1, 0, y, 1, 0, ssmin );
	assertClose( ssmin[ 0 ], tc.ssmin, 1e-14, 'ssmin' );
});

test( 'dlapll: nearly parallel vectors', function t() {
	var ssmin = new Float64Array( 1 );
	var tc = findCase( 'nearly_parallel' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.001 ] );
	dlapll( 5, x, 1, 0, y, 1, 0, ssmin );
	assertClose( ssmin[ 0 ], tc.ssmin, 1e-14, 'ssmin' );
});
