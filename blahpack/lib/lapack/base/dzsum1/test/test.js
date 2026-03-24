'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var dzsum1 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dzsum1.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}


// TESTS //

test( 'dzsum1: main export is a function', function t() {
	assert.strictEqual( typeof dzsum1, 'function' );
});

test( 'dzsum1: basic 3-element vector', function t() {
	var tc = findCase( 'basic_3' );
	// zx = [(3,4), (1,0), (0,1)] => |3+4i|=5, |1|=1, |i|=1 => sum=7
	var zx = new Complex128Array( [ 3, 4, 1, 0, 0, 1 ] );
	var result = dzsum1( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: stride=2', function t() {
	var tc = findCase( 'stride2' );
	// zx = [(3,4), skip, (5,12), skip, (8,15)]
	var zx = new Complex128Array( [ 3, 4, 99, 99, 5, 12, 99, 99, 8, 15 ] );
	var result = dzsum1( 3, zx, 2, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: N=0 returns 0', function t() {
	var tc = findCase( 'n_zero' );
	var zx = new Complex128Array( [ 1, 2 ] );
	var result = dzsum1( 0, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: N=1', function t() {
	var tc = findCase( 'n_one' );
	// |(6,8)| = 10
	var zx = new Complex128Array( [ 6, 8 ] );
	var result = dzsum1( 1, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: all zeros', function t() {
	var tc = findCase( 'all_zeros' );
	var zx = new Complex128Array( [ 0, 0, 0, 0, 0, 0 ] );
	var result = dzsum1( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: purely real', function t() {
	var tc = findCase( 'purely_real' );
	var zx = new Complex128Array( [ 3, 0, -4, 0, 5, 0 ] );
	var result = dzsum1( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: purely imaginary', function t() {
	var tc = findCase( 'purely_imag' );
	var zx = new Complex128Array( [ 0, 2, 0, -3 ] );
	var result = dzsum1( 2, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: nonzero offset', function t() {
	// Use offset to skip the first element
	// zx = [skip, (3,4), (1,0), (0,1)]
	var zx = new Complex128Array( [ 99, 99, 3, 4, 1, 0, 0, 1 ] );
	var result = dzsum1( 3, zx, 1, 1 );
	assertClose( result, 7.0, 1e-14, 'result' );
});
