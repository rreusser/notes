'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansy = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlansy.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

// Upper triangle of 4x4 symmetric matrix, stored column-major:
// A(1,1)=(2,1) A(1,2)=(1,2) A(1,3)=(3,-1) A(1,4)=(0.5,0.5)
//              A(2,2)=(5,-1) A(2,3)=(2,1)  A(2,4)=(1,-2)
//                            A(3,3)=(4,2)  A(3,4)=(3,0)
//                                          A(4,4)=(6,-3)
function makeUpperA() {
	// Column-major 4x4 complex, interleaved re/im
	return new Complex128Array( new Float64Array( [
		2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 2.0, 5.0, -1.0, 0.0, 0.0, 0.0, 0.0,
		3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.0, 0.0,
		0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0
	] ) );
}

// Lower triangle version (same matrix, lower stored)
function makeLowerA() {
	return new Complex128Array( new Float64Array( [
		2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5,
		0.0, 0.0, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0,
		0.0, 0.0, 0.0, 0.0, 4.0, 2.0, 3.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 6.0, -3.0
	] ) );
}


// TESTS //

test( 'zlansy: max norm, upper', function t() {
	var tc = findCase( 'max_upper' );
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'max', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: one norm, upper', function t() {
	var tc = findCase( 'one_upper' );
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'one-norm', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: inf norm, upper', function t() {
	var tc = findCase( 'inf_upper' );
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'inf-norm', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: frobenius norm, upper', function t() {
	var tc = findCase( 'fro_upper' );
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'frobenius', 'upper', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: one norm, lower', function t() {
	var tc = findCase( 'one_lower' );
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'one-norm', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: frobenius norm, lower', function t() {
	var tc = findCase( 'fro_lower' );
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'frobenius', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: max norm, lower', function t() {
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'max', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	// Same symmetric matrix, max norm should be same as upper
	var tc = findCase( 'max_upper' );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: inf norm, lower', function t() {
	var A = makeLowerA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'inf-norm', 'lower', 4, A, 1, 4, 0, WORK, 1, 0 );
	// For symmetric matrix, one-norm == inf-norm
	var tc = findCase( 'one_lower' );
	assertClose( result, tc.val, 1e-14, 'val' );
});

test( 'zlansy: N=0', function t() {
	var A = makeUpperA();
	var WORK = new Float64Array( 4 );
	var result = zlansy( 'max', 'upper', 0, A, 1, 4, 0, WORK, 1, 0 );
	assert.strictEqual( result, 0.0, 'N=0 returns 0' );
});
