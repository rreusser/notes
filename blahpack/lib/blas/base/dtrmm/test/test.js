/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrmm = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrmm.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i, relErr;
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		if ( relErr > tol ) {
			throw new Error( msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

function setupTriUpper3( a ) {
	// Upper tri 3x3 col-major LDA=3: [2 3 4; 0 5 6; 0 0 7]
	a[ 0 ] = 2; a[ 3 ] = 3; a[ 6 ] = 4;
	a[ 4 ] = 5; a[ 7 ] = 6;
	a[ 8 ] = 7;
}

function setupTriLower3( a ) {
	// Lower tri 3x3 col-major LDA=3: [2 0 0; 3 5 0; 4 6 7]
	a[ 0 ] = 2; a[ 1 ] = 3; a[ 2 ] = 4;
	a[ 4 ] = 5; a[ 5 ] = 6;
	a[ 8 ] = 7;
}

function setupB3x2( b ) {
	b[ 0 ] = 1; b[ 1 ] = 2; b[ 2 ] = 3;
	b[ 3 ] = 4; b[ 4 ] = 5; b[ 5 ] = 6;
}

var cases = [
	{ name: 'left_upper_n', s: 'left', u: 'upper', t: 'no-transpose', d: 'non-unit', m: 3, n: 2, al: 1, aFn: setupTriUpper3, aLda: 3 },
	{ name: 'left_lower_n', s: 'left', u: 'lower', t: 'no-transpose', d: 'non-unit', m: 3, n: 2, al: 1, aFn: setupTriLower3, aLda: 3 },
	{ name: 'left_upper_t', s: 'left', u: 'upper', t: 'transpose', d: 'non-unit', m: 3, n: 2, al: 1, aFn: setupTriUpper3, aLda: 3 },
	{ name: 'left_lower_t', s: 'left', u: 'lower', t: 'transpose', d: 'non-unit', m: 3, n: 2, al: 1, aFn: setupTriLower3, aLda: 3 },
];

cases.forEach( function( c ) {
	test( 'dtrmm: ' + c.name, function t() {
		var tc = findCase( c.name );
		var a = new Float64Array( 16 );
		var b = new Float64Array( 16 );
		c.aFn( a );
		setupB3x2( b );
		dtrmm( c.s, c.u, c.t, c.d, c.m, c.n, c.al, a, 1, c.aLda, 0, b, 1, 3, 0 );
		assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
	});
});

test( 'dtrmm: right upper N', function t() {
	var tc = findCase( 'right_upper_n' );
	var a = new Float64Array( 16 );
	a[ 0 ] = 2; a[ 2 ] = 3; a[ 3 ] = 5;
	var b = new Float64Array( 16 );
	b[ 0 ] = 1; b[ 1 ] = 2; b[ 2 ] = 3; b[ 3 ] = 4; b[ 4 ] = 5; b[ 5 ] = 6;
	dtrmm( 'right', 'upper', 'no-transpose', 'non-unit', 3, 2, 1.0, a, 1, 2, 0, b, 1, 3, 0 );
	assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
});

test( 'dtrmm: right lower N', function t() {
	var tc = findCase( 'right_lower_n' );
	var a = new Float64Array( 16 );
	a[ 0 ] = 2; a[ 1 ] = 3; a[ 3 ] = 5;
	var b = new Float64Array( 16 );
	b[ 0 ] = 1; b[ 1 ] = 2; b[ 2 ] = 3; b[ 3 ] = 4; b[ 4 ] = 5; b[ 5 ] = 6;
	dtrmm( 'right', 'lower', 'no-transpose', 'non-unit', 3, 2, 1.0, a, 1, 2, 0, b, 1, 3, 0 );
	assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
});

test( 'dtrmm: right upper T', function t() {
	var tc = findCase( 'right_upper_t' );
	var a = new Float64Array( 16 );
	a[ 0 ] = 2; a[ 2 ] = 3; a[ 3 ] = 5;
	var b = new Float64Array( 16 );
	b[ 0 ] = 1; b[ 1 ] = 2; b[ 2 ] = 3; b[ 3 ] = 4; b[ 4 ] = 5; b[ 5 ] = 6;
	dtrmm( 'right', 'upper', 'transpose', 'non-unit', 3, 2, 1.0, a, 1, 2, 0, b, 1, 3, 0 );
	assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
});

test( 'dtrmm: right lower T', function t() {
	var tc = findCase( 'right_lower_t' );
	var a = new Float64Array( 16 );
	a[ 0 ] = 2; a[ 1 ] = 3; a[ 3 ] = 5;
	var b = new Float64Array( 16 );
	b[ 0 ] = 1; b[ 1 ] = 2; b[ 2 ] = 3; b[ 3 ] = 4; b[ 4 ] = 5; b[ 5 ] = 6;
	dtrmm( 'right', 'lower', 'transpose', 'non-unit', 3, 2, 1.0, a, 1, 2, 0, b, 1, 3, 0 );
	assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
});

test( 'dtrmm: alpha=0', function t() {
	var tc = findCase( 'alpha_zero' );
	var a = new Float64Array( 16 );
	setupTriUpper3( a );
	var b = new Float64Array( 16 );
	setupB3x2( b );
	dtrmm( 'left', 'upper', 'no-transpose', 'non-unit', 3, 2, 0.0, a, 1, 3, 0, b, 1, 3, 0 );
	assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
});

test( 'dtrmm: M=0 quick return', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( [ 99 ] );
	dtrmm( 'left', 'upper', 'no-transpose', 'non-unit', 0, 2, 1.0, a, 1, 1, 0, b, 1, 1, 0 );
	if ( b[ 0 ] !== 99 ) {
		throw new Error( 'B changed on M=0' );
	}
});

test( 'dtrmm: unit diag', function t() {
	var tc = findCase( 'unit_diag' );
	var a = new Float64Array( 16 );
	a[ 0 ] = 99; a[ 3 ] = 3; a[ 6 ] = 4;
	a[ 4 ] = 99; a[ 7 ] = 6;
	a[ 8 ] = 99;
	var b = new Float64Array( 16 );
	setupB3x2( b );
	dtrmm( 'left', 'upper', 'no-transpose', 'unit', 3, 2, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	assertArrayClose( Array.prototype.slice.call( b, 0, tc.b.length ), tc.b, 1e-14, 'B' );
});

// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid side', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( 16 );
	assert.throws( function f() {
		ndarray( 'invalid', 'upper', 'no-transpose', 'non-unit', 3, 2, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( 16 );
	assert.throws( function f() {
		ndarray( 'left', 'invalid', 'no-transpose', 'non-unit', 3, 2, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid transa', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( 16 );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'invalid', 'non-unit', 3, 2, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( 16 );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'invalid', 3, 2, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative M', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( 16 );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'non-unit', -1, 2, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var a = new Float64Array( 16 );
	var b = new Float64Array( 16 );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'non-unit', 3, -1, 1.0, a, 1, 3, 0, b, 1, 3, 0 );
	}, RangeError );
});
