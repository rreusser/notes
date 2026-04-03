/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarfx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlarfx.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Builds a column-major M-by-N complex matrix from flat interleaved re/im column-major data.
*
* @private
* @param {NonNegativeInteger} M - rows
* @param {NonNegativeInteger} N - cols
* @param {Array} values - interleaved re/im values in column-major order
* @returns {Complex128Array} complex matrix
*/
function buildMatrix( M, N, values ) {
	return new Complex128Array( values );
}

/**
* Gets the Float64 view of a result matrix for comparison.
*
* @private
* @param {Complex128Array} C - complex matrix
* @returns {Array} array of Float64 values
*/
function getView( C ) {
	return toArray( reinterpret( C, 0 ) );
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'zlarfx is a function', function t() {
	assert.equal( typeof zlarfx, 'function' );
} );

test( 'zlarfx: left M=1 N=3', function t() {
	var WORK = new Complex128Array( 10 );
	var tau = new Complex128( 1.5, 0.3 );
	var tc = findCase( 'left M=1 N=3' );
	var v = new Complex128Array( [ 1.0, 0.0 ] );
	var C = buildMatrix( 1, 3, [
		2.0, 1.0, 3.0, -1.0, 4.0, 0.5
	]);
	zlarfx( 'left', 1, 3, v, 1, 0, tau, C, 1, 1, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-14, 'C' );
} );

test( 'zlarfx: left M=2 N=3', function t() {
	var WORK = new Complex128Array( 10 );
	var tau = new Complex128( 1.6, -0.2 );
	var tc = findCase( 'left M=2 N=3' );
	var v = new Complex128Array( [ 1.0, 0.0, 0.5, 0.3 ] );
	var C = buildMatrix( 2, 3, [
		1.0,
		2.0,
		4.0,
		-1.0,
		2.0,
		0.5,
		5.0,
		3.0,
		3.0,
		-1.0,
		6.0,
		0.0
	]);
	zlarfx( 'left', 2, 3, v, 1, 0, tau, C, 1, 2, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-14, 'C' );
} );

test( 'zlarfx: left M=3 N=2', function t() {
	var WORK = new Complex128Array( 10 );
	var tau = new Complex128( 1.2, 0.4 );
	var tc = findCase( 'left M=3 N=2' );
	var v = new Complex128Array( [ 1.0, 0.0, 0.3, -0.5, -0.4, 0.2 ] );
	var C = buildMatrix( 3, 2, [
		2.0,
		1.0,
		3.0,
		-2.0,
		5.0,
		0.0,
		1.0,
		-0.5,
		4.0,
		1.0,
		6.0,
		-3.0
	]);
	zlarfx( 'left', 3, 2, v, 1, 0, tau, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-14, 'C' );
} );

test( 'zlarfx: left M=4 N=2', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.4, -0.1 );
	v = new Complex128Array([
		1.0, 0.0, 0.3, 0.1, -0.4, 0.2, 0.2, -0.3
	]);
	vals = [];
	M = 4;
	N = 2;
	tc = findCase( 'left M=4 N=2' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.1 * (i - j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: left M=5 N=2', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.5, 0.2 );
	v = new Complex128Array([
		1.0, 0.0, 0.2, -0.3, -0.3, 0.1, 0.4, 0.2, -0.1, -0.4
	]);
	vals = [];
	M = 5;
	N = 2;
	tc = findCase( 'left M=5 N=2' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.1 * i );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: left M=6 N=2', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.3, -0.3 );
	v = new Complex128Array([
		1.0, 0.0, 0.2, -0.1, -0.3, 0.3, 0.4, -0.2, -0.1, 0.4, 0.5, 0.1
	]);
	vals = [];
	M = 6;
	N = 2;
	tc = findCase( 'left M=6 N=2' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.2 * j );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: left M=7 N=2', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.1, 0.5 );
	v = new Complex128Array([
		1.0,
		0.0,
		0.2,
		0.1,
		-0.3,
		-0.2,
		0.4,
		0.3,
		-0.1,
		-0.1,
		0.5,
		0.2,
		-0.2,
		0.4
	]);
	vals = [];
	M = 7;
	N = 2;
	tc = findCase( 'left M=7 N=2' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( -0.1 * i );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: left M=8 N=2', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.6, -0.4 );
	v = new Complex128Array([
		1.0,
		0.0,
		0.1,
		-0.2,
		-0.2,
		0.1,
		0.3,
		0.3,
		-0.4,
		-0.1,
		0.15,
		0.25,
		-0.25,
		-0.15,
		0.35,
		0.05
	]);
	vals = [];
	M = 8;
	N = 2;
	tc = findCase( 'left M=8 N=2' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.3 * (i - j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: left M=9 N=2', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.7, 0.1 );
	v = new Complex128Array([
		1.0,
		0.0,
		0.1,
		0.2,
		-0.2,
		-0.1,
		0.3,
		0.1,
		-0.4,
		0.2,
		0.15,
		-0.3,
		-0.25,
		0.15,
		0.35,
		-0.05,
		-0.05,
		0.35
	]);
	vals = [];
	M = 9;
	N = 2;
	tc = findCase( 'left M=9 N=2' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( -0.2 * j );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: left M=10 N=2', function t() {
	var vvals;
	var WORK;
	var vals;
	var tau;
	var tc;
	var M;
	var N;
	var i;
	var j;
	var v;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.8, -0.2 );
	vvals = [];
	vals = [];
	M = 10;
	N = 2;
	tc = findCase( 'left M=10 N=2' );
	for ( i = 1; i <= 10; i += 1 ) {
		vvals.push( 0.1 * i );
		vvals.push( -0.05 * i );
	}
	vvals[ 0 ] = 1.0;
	vvals[ 1 ] = 0.0;
	v = new Complex128Array( vvals );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + (0.5 * j) );
			vals.push( 0.1 * i * j );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-12, 'C' );
} );

test( 'zlarfx: right M=3 N=1', function t() {
	var WORK = new Complex128Array( 10 );
	var tau = new Complex128( 1.5, 0.3 );
	var tc = findCase( 'right M=3 N=1' );
	var v = new Complex128Array( [ 1.0, 0.0 ] );
	var C = buildMatrix( 3, 1, [
		2.0, 1.0, 3.0, -1.0, 4.0, 0.5
	]);
	zlarfx( 'right', 3, 1, v, 1, 0, tau, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-14, 'C' );
} );

test( 'zlarfx: right M=3 N=2', function t() {
	var WORK = new Complex128Array( 10 );
	var tau = new Complex128( 1.6, 0.2 );
	var tc = findCase( 'right M=3 N=2' );
	var v = new Complex128Array( [ 1.0, 0.0, 0.5, -0.3 ] );
	var C = buildMatrix( 3, 2, [
		1.0,
		0.5,
		3.0,
		1.0,
		5.0,
		-2.0,
		2.0,
		-1.0,
		4.0,
		0.0,
		6.0,
		3.0
	]);
	zlarfx( 'right', 3, 2, v, 1, 0, tau, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=3', function t() {
	var WORK = new Complex128Array( 10 );
	var tau = new Complex128( 1.2, -0.4 );
	var tc = findCase( 'right M=2 N=3' );
	var v = new Complex128Array( [ 1.0, 0.0, 0.3, 0.5, -0.4, -0.2 ] );
	var C = buildMatrix( 2, 3, [
		2.0,
		1.0,
		4.0,
		-2.0,
		1.0,
		0.5,
		5.0,
		1.0,
		3.0,
		-0.5,
		6.0,
		3.0
	]);
	zlarfx( 'right', 2, 3, v, 1, 0, tau, C, 1, 2, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=4', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.4, 0.1 );
	v = new Complex128Array([
		1.0, 0.0, 0.3, 0.1, -0.4, 0.2, 0.2, -0.3
	]);
	vals = [];
	M = 2;
	N = 4;
	tc = findCase( 'right M=2 N=4' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.1 * (i + j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=5', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.5, -0.2 );
	v = new Complex128Array([
		1.0, 0.0, 0.2, -0.3, -0.3, 0.1, 0.4, 0.2, -0.1, -0.4
	]);
	vals = [];
	M = 2;
	N = 5;
	tc = findCase( 'right M=2 N=5' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.15 * i * j );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=6', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.3, 0.3 );
	v = new Complex128Array([
		1.0, 0.0, 0.2, -0.1, -0.3, 0.3, 0.4, -0.2, -0.1, 0.4, 0.5, 0.1
	]);
	vals = [];
	M = 2;
	N = 6;
	tc = findCase( 'right M=2 N=6' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( -0.1 * j );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=7', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.1, -0.5 );
	v = new Complex128Array([
		1.0,
		0.0,
		0.2,
		0.1,
		-0.3,
		-0.2,
		0.4,
		0.3,
		-0.1,
		-0.1,
		0.5,
		0.2,
		-0.2,
		0.4
	]);
	vals = [];
	M = 2;
	N = 7;
	tc = findCase( 'right M=2 N=7' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.2 * i );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=8', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.6, 0.4 );
	v = new Complex128Array([
		1.0,
		0.0,
		0.1,
		-0.2,
		-0.2,
		0.1,
		0.3,
		0.3,
		-0.4,
		-0.1,
		0.15,
		0.25,
		-0.25,
		-0.15,
		0.35,
		0.05
	]);
	vals = [];
	M = 2;
	N = 8;
	tc = findCase( 'right M=2 N=8' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( -0.15 * (i + j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=9', function t() {
	var WORK;
	var vals;
	var tau;
	var tc;
	var v;
	var M;
	var N;
	var i;
	var j;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.7, -0.1 );
	v = new Complex128Array([
		1.0,
		0.0,
		0.1,
		0.2,
		-0.2,
		-0.1,
		0.3,
		0.1,
		-0.4,
		0.2,
		0.15,
		-0.3,
		-0.25,
		0.15,
		0.35,
		-0.05,
		-0.05,
		0.35
	]);
	vals = [];
	M = 2;
	N = 9;
	tc = findCase( 'right M=2 N=9' );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.1 * (i + j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-13, 'C' );
} );

test( 'zlarfx: right M=2 N=10', function t() {
	var vvals;
	var WORK;
	var vals;
	var tau;
	var tc;
	var M;
	var N;
	var i;
	var j;
	var v;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 1.8, 0.2 );
	vvals = [];
	vals = [];
	M = 2;
	N = 10;
	tc = findCase( 'right M=2 N=10' );
	for ( i = 1; i <= 10; i += 1 ) {
		vvals.push( 0.1 * i );
		vvals.push( 0.05 * i );
	}
	vvals[ 0 ] = 1.0;
	vvals[ 1 ] = 0.0;
	v = new Complex128Array( vvals );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + (0.5 * j) );
			vals.push( -0.1 * i * j );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-12, 'C' );
} );

test( 'zlarfx: tau=0 does nothing', function t() {
	var WORK;
	var tau;
	var tc;
	var v;
	var C;

	WORK = new Complex128Array( 10 );
	tau = new Complex128( 0.0, 0.0 );
	v = new Complex128Array( [ 1.0, 0.0, 0.5, 0.3, -0.2, 0.1 ] );
	C = buildMatrix( 3, 2, [
		1.0,
		2.0,
		4.0,
		0.5,
		6.0,
		-3.0,
		3.0,
		-1.0,
		5.0,
		2.0,
		7.0,
		1.0
	]);
	tc = findCase( 'tau=0' );
	zlarfx( 'left', 3, 2, v, 1, 0, tau, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-14, 'C' );
} );

test( 'zlarfx: left M=12 N=3 general (calls zlarf)', function t() {
	var vvals;
	var WORK;
	var vals;
	var tau;
	var tc;
	var M;
	var N;
	var i;
	var j;
	var v;
	var C;

	WORK = new Complex128Array( 20 );
	tau = new Complex128( 1.4, -0.3 );
	vvals = [];
	vals = [];
	M = 12;
	N = 3;
	tc = findCase( 'left M=12 N=3 general' );
	for ( i = 1; i <= M; i += 1 ) {
		vvals.push( 0.1 * i );
		vvals.push( -0.05 * i );
	}
	vvals[ 0 ] = 1.0;
	vvals[ 1 ] = 0.0;
	v = new Complex128Array( vvals );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( 0.2 * (i - j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'left', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-12, 'C' );
} );

test( 'zlarfx: right M=3 N=12 general (calls zlarf)', function t() {
	var vvals;
	var WORK;
	var vals;
	var tau;
	var tc;
	var M;
	var N;
	var i;
	var j;
	var v;
	var C;

	WORK = new Complex128Array( 20 );
	tau = new Complex128( 1.4, 0.3 );
	vvals = [];
	vals = [];
	M = 3;
	N = 12;
	tc = findCase( 'right M=3 N=12 general' );
	for ( i = 1; i <= N; i += 1 ) {
		vvals.push( 0.1 * i );
		vvals.push( 0.05 * i );
	}
	vvals[ 0 ] = 1.0;
	vvals[ 1 ] = 0.0;
	v = new Complex128Array( vvals );
	for ( j = 1; j <= N; j += 1 ) {
		for ( i = 1; i <= M; i += 1 ) {
			vals.push( i + ((j-1) * M) );
			vals.push( -0.1 * (i + j) );
		}
	}
	C = buildMatrix( M, N, vals );
	zlarfx( 'right', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );
	assertArrayClose( getView( C ), tc.C, 1e-12, 'C' );
} );
