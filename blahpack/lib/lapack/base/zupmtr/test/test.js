/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zupmtr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zupmtr.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Converts a Float64Array to a plain Array.
*
* @private
* @param {Float64Array} arr - input array
* @returns {Array} plain array
*/
function toArray( arr ) {
	var out;
	var i;
	out = [];
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Setup data: AP and TAU from ZHPTRD('U') on a 4x4 Hermitian packed matrix.
*
* @private
* @returns {Object} result with AP and TAU arrays
*/
function setupUpper() {
	var setup = findCase( 'setup_upper' );
	return {
		'AP': new Complex128Array( setup.AP ),
		'TAU': new Complex128Array( setup.TAU )
	};
}

/**
* Setup data: AP and TAU from ZHPTRD('L') on a 4x4 Hermitian packed matrix.
*
* @private
* @returns {Object} result with AP and TAU arrays
*/
function setupLower() {
	var setup = findCase( 'setup_lower' );
	return {
		'AP': new Complex128Array( setup.AP ),
		'TAU': new Complex128Array( setup.TAU )
	};
}

/**
* Returns a 4x4 complex identity matrix in column-major storage.
*
* @private
* @returns {Complex128Array} identity matrix
*/
function identity4() {
	var cv;
	var C;
	C = new Complex128Array( 16 );
	cv = reinterpret( C, 0 );
	cv[ 0 ] = 1.0;
	cv[ 10 ] = 1.0;
	cv[ 20 ] = 1.0;
	cv[ 30 ] = 1.0;
	return C;
}

/**
* Extracts column-major complex data into a plain array.
*
* @private
* @param {Complex128Array} C - column-major matrix data
* @param {number} M - number of rows
* @param {number} N - number of columns
* @returns {Array} flattened interleaved re/im array
*/
function flattenColMajor( C, M, N ) {
	var out;
	var idx;
	var cv;
	var i;
	var j;
	cv = reinterpret( C, 0 );
	out = [];
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			idx = ( i + ( j * M ) ) * 2;
			out.push( cv[ idx ] );
			out.push( cv[ idx + 1 ] );
		}
	}
	return out;
}


// TESTS //

test( 'zupmtr: left_notrans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_notrans_upper' );
	r = setupUpper();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'upper', 'no-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: left_conjtrans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_conjtrans_upper' );
	r = setupUpper();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'upper', 'conjugate-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: right_notrans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_notrans_upper' );
	r = setupUpper();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'right', 'upper', 'no-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: right_conjtrans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_conjtrans_upper' );
	r = setupUpper();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'right', 'upper', 'conjugate-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: left_notrans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_notrans_lower' );
	r = setupLower();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'lower', 'no-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: left_conjtrans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_conjtrans_lower' );
	r = setupLower();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'lower', 'conjugate-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: right_notrans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_notrans_lower' );
	r = setupLower();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'right', 'lower', 'no-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: right_conjtrans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_conjtrans_lower' );
	r = setupLower();
	C = identity4();
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'right', 'lower', 'conjugate-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: m_zero', function t() {
	var WORK;
	var info;

	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'upper', 'no-transpose', 0, 4, new Complex128Array( 10 ), 1, 0, new Complex128Array( 3 ), 1, 0, new Complex128Array( 4 ), 1, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'zupmtr: n_zero', function t() {
	var WORK;
	var info;

	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'upper', 'no-transpose', 4, 0, new Complex128Array( 10 ), 1, 0, new Complex128Array( 3 ), 1, 0, new Complex128Array( 16 ), 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'zupmtr: left_notrans_upper_rect', function t() {
	var WORK;
	var info;
	var tc;
	var cv;
	var r;
	var C;

	tc = findCase( 'left_notrans_upper_rect' );
	r = setupUpper();
	C = new Complex128Array( 8 );
	cv = reinterpret( C, 0 );
	cv[ 0 ] = 1.0;
	cv[ 1 ] = 0.5;
	cv[ 2 ] = 2.0;
	cv[ 3 ] = -0.5;
	cv[ 4 ] = 3.0;
	cv[ 5 ] = 1.0;
	cv[ 6 ] = 4.0;
	cv[ 7 ] = 0.0;
	cv[ 8 ] = 5.0;
	cv[ 9 ] = -1.0;
	cv[ 10 ] = 6.0;
	cv[ 11 ] = 0.5;
	cv[ 12 ] = 7.0;
	cv[ 13 ] = -0.5;
	cv[ 14 ] = 8.0;
	cv[ 15 ] = 1.0;
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'left', 'upper', 'no-transpose', 4, 2, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 2 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: right_notrans_lower_rect', function t() {
	var WORK;
	var info;
	var tc;
	var cv;
	var r;
	var C;

	tc = findCase( 'right_notrans_lower_rect' );
	r = setupLower();
	C = new Complex128Array( 8 );
	cv = reinterpret( C, 0 );
	cv[ 0 ] = 1.0;
	cv[ 1 ] = 0.0;
	cv[ 2 ] = 2.0;
	cv[ 3 ] = 1.0;
	cv[ 4 ] = 3.0;
	cv[ 5 ] = -1.0;
	cv[ 6 ] = 4.0;
	cv[ 7 ] = 0.0;
	cv[ 8 ] = 5.0;
	cv[ 9 ] = 0.5;
	cv[ 10 ] = 6.0;
	cv[ 11 ] = -0.5;
	cv[ 12 ] = 7.0;
	cv[ 13 ] = 0.0;
	cv[ 14 ] = 8.0;
	cv[ 15 ] = 1.0;
	WORK = new Complex128Array( 4 );
	info = zupmtr( 'right', 'lower', 'no-transpose', 2, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 2, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 2, 4 ), tc.C, 1e-14, 'C' );
});

test( 'zupmtr: AP is restored after call', function t() {
	var APcopy;
	var WORK;
	var r;
	var C;

	r = setupUpper();
	APcopy = new Complex128Array( r.AP );
	C = identity4();
	WORK = new Complex128Array( 4 );
	zupmtr( 'left', 'upper', 'no-transpose', 4, 4, r.AP, 1, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( r.AP, 0 ) ), toArray( reinterpret( APcopy, 0 ) ), 1e-15, 'AP restored' ); // eslint-disable-line max-len
});

test( 'zupmtr: main export is a function', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main, 'function' );
});

test( 'zupmtr: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	var main = require( './../lib' );
	assert.strictEqual( typeof main.ndarray, 'function' );
});

test( 'zupmtr: ndarray validates side argument', function t() {
	var ndarray = require( './../lib/ndarray.js' );
	assert.throws( function badSide() {
		ndarray( 'bad', 'upper', 'no-transpose', 4, 4, new Complex128Array( 10 ), 1, 0, new Complex128Array( 3 ), 1, 0, new Complex128Array( 16 ), 1, 4, 0, new Complex128Array( 4 ), 1, 0 ); // eslint-disable-line max-len
	}, /TypeError/ );
});

test( 'zupmtr: ndarray validates uplo argument', function t() {
	var ndarray = require( './../lib/ndarray.js' );
	assert.throws( function badUplo() {
		ndarray( 'left', 'bad', 'no-transpose', 4, 4, new Complex128Array( 10 ), 1, 0, new Complex128Array( 3 ), 1, 0, new Complex128Array( 16 ), 1, 4, 0, new Complex128Array( 4 ), 1, 0 ); // eslint-disable-line max-len
	}, /TypeError/ );
});

test( 'zupmtr: ndarray validates trans argument', function t() {
	var ndarray = require( './../lib/ndarray.js' );
	assert.throws( function badTrans() {
		ndarray( 'left', 'upper', 'bad', 4, 4, new Complex128Array( 10 ), 1, 0, new Complex128Array( 3 ), 1, 0, new Complex128Array( 16 ), 1, 4, 0, new Complex128Array( 4 ), 1, 0 ); // eslint-disable-line max-len
	}, /TypeError/ );
});
