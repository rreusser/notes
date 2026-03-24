
'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytrd = require( '../../dsytrd/lib/base.js' );
var dormtr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dormtr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	var relErr;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}

/**
* Setup: symmetric 4x4 matrix
*   [ 4  1 -2  2 ]
*   [ 1  2  0  1 ]
*   [-2  0  3 -2 ]
*   [ 2  1 -2 -1 ]
*/
function symMatrix4() {
	// Column-major: col0=[4,1,-2,2], col1=[1,2,0,1], col2=[-2,0,3,-2], col3=[2,1,-2,-1]
	return new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);
}

function identity4() {
	return new Float64Array([
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	]);
}

function reduceUpper() {
	var A = symMatrix4();
	var D = new Float64Array( 4 );
	var E = new Float64Array( 4 );
	var TAU = new Float64Array( 4 );
	dsytrd( 'upper', 4, A, 1, 4, 0, D, 1, 0, E, 1, 0, TAU, 1, 0 );
	return { A: A, TAU: TAU };
}

function reduceLower() {
	var A = symMatrix4();
	var D = new Float64Array( 4 );
	var E = new Float64Array( 4 );
	var TAU = new Float64Array( 4 );
	dsytrd( 'lower', 4, A, 1, 4, 0, D, 1, 0, E, 1, 0, TAU, 1, 0 );
	return { A: A, TAU: TAU };
}

function flattenColMajor( C, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( C[ i + j * M ] );
		}
	}
	return out;
}


// TESTS //

test( 'dormtr: left_notrans_upper', function t() {
	var tc = findCase( 'left_notrans_upper' );
	var r = reduceUpper();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'upper', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: left_trans_upper', function t() {
	var tc = findCase( 'left_trans_upper' );
	var r = reduceUpper();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'upper', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_notrans_upper', function t() {
	var tc = findCase( 'right_notrans_upper' );
	var r = reduceUpper();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'right', 'upper', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_trans_upper', function t() {
	var tc = findCase( 'right_trans_upper' );
	var r = reduceUpper();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'right', 'upper', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: left_notrans_lower', function t() {
	var tc = findCase( 'left_notrans_lower' );
	var r = reduceLower();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'lower', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: left_trans_lower', function t() {
	var tc = findCase( 'left_trans_lower' );
	var r = reduceLower();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'lower', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_notrans_lower', function t() {
	var tc = findCase( 'right_notrans_lower' );
	var r = reduceLower();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'right', 'lower', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_trans_lower', function t() {
	var tc = findCase( 'right_trans_lower' );
	var r = reduceLower();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'right', 'lower', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: m_zero', function t() {
	var r = reduceUpper();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'upper', 'no-transpose', 0, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, 0 );
});

test( 'dormtr: n_zero', function t() {
	var r = reduceUpper();
	var C = identity4();
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'upper', 'no-transpose', 4, 0, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, 0 );
});

test( 'dormtr: nq_one_left', function t() {
	var r = reduceUpper();
	var C = new Float64Array([ 7, 3, -1, 2 ]);
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'upper', 'no-transpose', 1, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 256 );
	assert.equal( info, 0 );
});

test( 'dormtr: left_notrans_upper_rect', function t() {
	var tc = findCase( 'left_notrans_upper_rect' );
	var r = reduceUpper();
	// C is 4x2 column-major: col0=[1,2,3,4], col1=[5,6,7,8]
	var C = new Float64Array([
		1, 2, 3, 4,
		5, 6, 7, 8
	]);
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'left', 'upper', 'no-transpose', 4, 2, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 2 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_notrans_lower_rect', function t() {
	var tc = findCase( 'right_notrans_lower_rect' );
	var r = reduceLower();
	// C is 2x4 column-major (LDC=4 in Fortran but we store with LDC=2 in 0-based):
	// col0=[1,2], col1=[3,4], col2=[5,6], col3=[7,8]
	var C = new Float64Array([
		1, 2,
		3, 4,
		5, 6,
		7, 8
	]);
	var WORK = new Float64Array( 256 );
	var info = dormtr( 'right', 'lower', 'no-transpose', 2, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 2, 0, WORK, 1, 0, 256 );
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 2, 4 ), tc.C, 1e-14, 'C' );
});
