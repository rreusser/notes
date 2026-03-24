'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dstebz = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dstebz.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

var SAFEMN = 2.2250738585072014e-308;
var ABSTOL = 2.0 * SAFEMN;
var NMAX = 20;

function callDstebz( range, order, N, vl, vu, il, iu, d, e ) {
	var WORK = new Float64Array( 4 * N + 10 );
	var IWORK = new Int32Array( 3 * N + 10 );
	var w = new Float64Array( N + 1 );
	var IBLOCK = new Int32Array( N + 1 );
	var ISPLIT = new Int32Array( N + 1 );
	var M = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );

	var info = dstebz( range, order, N, vl, vu, il, iu, ABSTOL,
		d, 1, 0, e, 1, 0, M, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );

	return {
		info: info,
		m: M[ 0 ],
		nsplit: nsplitArr[ 0 ],
		w: Array.from( w.subarray( 0, M[ 0 ] ) ),
		iblock: Array.from( IBLOCK.subarray( 0, M[ 0 ] ) ),
		isplit: Array.from( ISPLIT.subarray( 0, nsplitArr[ 0 ] ) )
	};
}


// TESTS //

test( 'dstebz: N=0 quick return', function t() {
	var tc = findCase( 'n0' );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var result = callDstebz( 'all', 'block', 0, 0.0, 0.0, 1, 0, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
});

test( 'dstebz: N=1, RANGE=A', function t() {
	var tc = findCase( 'n1_all' );
	var d = new Float64Array( [ 5.0 ] );
	var e = new Float64Array( 1 );
	var result = callDstebz( 'all', 'block', 1, 0.0, 0.0, 1, 1, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	assert.deepEqual( result.w, tc.w, 'w' );
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
	assert.deepEqual( result.isplit, tc.isplit, 'isplit' );
});

test( 'dstebz: N=1, RANGE=V, eigenvalue outside interval', function t() {
	var tc = findCase( 'n1_v_outside' );
	var d = new Float64Array( [ 5.0 ] );
	var e = new Float64Array( 1 );
	var result = callDstebz( 'value', 'block', 1, 0.0, 3.0, 1, 1, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
});

test( 'dstebz: 5x5, RANGE=A, ORDER=B', function t() {
	var tc = findCase( 'n5_all_orderB' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'all', 'block', 5, 0.0, 0.0, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
	assert.deepEqual( result.isplit, tc.isplit, 'isplit' );
});

test( 'dstebz: 5x5, RANGE=A, ORDER=E', function t() {
	var tc = findCase( 'n5_all_orderE' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'all', 'entire', 5, 0.0, 0.0, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: 5x5, RANGE=V (0, 3]', function t() {
	var tc = findCase( 'n5_rangeV' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'value', 'entire', 5, 0.0, 3.0, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: 5x5, RANGE=I (eigenvalues 2-4)', function t() {
	var tc = findCase( 'n5_rangeI' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'index', 'entire', 5, 0.0, 0.0, 2, 4, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: RANGE=I with IL=1,IU=N simplifies to A', function t() {
	var tc = findCase( 'n5_rangeI_all' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'index', 'entire', 5, 0.0, 0.0, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: split matrix (E(2)=0), ORDER=B', function t() {
	var tc = findCase( 'n5_split_orderB' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 0.0, 1.0, 1.0 ] );
	var result = callDstebz( 'all', 'block', 5, 0.0, 0.0, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
	assert.deepEqual( result.isplit, tc.isplit, 'isplit' );
});

test( 'dstebz: split matrix, ORDER=E', function t() {
	var tc = findCase( 'n5_split_orderE' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 0.0, 1.0, 1.0 ] );
	var result = callDstebz( 'all', 'entire', 5, 0.0, 0.0, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
	assert.deepEqual( result.isplit, tc.isplit, 'isplit' );
});

test( 'dstebz: diagonal matrix (all E=0)', function t() {
	var tc = findCase( 'diagonal' );
	var d = new Float64Array( [ 5.0, 1.0, 3.0, 2.0 ] );
	var e = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	var result = callDstebz( 'all', 'entire', 4, 0.0, 0.0, 1, 4, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
	assert.deepEqual( result.isplit, tc.isplit, 'isplit' );
});

test( 'dstebz: split matrix, RANGE=V', function t() {
	var tc = findCase( 'split_rangeV' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 0.0, 0.0, 1.0, 1.0 ] );
	var result = callDstebz( 'value', 'block', 5, -2.0, 3.5, 1, 5, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
	assert.deepEqual( result.isplit, tc.isplit, 'isplit' );
});

test( 'dstebz: split matrix, RANGE=I', function t() {
	var tc = findCase( 'split_rangeI' );
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 0.0, 0.0, 1.0, 1.0 ] );
	var result = callDstebz( 'index', 'entire', 5, 0.0, 0.0, 2, 4, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
	assert.deepEqual( result.iblock, tc.iblock, 'iblock' );
});

test( 'dstebz: Wilkinson 10x10, RANGE=A', function t() {
	var tc = findCase( 'wilkinson10' );
	var d = new Float64Array( [ 4, 3, 2, 1, 0, 1, 2, 3, 4, 5 ] );
	var e = new Float64Array( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] );
	var result = callDstebz( 'all', 'entire', 10, 0.0, 0.0, 1, 10, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	assert.equal( result.nsplit, tc.nsplit, 'nsplit' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: Wilkinson 10x10, RANGE=I (eigenvalues 3-7)', function t() {
	var tc = findCase( 'wilkinson10_rangeI' );
	var d = new Float64Array( [ 4, 3, 2, 1, 0, 1, 2, 3, 4, 5 ] );
	var e = new Float64Array( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] );
	var result = callDstebz( 'index', 'entire', 10, 0.0, 0.0, 3, 7, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: Wilkinson 10x10, RANGE=V (1, 4]', function t() {
	var tc = findCase( 'wilkinson10_rangeV' );
	var d = new Float64Array( [ 4, 3, 2, 1, 0, 1, 2, 3, 4, 5 ] );
	var e = new Float64Array( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] );
	var result = callDstebz( 'value', 'entire', 10, 1.0, 4.0, 1, 10, d, e );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.m, tc.m, 'm' );
	for ( var i = 0; i < tc.m; i++ ) {
		assertClose( result.w[ i ], tc.w[ i ], 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: N < 0 returns error', function t() {
	var d = new Float64Array( [ 1.0 ] );
	var e = new Float64Array( 1 );
	var WORK = new Float64Array( 20 );
	var IWORK = new Int32Array( 20 );
	var w = new Float64Array( 5 );
	var IBLOCK = new Int32Array( 5 );
	var ISPLIT = new Int32Array( 5 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'all', 'block', -1, 0, 0, 1, 1, ABSTOL,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, -3, 'info' );
});

test( 'dstebz: RANGE=V with vl >= vu returns error', function t() {
	var d = new Float64Array( [ 1.0, 2.0 ] );
	var e = new Float64Array( [ 0.5 ] );
	var WORK = new Float64Array( 20 );
	var IWORK = new Int32Array( 20 );
	var w = new Float64Array( 5 );
	var IBLOCK = new Int32Array( 5 );
	var ISPLIT = new Int32Array( 5 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'value', 'block', 2, 3.0, 1.0, 1, 2, ABSTOL,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, -5, 'info' );
});

test( 'dstebz: RANGE=I with invalid IL returns error', function t() {
	var d = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var WORK = new Float64Array( 40 );
	var IWORK = new Int32Array( 30 );
	var w = new Float64Array( 5 );
	var IBLOCK = new Int32Array( 5 );
	var ISPLIT = new Int32Array( 5 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	// IL = 0 is invalid (must be >= 1)
	var info = dstebz( 'index', 'block', 3, 0, 0, 0, 3, ABSTOL,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, -6, 'info' );
});

test( 'dstebz: RANGE=I with invalid IU returns error', function t() {
	var d = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var WORK = new Float64Array( 40 );
	var IWORK = new Int32Array( 30 );
	var w = new Float64Array( 5 );
	var IBLOCK = new Int32Array( 5 );
	var ISPLIT = new Int32Array( 5 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	// IU = 5 > N = 3
	var info = dstebz( 'index', 'block', 3, 0, 0, 1, 5, ABSTOL,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, -7, 'info' );
});

test( 'dstebz: positive abstol', function t() {
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'all', 'block', 5, 0.0, 0.0, 1, 5, d, e );
	// Call again with positive abstol
	var d2 = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var WORK = new Float64Array( 30 );
	var IWORK = new Int32Array( 25 );
	var w = new Float64Array( 6 );
	var IBLOCK = new Int32Array( 6 );
	var ISPLIT = new Int32Array( 6 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'all', 'block', 5, 0.0, 0.0, 1, 5, 1e-10,
		d2, 1, 0, e2, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( MM[ 0 ], 5, 'm' );
});

test( 'dstebz: RANGE=V with interval excluding all eigenvalues', function t() {
	// All eigenvalues of this matrix are near 2 +/- 1, so interval [10,20] is empty
	var d = new Float64Array( [ 2.0, 2.0, 2.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );
	var result = callDstebz( 'value', 'block', 3, 10.0, 20.0, 1, 3, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 0, 'no eigenvalues in [10,20]' );
});

test( 'dstebz: RANGE=I with split matrix needing discard path', function t() {
	// Create a split matrix where the RANGE=I discard logic is exercised.
	// E(2) = 0 splits into two blocks: [d1,d2] and [d3,d4,d5]
	// Then request IL=2, IU=3 to exercise the discard path.
	var d = new Float64Array( [ 1.0, 5.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, 0.0, 0.5, 0.5 ] );
	var result = callDstebz( 'index', 'entire', 5, 0.0, 0.0, 2, 3, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 2, 'should find 2 eigenvalues' );
	// Eigenvalues should be sorted
	assert.ok( result.w[ 0 ] <= result.w[ 1 ], 'eigenvalues sorted' );
});

test( 'dstebz: RANGE=I requesting single eigenvalue', function t() {
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var result = callDstebz( 'index', 'entire', 5, 0.0, 0.0, 3, 3, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 1, 'should find exactly 1 eigenvalue' );
});

test( 'dstebz: split matrix with RANGE=V and positive abstol', function t() {
	var d = new Float64Array( [ 1.0, 5.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, 0.0, 0.5, 0.5 ] );
	var WORK = new Float64Array( 30 );
	var IWORK = new Int32Array( 25 );
	var w = new Float64Array( 6 );
	var IBLOCK = new Int32Array( 6 );
	var ISPLIT = new Int32Array( 6 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'value', 'block', 5, 0.0, 3.5, 1, 5, 1e-8,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.ok( MM[ 0 ] >= 1, 'found some eigenvalues' );
	// All found eigenvalues should be in (0, 3.5]
	for ( var i = 0; i < MM[ 0 ]; i++ ) {
		assert.ok( w[ i ] >= 0.0 && w[ i ] <= 3.5 + 1e-6, 'eigenvalue in range: ' + w[ i ] );
	}
});

test( 'dstebz: RANGE=I with split matrix and ORDER=B', function t() {
	// Split matrix with RANGE=I and ORDER=B to exercise the block-ordered discard path
	var d = new Float64Array( [ 10.0, 1.0, 2.0, 3.0, 20.0 ] );
	var e = new Float64Array( [ 0.0, 0.5, 0.5, 0.0 ] );
	// Three blocks: [10], [1,2,3], [20]
	// Eigenvalues approx: 10, 0.59, 2, 3.41, 20 -> sorted: 0.59, 2, 3.41, 10, 20
	// Request eigenvalues 2-4 (sorted)
	var result = callDstebz( 'index', 'block', 5, 0.0, 0.0, 2, 4, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 3, 'should find 3 eigenvalues' );
});

test( 'dstebz: degenerate eigenvalues (identical diagonal entries)', function t() {
	// All diagonal entries the same -> all eigenvalues are the same (for diagonal matrix)
	var d = new Float64Array( [ 3.0, 3.0, 3.0, 3.0 ] );
	var e = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	var result = callDstebz( 'all', 'entire', 4, 0.0, 0.0, 1, 4, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 4, 'm' );
	for ( var i = 0; i < 4; i++ ) {
		assertClose( result.w[ i ], 3.0, 1e-14, 'w[' + i + ']' );
	}
});

test( 'dstebz: large split matrix (15x15) with multiple blocks', function t() {
	// 15x15 with 3 splits -> 4 blocks
	var N = 15;
	var d = new Float64Array( N );
	var e = new Float64Array( N - 1 );
	var i;
	for ( i = 0; i < N; i++ ) {
		d[ i ] = ( i + 1 ) * 0.5;
	}
	for ( i = 0; i < N - 1; i++ ) {
		e[ i ] = 0.3;
	}
	// Create splits at positions 4, 8, 12
	e[ 3 ] = 0.0;
	e[ 7 ] = 0.0;
	e[ 11 ] = 0.0;
	var result = callDstebz( 'all', 'entire', N, 0.0, 0.0, 1, N, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, N, 'should find all eigenvalues' );
	assert.equal( result.nsplit, 4, 'should have 4 blocks' );
	// Eigenvalues should be sorted (ORDER=E)
	for ( i = 0; i < N - 1; i++ ) {
		assert.ok( result.w[ i ] <= result.w[ i + 1 ] + 1e-10, 'sorted: w[' + i + ']=' + result.w[i] + ' <= w[' + (i+1) + ']=' + result.w[i+1] );
	}
});

test( 'dstebz: RANGE=I on large split matrix (subset from middle)', function t() {
	var N = 15;
	var d = new Float64Array( N );
	var e = new Float64Array( N - 1 );
	var i;
	for ( i = 0; i < N; i++ ) {
		d[ i ] = ( i + 1 ) * 0.5;
	}
	for ( i = 0; i < N - 1; i++ ) {
		e[ i ] = 0.3;
	}
	e[ 3 ] = 0.0;
	e[ 7 ] = 0.0;
	e[ 11 ] = 0.0;
	var result = callDstebz( 'index', 'entire', N, 0.0, 0.0, 5, 10, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 6, 'should find 6 eigenvalues (indices 5-10)' );
	for ( i = 0; i < result.m - 1; i++ ) {
		assert.ok( result.w[ i ] <= result.w[ i + 1 ] + 1e-10, 'sorted' );
	}
});

test( 'dstebz: RANGE=I with split and positive abstol', function t() {
	// Exercise the abstol > 0 path inside the RANGE=I block (line 266)
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 0.0, 1.0, 1.0 ] );
	var WORK = new Float64Array( 30 );
	var IWORK = new Int32Array( 25 );
	var w = new Float64Array( 6 );
	var IBLOCK = new Int32Array( 6 );
	var ISPLIT = new Int32Array( 6 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'index', 'entire', 5, 0.0, 0.0, 2, 4, 1e-10,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( MM[ 0 ], 3, 'should find 3 eigenvalues' );
});

test( 'dstebz: RANGE=I with abstol=0 (uses ulp-based tolerance)', function t() {
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var N = 5;
	var WORK = new Float64Array( 4 * N + 10 );
	var IWORK = new Int32Array( 3 * N + 10 );
	var w = new Float64Array( N + 1 );
	var IBLOCK = new Int32Array( N + 1 );
	var ISPLIT = new Int32Array( N + 1 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	// Use abstol = 0 to trigger the ulp*tnorm path (line 266)
	var info = dstebz( 'index', 'entire', N, 0.0, 0.0, 2, 4, 0.0,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( MM[ 0 ], 3, 'should find 3 eigenvalues' );
});

test( 'dstebz: RANGE=A with abstol=0 (split matrix, exercises atoli=ulp*max)', function t() {
	// Also need abstol=0 for RANGE=A path inside the block loop (line 402)
	var d = new Float64Array( [ 2.0, -1.0, 3.0, 0.5, 4.0 ] );
	var e = new Float64Array( [ 1.0, 0.0, 1.0, 1.0 ] );
	var N = 5;
	var WORK = new Float64Array( 4 * N + 10 );
	var IWORK = new Int32Array( 3 * N + 10 );
	var w = new Float64Array( N + 1 );
	var IBLOCK = new Int32Array( N + 1 );
	var ISPLIT = new Int32Array( N + 1 );
	var MM = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'all', 'entire', N, 0.0, 0.0, 1, N, 0.0,
		d, 1, 0, e, 1, 0, MM, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( MM[ 0 ], 5, 'should find all 5 eigenvalues' );
});

test( 'dstebz: RANGE=I with clustered eigenvalues', function t() {
	// Matrix with clustered eigenvalues to exercise the discard paths.
	// Wilkinson-like matrix with near-degenerate eigenvalues.
	var N = 10;
	var d = new Float64Array( N );
	var e = new Float64Array( N - 1 );
	var i;
	// Uniform diagonal with small perturbations creates clusters
	for ( i = 0; i < N; i++ ) {
		d[ i ] = 1.0 + 0.001 * i;
	}
	for ( i = 0; i < N - 1; i++ ) {
		e[ i ] = 0.0001;
	}
	// Request middle eigenvalues
	var result = callDstebz( 'index', 'entire', N, 0.0, 0.0, 4, 7, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 4, 'should find 4 eigenvalues' );
	for ( i = 0; i < result.m - 1; i++ ) {
		assert.ok( result.w[ i ] <= result.w[ i + 1 ] + 1e-10, 'sorted' );
	}
});

test( 'dstebz: RANGE=V with very narrow interval', function t() {
	// Narrow interval that contains exactly one eigenvalue
	var d = new Float64Array( [ 1.0, 3.0, 5.0 ] );
	var e = new Float64Array( [ 0.1, 0.1 ] );
	// Eigenvalues are approximately 0.99, 3.0, 5.01
	var result = callDstebz( 'value', 'entire', 3, 2.5, 3.5, 1, 3, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 1, 'should find 1 eigenvalue' );
	assertClose( result.w[ 0 ], 3.0, 1e-2, 'eigenvalue near 3.0' );
});

test( 'dstebz: RANGE=I with N=2', function t() {
	var d = new Float64Array( [ 1.0, 3.0 ] );
	var e = new Float64Array( [ 0.5 ] );
	var result = callDstebz( 'index', 'entire', 2, 0.0, 0.0, 1, 1, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 1, 'should find 1 eigenvalue' );
});

test( 'dstebz: RANGE=V on split matrix where one block is entirely excluded', function t() {
	// Block 1: eigenvalues near 1, Block 2: eigenvalues near 100
	// Request interval [50, 150] which excludes block 1 (gu < wl path)
	var d = new Float64Array( [ 1.0, 1.0, 100.0, 100.0 ] );
	var e = new Float64Array( [ 0.1, 0.0, 0.1 ] );
	var result = callDstebz( 'value', 'block', 4, 50.0, 150.0, 1, 4, d, e );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.m, 2, 'should find 2 eigenvalues from block 2' );
});

test( 'dstebz: invalid RANGE returns error', function t() {
	var d = new Float64Array( [ 1.0 ] );
	var e = new Float64Array( 1 );
	var WORK = new Float64Array( 20 );
	var IWORK = new Int32Array( 20 );
	var w = new Float64Array( 5 );
	var IBLOCK = new Int32Array( 5 );
	var ISPLIT = new Int32Array( 5 );
	var M = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'X', 'block', 1, 0, 0, 1, 1, ABSTOL,
		d, 1, 0, e, 1, 0, M, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, -1, 'info' );
});

test( 'dstebz: invalid ORDER returns error', function t() {
	var d = new Float64Array( [ 1.0 ] );
	var e = new Float64Array( 1 );
	var WORK = new Float64Array( 20 );
	var IWORK = new Int32Array( 20 );
	var w = new Float64Array( 5 );
	var IBLOCK = new Int32Array( 5 );
	var ISPLIT = new Int32Array( 5 );
	var M = new Int32Array( 1 );
	var nsplitArr = new Int32Array( 1 );
	var info = dstebz( 'all', 'X', 1, 0, 0, 1, 1, ABSTOL,
		d, 1, 0, e, 1, 0, M, nsplitArr,
		w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0,
		WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, -2, 'info' );
});
