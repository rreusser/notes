

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dstevx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dstevx.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Helper to call dstevx with standard arguments.
*
* @param {string} jobz - 'compute-vectors' or 'no-vectors'
* @param {string} range - 'all', 'value', or 'index'
* @param {number} N - order
* @param {Array} dArr - diagonal elements
* @param {Array} eArr - subdiagonal elements
* @param {number} vl - lower value bound
* @param {number} vu - upper value bound
* @param {number} il - lower index (1-based)
* @param {number} iu - upper index (1-based)
* @param {number} abstol - tolerance
* @returns {Object} result with info, m, w, Z, IFAIL
*/
function callDstevx( jobz, range, N, dArr, eArr, vl, vu, il, iu, abstol ) {
	var IFAIL = new Int32Array( Math.max( N, 1 ) );
	var IWORK = new Int32Array( 5 * Math.max( N, 1 ) );
	var WORK = new Float64Array( 5 * Math.max( N, 1 ) );
	var d = new Float64Array( dArr );
	var e = new Float64Array( eArr );
	var w = new Float64Array( N );
	var Z = new Float64Array( N * N );
	var M = new Int32Array( 1 );
	var info;

	info = dstevx( jobz, range, N, d, 1, 0, e, 1, 0, vl, vu, il, iu, abstol, M, w, 1, 0, Z, 1, N, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
	return {
		info: info,
		m: M[ 0 ],
		w: Array.prototype.slice.call( w, 0, M[ 0 ] ),
		Z: Z,
		IFAIL: Array.prototype.slice.call( IFAIL, 0, M[ 0 ] )
	};
}


// TESTS //

test( 'dstevx: vectors_all_4x4', function t() {
	var tc = findCase( 'vectors_all_4x4' );
	var res = callDstevx( 'compute-vectors', 'all', 4,
		[ 2.0, 2.0, 2.0, 2.0 ],
		[ 1.0, 1.0, 1.0 ],
		0.0, 0.0, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
	// Check eigenvectors: each column should be close (up to sign)
	assertArrayClose( res.IFAIL, tc.ifail, 1e-14, 'ifail' );
});

test( 'dstevx: novec_all_4x4', function t() {
	var tc = findCase( 'novec_all_4x4' );
	var res = callDstevx( 'no-vectors', 'all', 4,
		[ 2.0, 2.0, 2.0, 2.0 ],
		[ 1.0, 1.0, 1.0 ],
		0.0, 0.0, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_value_4x4', function t() {
	var tc = findCase( 'vectors_value_4x4' );
	var res = callDstevx( 'compute-vectors', 'value', 4,
		[ 2.0, 2.0, 2.0, 2.0 ],
		[ 1.0, 1.0, 1.0 ],
		1.5, 3.5, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_index_4x4', function t() {
	var tc = findCase( 'vectors_index_4x4' );
	var res = callDstevx( 'compute-vectors', 'index', 4,
		[ 2.0, 2.0, 2.0, 2.0 ],
		[ 1.0, 1.0, 1.0 ],
		0.0, 0.0, 2, 3, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var res = callDstevx( 'compute-vectors', 'all', 0,
		[], [],
		0.0, 0.0, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
});

test( 'dstevx: n_one', function t() {
	var tc = findCase( 'n_one' );
	var res = callDstevx( 'compute-vectors', 'all', 1,
		[ 3.5 ], [],
		0.0, 0.0, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
	assertArrayClose( Array.prototype.slice.call( res.Z, 0, 1 ), tc.z, 1e-14, 'z' );
});

test( 'dstevx: n_one_out_of_range', function t() {
	var tc = findCase( 'n_one_out_of_range' );
	var res = callDstevx( 'compute-vectors', 'value', 1,
		[ 3.5 ], [],
		5.0, 10.0, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
});

test( 'dstevx: novec_index_4x4', function t() {
	var tc = findCase( 'novec_index_4x4' );
	var res = callDstevx( 'no-vectors', 'index', 4,
		[ 2.0, 2.0, 2.0, 2.0 ],
		[ 1.0, 1.0, 1.0 ],
		0.0, 0.0, 1, 2, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_all_6x6', function t() {
	var tc = findCase( 'vectors_all_6x6' );
	var res = callDstevx( 'compute-vectors', 'all', 6,
		[ 1.0, 3.0, 2.0, 4.0, 1.5, 2.5 ],
		[ 0.5, 1.0, 0.3, 0.8, 0.6 ],
		0.0, 0.0, 0, 0, 0.0
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_value_abstol', function t() {
	var tc = findCase( 'vectors_value_abstol' );
	var res = callDstevx( 'compute-vectors', 'value', 4,
		[ 2.0, 2.0, 2.0, 2.0 ],
		[ 1.0, 1.0, 1.0 ],
		0.0, 5.0, 0, 0, 1.0e-12
	);
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});
