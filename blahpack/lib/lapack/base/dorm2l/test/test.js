

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dorm2l = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorm2l.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Returns the QL-factored A (4x3 column-major, LDA=4) and TAU from dgeqlf.
*/
function getQLFactors() {
	var ql = findCase( 'ql_factor' );
	var A = new Float64Array( ql.A );
	var TAU = new Float64Array( ql.TAU );
	return { A: A, TAU: TAU };
}


// TESTS //

test( 'dorm2l: left_notrans (Q*I = Q)', function t() {
	var tc = findCase( 'left_notrans' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'left', 'no-transpose', 4, 4, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: left_trans (Q^T*I)', function t() {
	var tc = findCase( 'left_trans' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'left', 'transpose', 4, 4, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: right_notrans (I*Q)', function t() {
	var tc = findCase( 'right_notrans' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'right', 'no-transpose', 4, 4, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: right_trans (I*Q^T)', function t() {
	var tc = findCase( 'right_trans' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'right', 'transpose', 4, 4, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: m_zero', function t() {
	var ql = getQLFactors();
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dorm2l( 'left', 'no-transpose', 0, 4, 0, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dorm2l: n_zero', function t() {
	var ql = getQLFactors();
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dorm2l( 'left', 'no-transpose', 4, 0, 0, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dorm2l: k_zero', function t() {
	var ql = getQLFactors();
	var C = new Float64Array( 16 );
	var WORK = new Float64Array( 4 );
	var info = dorm2l( 'left', 'no-transpose', 4, 4, 0, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dorm2l: left_notrans_rect (Q*C, 4x2)', function t() {
	var tc = findCase( 'left_notrans_rect' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 3, -1, 2,
		2, 0, 4, -1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'left', 'no-transpose', 4, 2, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: left_trans_rect (Q^T*C, 4x2)', function t() {
	var tc = findCase( 'left_trans_rect' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 3, -1, 2,
		2, 0, 4, -1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'left', 'transpose', 4, 2, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: right_notrans_rect (C*Q, 2x4)', function t() {
	var tc = findCase( 'right_notrans_rect' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0,
		2, 1,
		-1, 3,
		4, -2
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'right', 'no-transpose', 2, 4, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 2, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: right_trans_rect (C*Q^T, 2x4)', function t() {
	var tc = findCase( 'right_trans_rect' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0,
		2, 1,
		-1, 3,
		4, -2
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'right', 'transpose', 2, 4, 3, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 2, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});

test( 'dorm2l: k_one (single reflector)', function t() {
	var tc = findCase( 'k_one' );
	var ql = getQLFactors();
	var C = new Float64Array([
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	]);
	var WORK = new Float64Array( 100 );
	var info = dorm2l( 'left', 'no-transpose', 4, 4, 1, ql.A, 1, 4, 0, ql.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
	assert.equal( info, tc.info, 'INFO' );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'C' );
});
